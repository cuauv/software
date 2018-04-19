; For storing utility procedures that are used by peacock internally but not
; strongly related to peacock itself. Procedures that use the FFI should be
; put here, so that peacock can be interpreted while they are compiled.
(module peacock-internal (process-up?
                          time-after
                          process-terminate!

                          term-wait-period

                          child-guard-start!
                          child-guard-stop!
                          child-guard-set!
                          child-guard-unset!

                          zmq-request

                          discard-input

                          (syntax: req/kp make-invalid-arg-exn)
                    )
  (import scheme chicken)
  (import foreign)

  (use srfi-18 srfi-69)
  (use posix extras)

  (use raisin)

  (define-syntax define-peacock-exn
    (ir-macro-transformer
      (lambda (x i c)
        (if (not (symbol? (cadr x)))
          (abort "Expected (define-peacock-exn name)."))
        `(define (,(i (string->symbol (format "make-~A-exn" (i (cadr x))))) message . fargs)
           (make-composite-condition
             (make-property-condition 'exn 'message (apply format message fargs))
             (make-property-condition 'peacock)
             (make-property-condition (quote ,(i (cadr x)))))))))

  (define-peacock-exn unexpected-child-exit)
  (define-peacock-exn invalid-arg)

  (define child-exited-wait-period (make-parameter 1))
  (define term-wait-period (make-parameter 0.25))

  (foreign-declare "#include <sys/types.h>")
  (foreign-declare "#include <signal.h>")
  (foreign-declare "#include <errno.h>")
  (foreign-declare "#include <sys/wait.h>")
  (foreign-declare "#include <assert.h>")

  ; http://stackoverflow.com/a/19804986
  ; Section 1.9 of http://lib.ru/UNIXFAQ/unixprogrfaq.txt 
  (define (process-up? pid)
    ((foreign-lambda* bool ((int pid))
       "int rc = kill(pid, 0);
        C_return(rc == 0 || (rc == -1 && errno == EPERM));")
     pid))

  (define (child-up? pid)
     (let ((result ((foreign-lambda* int ((int pid))
                      "do {
                        pid_t ret = waitpid(pid, NULL, WNOHANG);
                        if (ret == 0) {
                          C_return(1);
                        }
                        if (ret != -1) {
                          C_return(0);
                        }
                        if (errno == ECHILD) {
                          C_return(0);
                        }
                      } while (errno == EINTR);
                      C_return(errno);
                      "
                      ) pid)))
       (case result
         ((1) #t)
         ((0) #f)
         ((-1) (abort (format "waitpid: ~A" result))))
       )
    )

  ;(define (child-up? pid #!optional (wait #f))
  ;  (and (process-up? pid)
  ;       (receive (pid normal code) (process-wait pid (not wait))
  ;         (= pid 0))))

  (define (time-after s)
    (seconds->time (+ s (time->seconds (current-time)))))

  (define (process-terminate! pid #!optional (intense #f))
    (if (not (process-up? pid))
      #t
      (begin
        (process-signal pid (if intense signal/term signal/int))
        (thread-sleep! (time-after (term-wait-period)))
        (if (not (child-up? pid))
          #t
          (process-terminate! pid #t)))))

  ; `child-exited` returns a deferred that becomes determined when the child 
  ; with PID `pid` exits. The status of the child is checked every
  ; `(child-exited-wait-period)` seconds.
  (define (child-exited pid)
    (let* ((i (new-ivar))
           (f (lambda ()
                (let loop ()
                  (if (not (child-up? pid))
                    (ivar-fill! i #t)
                    (begin
                      (thread-sleep! (time-after (child-exited-wait-period)))
                      (loop)))))))
      (thread-start! (make-thread f))
      (ivar-read i)))

  ; `child-guard` binds the deferred returned by `(child-exited pid)` to a procedure
  ; that calls `abort` with an `(exn peacock unexpected-child-exit)` unless the exit is
  ; marked as expected. The exit is marked as expected by calling the thunk returned by
  ; `child-guard`.
  (define (child-guard pid)
    (let* ((mut (make-mutex))
           (expected #f)
           (exited (child-exited pid))
           (f (lambda (_)
                (mutex-lock! mut)
                (when (not expected)
                  (mutex-unlock! mut)
                  (abort (make-unexpected-child-exit-exn "Child with PID ~A exited unexpectedly." pid)))
                (mutex-unlock! mut)
                (return '()))))
      (bind exited f)
      (lambda ()
        (mutex-lock! mut)
        (set! expected #t)
        (mutex-unlock! mut))))

  (define child-guard-mutex (make-mutex))
  (define child-guard-ht (make-hash-table))
  (define child-guard-queued (make-hash-table))

  ; CHICKEN's process-wait errors on -1 being returned. This can happen when we
  ; call process-wait but we have no children to wait for. We *don't* want an
  ; error to be thrown in this case, so we have our own waitpid...
  (define waitpid
    (foreign-lambda* int ((int pid))
      "int res;"
      "do {"
      "  res = waitpid(pid, NULL, WNOHANG);"
      "} while (res == -1 && errno == EINTR);"
      "C_return(res);"))

  ; ... on success, returns the process ID of the child whose state has
  ; changed; if WNOHANG was specified and one or more child(ren) specified by
  ; pid exist, but have not yet changed state, then 0 is returned.  On error,
  ; -1 is returned.
  ; (In the possible errors: ECHILD, EINTR, EINVAL. Because we loop on EINTR
  ; and assume EINVAL cannot occur, the only case where the retrun value from
  ; waitpid will be -1 is on ECHILD, i.e., "The process specified by pid
  ; (waitpid()) or idtype and id (waitid()) does  not exist  or  is  not a
  ; child of the calling process."

  (define (child-guard-start!)
    (set-signal-handler! signal/chld
      (lambda (_)
        (thread-start!
          (make-thread
            (lambda ()
              (mutex-lock! child-guard-mutex)
              ; http://www.microhowto.info/howto/reap_zombie_processes_using_a_sigchld_handler.html
              (let loop ()
                (let ((pid (waitpid -1)))
                  (when (not (<= pid 0))
                    (if (hash-table-exists? child-guard-ht pid)
                        (let ((f (hash-table-ref child-guard-ht pid)))
                          (mutex-unlock! child-guard-mutex)
                          (f pid)
                          )
                        (begin
                          (hash-table-set! child-guard-queued pid #t))))
                  (loop)))
              (mutex-unlock! child-guard-mutex)))))))

  (define (default-child-guard-handler pid)
    (process-signal (current-process-id) signal/term)
    ; Signal so that others can clean up.
    (abort (make-unexpected-child-exit-exn "Child with PID ~A exited unexpectedly." pid)))

  (define (child-guard-stop!)
    (set-signal-handler! signal/chld #f))

  ; [f] is called with [pid], [normal] and [code] from [process-wait].
  (define (child-guard-set! pid #!optional (f default-child-guard-handler))
    (mutex-lock! child-guard-mutex)
    (if (hash-table-exists? child-guard-queued pid)
      (f pid))
    (hash-table-set! child-guard-ht pid f)
    (mutex-unlock! child-guard-mutex))

  (define (child-guard-unset! pid)
    (mutex-lock! child-guard-mutex)
    (if (not (hash-table-exists? child-guard-ht pid))
      (begin
        (mutex-unlock! child-guard-mutex)
        #f)
      (begin
        (hash-table-delete! child-guard-ht pid)
        (mutex-unlock! child-guard-mutex)
        #t)))

  (foreign-declare "#include <zmq.h>")

  (foreign-declare "void peacock_util_free(void* data, void* hint)
                    {
                      free(data);
                    }
                    ")

  (define (zmq-request url request)
    ((foreign-lambda* c-string* ((c-string url) (c-string request) (int len))
       "// We copy the input string because it will be garbage collected by
       // CHICKEN, but zmq wants to 'take ownership of the provided buffer'.
       char* input = malloc(len + 1);
       assert(input != NULL);
       memcpy(input, request, len);
       input[len] = '\\0';

       int rc;

       void* ctx = zmq_ctx_new();
       if (ctx == NULL) {
         C_return(NULL);
       }

       void* socket = zmq_socket(ctx, ZMQ_REQ);
       if (socket == NULL) {
         C_return(NULL);
       }

       rc = zmq_connect(socket, url);
       if (rc != 0) {
         C_return(NULL);
       }

       zmq_msg_t msg;

       rc = zmq_msg_init_data(&msg, input, len, peacock_util_free, NULL);
       if (rc != 0) {
         C_return(NULL);
       }

       rc = zmq_msg_send(&msg, socket, 0);
       if (rc == -1) {
         C_return(NULL);
       }

       // zmq deallocates the data previously stored in msg before receiving
       // and allocates a new buffer that will be freed by zmq using
       // peacock_util_free.
       rc = zmq_msg_recv(&msg, socket, 0);
       if (rc == -1) {
         C_return(NULL);
       }

       // We copy the received message data because zeromq wants to free the
       // the received message data.
       char* data = malloc(zmq_msg_size(&msg) + 1);
       assert(data != NULL);
       memcpy(data, zmq_msg_data(&msg), zmq_msg_size(&msg));
       data[zmq_msg_size(&msg)] = '\\0';

       zmq_close(socket);
       zmq_term(ctx);

       C_return(data);
       ") url request (string-length request)))

  (define (discard-input* in)
    (let ((s (make-string 1024)))
      (let loop ()
        (let ((n (read-string! 1024 s in)))
          (if (not (= n 0))
            (loop))))))

  (define (discard-input in)
    (thread-start! (make-thread (lambda () (discard-input* in)))))

  (define-syntax req/kp
    (syntax-rules ()
      ((_ name)
       (if (not name)
         (abort (make-invalid-arg-exn "Missing required keyword argument ~A." (quote name)))))
      ((_ name ...)
       (begin
         (req/kp name) ...))))
  )

; vim: set lispwords+=set-signal-handler! :
