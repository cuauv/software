(module peacock (pass fail note
                 triggered
                 vehicle
                 camera-stream-off!
                 camera-stream-on!
                 camera-stream-zero!

                 path-started
                 path-finished

                 make-camera-proc

                 changed

                 must

                 (syntax: collision collision*)
                 (syntax: vehicle-set! must vehicle)
                 (syntax: entity entity*)
                 (syntax: camera camera*)
                 (syntax: camera-stream camera-stream*)
                 (syntax: shape shape* camera-properties* camera-proc-register*)
                 (syntax: path path*)
                 (syntax: peacock peacock* mission*)
                 )
  (import scheme chicken)
  (use ports extras data-structures posix)
  (use srfi-13 srfi-18 srfi-69)
  (use matchable irregex)

  (use raisin (prefix fishbowl bowl:) cuauv-shm)
  (import (prefix cuauv-dshm dshm:))
  (use peacock-internal)

  (define-syntax dbg
    (syntax-rules ()
      ((_ . _) #f)))

  #|(define-syntax dbg
    (syntax-rules ()
      ((_ . _)
       (with-output-to-port (current-error-port)
         (lambda ()
           (print . _))))))|#

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

  (define-peacock-exn already-running)
  (define-peacock-exn run-mission-fail)
  (define-peacock-exn run-simulator-fail)
  (define-peacock-exn cuauv-software-undefined)
  (define-peacock-exn simulator-error)
  (define-peacock-exn invalid-arg)
  (define-peacock-exn notifier-error)

  (define-record context
    simulator
    mission-runner
    navigated

    notifier
    notifier-ht
    notifier-mutex

    mission
    stream-handlers
    scheduling?

    camera-properties
    camera-procs
    )

  (define (new-context)
    (make-context
      #f                ; simulator
      #f                ; mission-runner
      #f                ; navigated

      #f                ; notifier
      (make-hash-table) ; notifier-ht
      (make-mutex)      ; notifier-mutex

      #f                ; mission
      (make-hash-table) ; stream-handlers
      #f                ; scheduling?

      (make-hash-table) ; camera-properties, #(w h) hash-table
      (make-hash-table) ; camera-stream, #(on off) hash-table
      )
    )

  (define vehicle 0)

  (define (cuauv-path path)
    (let ((dir (get-environment-variable "CUAUV_SOFTWARE")))
      (if (not (and dir (string-suffix? "/" dir)))
        (abort (make-cuauv-software-undefined-exn "CUAUV_SOFTWARE must be set to the root of the software repository, with a trailing slash."))
        (string-append dir path))))

  (define simulator-port (make-parameter 7772))
  (define simulator-bin (make-parameter "auv-fishbowl"))
  (define simulator-args (make-parameter '("--use-des-thrusters")))

  (define notifier-bin (make-parameter "auv-shm-notifier"))

  (define current-context (make-parameter #f))

  ; relative to CUAUV_SOFTWARE
  (define mission-runner-bin (make-parameter "mission/runner.py"))
  (define mission-runner-args (make-parameter '()))

  (define navigated-bin (make-parameter "control/auv_navigated.py"))
  (define navigated-args (make-parameter '()))

  (define mission-module-prefix (make-parameter ""))

  (define connect-wait-period (make-parameter 0.1))
  (define connect-max-tries (make-parameter 20))

  (define regroup-bin (make-parameter (cuauv-path "peacock/regroup")))
  (define mute-bin (make-parameter (cuauv-path "peacock/mute")))

  (define (process-run/rg path #!optional (args '()))
    (process-run (regroup-bin) (cons path args)))

  (define (process/rg path #!optional (args '()))
    (process (regroup-bin) (cons path args)))

  (define (process/mrg path #!optional (args '()))
    (process (mute-bin) (append (list (regroup-bin) path) args)))

  ; assumes all the streaming things have format #(TYPE id ...)
  (define (context-on-stream ctx m)
    (dbg "context-on-stream")
    (bind (return '())
          (lambda (_)
            (let* ((type (vector-ref m 0))
                   (id (vector-ref m 1))
                   (fs (hash-table-ref/default (context-stream-handlers ctx) (cons type id) '()))
                   (fs* '()))
              (call/cc 
                (lambda (break)
                  (for-each (lambda (f)
                              (let ((ret (f m)))
                                (case ret
                                  ((remove) #t)
                                  ((cancel) (break #t))
                                  (else (set! fs* (cons f fs*))))))
                            fs)))
              (hash-table-set! (context-stream-handlers ctx) (cons type id) fs*))
            (return '())))
    )

  ; if f returns 'remove, the handler is removed
  ; if f returns 'cancel, the handler is removed and subsequent handlers are not executed
  (define (context-add-stream-handler! ctx type id f)
    (hash-table-update! (context-stream-handlers ctx) (cons type id) (cut cons f <>) (lambda () '())))

  (define notifier-rgx (irregex "(.+)=(.+)"))o

  (define (notifier-notify ctx k v)
    (let ((m (context-notifier-mutex ctx))
          (ht (context-notifier-ht ctx)))
      (mutex-lock! m)
      (when (hash-table-exists? ht k)
        (let* ((entry (hash-table-ref ht k))
               (from (vector-ref entry 0))
               (to (vector-ref entry 1)))
          (vector-set! entry 0
                       (foldl (lambda (a l)
                                (let ((v* (car l))
                                      (i (cdr l)))
                                  (if (not (eq? v v*))
                                    (begin
                                      (ivar-fill! i v)
                                      a)
                                    (cons l a))))
                              '() from))
          (when (hash-table-exists? to v)
            (ivar-fill! (hash-table-ref to v) v)
            (hash-table-delete! to v))))
      (mutex-unlock! m)
      ))

  (define (notifier-add! ctx k type v)
    (let* ((i #f)
           (m (context-notifier-mutex ctx))
           (ht (context-notifier-ht ctx))
           (notifier (context-notifier ctx))
           (not-out (vector-ref notifier 1)))
      (mutex-lock! m)
      (unless (hash-table-exists? ht k)
        (hash-table-set! ht k (vector '() (make-hash-table)))
        (write-line (format "?~A" k) not-out))
      (let ((entry (hash-table-ref ht k)))
        (case type
          ((from)
           (set! i (new-ivar))
           (vector-set! entry 0 (cons (cons v i) (vector-ref entry 0))))
          ((to)
           (let ((to (vector-ref entry 1)))
             (if (hash-table-exists? to v)
               (set! i (hash-table-ref to v))
               (begin
                 (set! i (new-ivar))
                 (hash-table-set! to v i)))))
          (else
            (abort (make-invalid-arg-exn "notifier-add: Invalid type: ~A" type)))))
      (mutex-unlock! m)
      (ivar-read i)))

  (define (notifier-thread-start! ctx in)
    (define (handle)
      (let ((line (read-line in)))
        (when (not (eof-object? line))
          (let ((c (substring/shared line 0 1))
                (b (substring/shared line 1)))
            (dbg "notifier: "line)
            (cond
              ((string=? c "!")
               (let* ((m (irregex-match notifier-rgx b))
                      (k (string->number (irregex-match-substring m 1)))
                      (v (call-with-input-string (irregex-match-substring m 2) read)))
                 (notifier-notify ctx k v)))
              ((string=? c "+") #t)
              ((string=? c "-") (abort (make-notifier-error-exn "Notifier error: ~A" b)))
              (else (abort (make-notifier-error-exn "Unexpected notifier message: ~A" line)))))
          (handle))))
    (thread-start! (make-thread (lambda () (handle)))))

  (define (changed name #!key (from '()) (to '()))
    (if (and (not (null? from)) (not (null? to)))
      (abort (make-invalid-arg-exn "changed: You must only specify zero or one of `from` and `to`.")))
    (let ((id (dshm:name->id name)))
      (if (not id)
        (abort (make-invalid-arg-exn "changed: Unrecognized name: ~A" name)))
      (cond
        ((not (null? from))
         (notifier-add! (current-context) id 'from from))
        ((not (null? to))
         (notifier-add! (current-context) id 'to to))
        (else
          (changed name from: ((dshm:name->getter name)))))))

  (define (must result)
    (match result
      (#('OK) #t)
      (#('ERROR code message) (abort (make-simulator-error-exn "~A (~A)" message code)))))

  ; use current-context
  (define-syntax vehicle-set!
    (ir-macro-transformer
      (lambda (x i c)
        (define (pair-up args*)
          (if (null? args*)
            '()
            (if (< (length args*) 2)
              (syntax-error "Expected (vehicle name1: value1 name2: value2 ...).")
              (let* ((name (car args*))
                     (value (cadr args*))
                     (rest (cddr args*))
                     (sname (symbol->string name)))
                (if (not (c name (string->keyword sname)))
                  (syntax-error "Expected (vehicle name1: value1 name2: value2 ...).")
                  (let ((tname (i (string->symbol sname))))
                    (cons (cons tname value) (pair-up rest))))))))
        (let ((args (pair-up (cdr x))))
          `(begin
             ,@(map (lambda (assign)
                     `(must (bowl:eset vehicle (quote ,(car assign)) ,(cdr assign))))
                   args))))))

  (define (must-add result)
    (match result
      (#('ADDED id) id)
      (#('ERROR code message) (abort (make-simulator-error-exn "~A (~A)" message code)))))

  (define (entity* #!key x r (corporeal '()) (m 1.0) (I #(1 1 1)) (btom-rq #(1 0 0 0)) (q #(1 0 0 0)) (xattrs '()))
    (req/kp x r)
    (if (null? corporeal)
      (abort (syntax-error "Missing required keyword argument corporeal.")))
    (must-add (bowl:eadd m: m r: r I: I btom-rq: btom-rq x: x q: q corporeal: corporeal xattrs: xattrs)))

  (define-syntax entity
    (syntax-rules ()
      ((_ name args ...)
       (define name (entity* args ...)))))

  ; focal length is in pixels
  ; approximate default using (focal length in meters) / (sensor side length in
  ; meters) * pixels side length
  ; focal length in meters = 25 mm
  ; sensor side length in meters = 10 mm
  ; pixels side length = 1000
  ; ... but actually, apparently ~500 was the value we got from OpenCV (very wide angle?)
  ; This value and min-r for make-camera-proc are important defaults wrt the camera.
  (define (camera* #!key w h (entity vehicle) (q #(1 0 0 0)) (x #(0 0 0)) (f 500))
    (req/kp w h)
    (let ((id (must-add (bowl:cadd entity: entity q: q x: x f: f enabled: #t))))
      (hash-table-set! (context-camera-properties (current-context)) id (vector w h))
      id))

  (define-syntax camera
    (syntax-rules ()
      ((_ name args ...)
       (define name (camera* args ...)))))

  (define (camera-stream* camera target proc #!key (period 10))
    (req/kp camera target period proc)
    (let ((id (must-add (bowl:cstream camera period target))))
      (context-add-stream-handler! (current-context) 'CDATA id
                                   (match-lambda
                                     (#('CDATA id x y r d)
                                      (proc x y r d))))
      id))

  (define-syntax camera-stream
    (syntax-rules (>>)
      ((_ name camera >> target args ...)
       (define name (camera-stream* camera target args ...)))))

  ; XXX currently uses collision sweep
  ; in the future, should we use the cheaper collision?
  (define (collision* a b)
    (must-add (bowl:cstadd a b)))

  (define-syntax collision
    (syntax-rules (**)
      ((_ name a ** b)
       (define name (collision* a b)))))

  (define (triggered id)
    (let ((i (new-ivar)))
      (context-add-stream-handler! (current-context) 'TRIGGERED id
                                   (lambda (m)
                                     (ivar-fill! i '())
                                     'remove))
      (ivar-read i)))

  (define (untriggered id)
    (let ((i (new-ivar)))
      (context-add-stream-handler! (current-context) 'UNTRIGGERED id
                                   (lambda (m)
                                     (ivar-fill! i '())
                                     'remove))
      (ivar-read i)))

  ; parses fake keyword parameters
  (define (parse/kps args acc)
    (match args
      ((name value . rest)
       (parse/kps rest (cons (name value) acc)))
      (()
       acc)))

  (define (req/fkp name pairs #!optional (compare eq?))
    (let* ((not-found (gensym))
           (v (alist-ref name pairs compare not-found)))
      (if (eq? v not-found)
        (syntax-error (format "Missing required keyword argument ~A." name))
        v)))

  (define (camera-properties* id)
    (hash-table-ref (context-camera-properties (current-context)) id))

  (define (make-camera-proc w h p-set! x-set! y-set! #!optional (p 0.9) (min-d 0.05) (min-r 25))
    (let* ((enabled #t)
           (zero! (lambda ()
                    (p-set! 0)
                    (x-set! 0)
                    (y-set! 0)))
           (post! (lambda (p x y)
                    (p-set! p)
                    (x-set! x)
                    (y-set! y)))
           (f (lambda (x y r d)
                ; Adjust the coordinates so they correspond to that of the camera;
                ; we receive them with the origin at the center, the camera has
                ; the origin at the top left.
                (if enabled
                  (let ((x* (inexact->exact (round (+ (/ w 2) x))))
                        (y* (inexact->exact (round (+ (/ h 2) y)))))
                    (if (or (< d min-d) (< r min-r) (< x* 0) (>= x* w) (< y* 0) (>= y* h))
                      (zero!)
                      (post! p x* y*)))
                  (zero!)))))
      (values f
              (lambda () (set! enabled #t))
              (lambda () (zero!) (set! enabled #f))
              zero!)))

  (define (shape* cam proc #!key r x f (q #(1 0 0 0)) (xattrs '()))
    (req/kp r x)
    (let* ((e (entity* x: x r: r q: q corporeal: #f xattrs: xattrs))
           (f-on #t)
           (s (camera-stream* cam e
                              (if f
                                (lambda (x y r d)
                                  (if f-on
                                    (apply proc (f x y r d))))
                                proc))))
      (values e s (lambda () (set! f-on #t)) (lambda () (set! f-on #f)))))

  (define (camera-proc-register* stream on! off! zero!)
    (hash-table-set! (context-camera-procs (current-context)) stream
                     (vector on! off! zero!)))

  (define (camera-stream-on! stream)
    (if (not (hash-table-exists? (context-camera-procs (current-context)) stream))
      (abort (make-invalid-arg-exn "No camera procs registered for given stream. Was it not defined using (shape)?")))
    (let ((on! (vector-ref (hash-table-ref (context-camera-procs (current-context)) stream) 0)))
      (on!)))

  (define (camera-stream-off! stream)
    (if (not (hash-table-exists? (context-camera-procs (current-context)) stream))
      (abort (make-invalid-arg-exn "No camera procs registered for given stream. Was it not defined using (shape)?")))
    (let ((off! (vector-ref (hash-table-ref (context-camera-procs (current-context)) stream) 1)))
      (off!)))

  (define (camera-stream-zero! stream)
    (if (not (hash-table-exists? (context-camera-procs (current-context)) stream))
      (abort (make-invalid-arg-exn "No camera procs registered for given stream. Was it not defined using (shape)?")))
    (let ((zero! (vector-ref (hash-table-ref (context-camera-procs (current-context)) stream) 2)))
      (zero!)))

  ; (_ camera >> name x: x r: r)
  ; defines entity named name
  ; stream named camera>>name
  ; two procedures,
  ; camera>>name-on!
  ; camera>>name-off!
  (define-syntax shape
    (ir-macro-transformer 
      (lambda (x i c)
        (import chicken)
        (use matchable)

        (define (format-symbol fmt . args)
          (string->symbol (apply format #f fmt args)))

        (define (usage)
          (abort (syntax-error "Expected (shape camera >> name x: _ r: _) or (shape camera >> name (p x y) x: _ r: _.")))

        (define (render camera entity args make-camera-proc-args)
          (let* ((camera-name (strip-syntax camera))
                 (name (strip-syntax entity))
                 (stream (i (format-symbol "~A>>~A" camera-name name)))
                 (enable (i (format-symbol "~A>>~A-on!" camera-name name))) 
                 (disable (i (format-symbol "~A>>~A-off!" camera-name name)))
                 (zero (i (format-symbol "~A>>~A-zero!" camera-name name)))) 
            (if (not (and (symbol? camera-name) (symbol? name)))
              (usage))
            `(begin
               (define w-h (camera-properties* ,camera))
               (define w (vector-ref w-h 0))
               (define h (vector-ref w-h 1))
               (define ,entity #f)
               (define ,stream #f)
               (receive (proc on! off! zero!) (make-camera-proc w h ,@make-camera-proc-args)
                 (receive (entity stream f-on f-off) (shape* ,camera proc ,@args)
                   (set! ,entity entity)
                   (set! ,stream stream)
                   ; Is this kind of hacky?
                   (set! ,enable (lambda ()
                                   (f-on)
                                   (on!)))
                   (set! ,disable (lambda ()
                                    (f-off)
                                    (off!)))
                   (camera-proc-register* stream ,enable ,disable zero!))))))

        (match x
          ((_ camera >> entity (camera-proc-args ...) args ...)
           (if (not (and (c >> '>>)))
             (usage))
           (render camera entity args camera-proc-args))
          ((_ camera >> entity args ...)
           (let ((name (strip-syntax entity)))
             (if (not (and (c >> '>>) (symbol? name)))
               (usage))
             (render camera entity args
                     (list (i (format-symbol "shape-~A.p-set!" name))
                           (i (format-symbol "shape-~A.x-set!" name))
                           (i (format-symbol "shape-~A.y-set!" name))))))
          (else (usage))))))
  
  (define (path* entity x0 x1 r)
    (must-add (bowl:ptadd entity x0 x1 r)))
  
  (define-syntax path
    (syntax-rules (: -> r:)
      ((_ name
          entity : x0 -> x1
          r: r)
       (define name (path* entity x0 x1 r)))))

  (define (path-started path)
    (let* ((i (new-ivar))
           (f (lambda (_)
                (ivar-fill! i #t)
                'cancel)))
      (context-add-stream-handler! (current-context) 'PSTART path f)
      (ivar-read i)))

  (define (path-finished path)
    (let* ((i (new-ivar))
           (done #f)
           (f (lambda (m)
                (if done
                  #t
                  (begin
                    (case (vector-ref m 0)
                      ((PFAIL) (ivar-fill! i 'fail))
                      ((PEND) (ivar-fill! i 'end)))
                    (set! done #t))))))
      (context-add-stream-handler! (current-context) 'PFAIL path f)
      (context-add-stream-handler! (current-context) 'PEND path f)
      (ivar-read i)))

  (define (init!)
    (if (current-context)
      (abort (make-already-running-exn "Another peacock instance is already running.")))
    (let ((ctx (new-context)))
      (stop-on-signals! ctx)
      (child-guard-start!)

      (current-context ctx)

      ; Start the simulator
      (receive (sim-in sim-out sim-pid) (process/rg (simulator-bin) (append (simulator-args) (list "-p" (number->string (simulator-port)))))
        (let loop ((tries 0))
          (if (not (process-up? sim-pid))
            (abort (make-run-simulator-fail-exn "Failed to start simulator.")))
          (condition-case
            (bowl:connect (simulator-port) on-stream: (cute context-on-stream (current-context) <>))
            ((exn i/o net)
             (if (= tries (connect-max-tries))
               (abort (make-run-simulator-fail-exn "Failed to connect to simulator.")))
             (thread-sleep! (time-after (connect-wait-period)))
             (loop (+ tries 1)))))
        (context-simulator-set! ctx (vector sim-in sim-out sim-pid))
        (child-guard-set! sim-pid)
        (discard-input sim-in))

      (receive (not-in not-out not-pid) (process/rg (notifier-bin))
        (context-notifier-set! ctx (vector not-in not-out not-pid))
        (child-guard-set! not-pid)
        (notifier-thread-start! ctx not-in))

      ctx))


  (define (stop! ctx)
    (child-guard-stop!)

    (and (context-mission-runner ctx)
         (let ((mr-pid (context-mission-runner ctx)))
           (context-mission-runner-set! ctx #f)
           (child-guard-unset! mr-pid)
           (process-terminate! mr-pid)))

    (and (context-navigated ctx)
         (let ((nv-pid (context-navigated ctx)))
           (context-navigated-set! ctx #f)
           (child-guard-unset! nv-pid)
           (process-terminate! nv-pid)))

    (and (context-simulator ctx)
         (match-let ((#(sim-in sim-out sim-pid) (context-simulator ctx)))
           (context-simulator-set! ctx #f)
           (child-guard-unset! sim-pid)
           (process-terminate! sim-pid)))

    (and (context-notifier ctx)
         (match-let ((#(not-in not-out not-pid) (context-notifier ctx)))
           (context-notifier-set! ctx #f)
           (child-guard-unset! not-pid)
           (process-terminate! not-pid)))

    (if (context-scheduling? ctx)
      (scheduler-stop!)
      (context-scheduling?-set! ctx #f)))

  (define-syntax define-colored-format
    (syntax-rules ()
      ((_ name fmts)
       (define (name fmt . args)
         (if (terminal-port? (current-output-port))
           (format fmts (apply format fmt args))
           (apply format fmt args))))))

  (define (halt!)
    (signal (make-composite-condition
              (make-property-condition 'peacock)
              (make-property-condition 'stopped))))

  (define-colored-format green "\x1b[32m~A\x1b[0m")
  (define-colored-format red "\x1b[31m~A\x1b[0m")
  (define-colored-format blue "\x1b[34m~A\x1b[0m")

  (define (pass fmt . args)
    (display (green "[PASS]"))
    (printf " ~A~%" (apply format fmt args))
    (stop! (current-context))
    (halt!)
    (return '()))

  (define (note fmt . args)
    (display (blue "[NOTE]"))
    (printf " ~A~%" (apply format fmt args))
    (return '()))

  (define (fail* fmt . args)
    (display (red "[FAIL]"))
    (printf " ~A~%" (apply format fmt args)))

  (define (fail fmt . args)
    (apply fail* fmt args)
    (stop! (current-context))
    (halt!)
    (return '()))

  (define (navigated-run! ctx)
    (let ((nv-pid (process-run/rg (cuauv-path (navigated-bin)) (navigated-args))))
      (context-navigated-set! ctx nv-pid)
      (child-guard-set! nv-pid (lambda (pid)
                                 (bind (return '())
                                       (lambda (_)
                                         (fail "auv-navigated exited abnormally.")))))))

  (define (mission-run! ctx mission)
    (match-let* ((#(mod* task) mission)
                 (mod (string-append (mission-module-prefix) mod*)))

      ; Start mission-runner
      (let ((mr-pid (process-run/rg (cuauv-path (mission-runner-bin))
                                    (append (mission-runner-args) (list (string-append mod "." task))))))
        (context-mission-runner-set! ctx mr-pid)
        (child-guard-set! mr-pid (lambda (pid)
                                   ; Make the scheduler run this.
                                   ; Should really clarify what assumptions each thing makes.
                                   (bind (return '())
                                         (lambda (_)
                                             (fail "Mission runner exited abnormally."))))))))

  (define (shm-setup!)
    (switches.soft-kill-set! #f)

    (settings-control.enabled-set! #t)
    ; why aren't these booleans in shm? who knows.
    (settings-control.depth-active-set! 1)
    (settings-control.pitch-active-set! 1)
    (settings-control.heading-active-set! 1)
    (settings-control.velx-active-set! 1)
    (settings-control.vely-active-set! 1)
    (settings-control.roll-active-set! 1)

    (desires-zero!)
    )

  (define (shm-teardown!)
    (switches.soft-kill-set! #t)
    (settings-control.enabled-set! #f))

  (define (peacock* setup mission #!key (debug #f))
    (match-let* ((ctx (init!)))
      (define (peacock**)
        (thread-start!
          (make-thread
            (lambda ()
              ; If the server shuts down the connection, deadlock will occur,
              ; because calls to simulator methods will block on the read
              ; queue but nothing will be able to fill it.
              (condition-case (bowl:poll!)
                (e (exn fishbowl net)
                   (when (context-simulator ctx)
                     (stop! ctx)
                     (abort e)))
                (e ()
                   (stop! ctx) (abort e))))))
        (bowl:run)
        (shm-init!)
        (shm-setup!)
        (setup)
        (bind (return '())
              (lambda (_)
                (context-scheduling?-set! ctx #t)
                (navigated-run! ctx)
                (mission-run! ctx mission)
                (return '())))
        (scheduler-start!))
      (condition-case
        (if debug
          (peacock**)
          (condition-case (peacock**)
            (e (exn)
               (stop! ctx)
               (printf "~A ~A~%" (red "[CRITICAL]") ((condition-property-accessor 'exn 'message) e)))))
        ((peacock stopped) #t))
      (shm-teardown!)))

  (define (mission* #!key module task)
    (vector module task))

  (define-syntax peacock
    (syntax-rules (setup mission options)
      ((_ name (setup setup-body ...) (mission mission-args ...) (options args ...))
       (peacock* (lambda ()
                   setup-body ...)
                 (mission* mission-args ...)
                 args ...))))

  (define (on-signal sig)
    (let* ((i (new-ivar))
           (d #f)
           (f (lambda (sig)
                (thread-start!
                  (make-thread
                    (lambda ()
                      (unless d
                        (set! d #t)
                        (ivar-fill! i sig))))))))
      (set-signal-handler! sig f)
      (ivar-read i)))

  (define (stop-on-signals! ctx)
    (define (handle-signal signal name)
      (bind (on-signal signal)
            (lambda (_)
              (printf "Received ~A, stopping.~%" name)
              (stop! ctx)
              (return '()))))
    (handle-signal signal/int "SIGINT")
    (handle-signal signal/term "SIGTERM"))
  )

; vim: set lispwords+=peacock,entity,camera,entity-stream,collision,collision-sweep,vehicle,shape :
