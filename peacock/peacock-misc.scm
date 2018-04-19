; Miscellaneous Peacock Utilities / Functions

(module peacock-misc (
  on
  vary-get
  vary-set
  json-read
  json-write
  rgauss
  pos
  dump
  auvlog
  entity-set
    )

  (import scheme chicken)
  (use raisin cuauv-shm (prefix fishbowl bowl:) (prefix random-mtzig rnd:) json nanomsg)
  (use extras)
  (use ports)

  (use srfi-18 srfi-69 srfi-13 srfi-1 posix)
  (use matchable)

  (import peacock)

  (define auvlog-sock (nn-socket 'pub))
  (nn-connect auvlog-sock "tcp://127.0.0.1:7654")

  (define (auvlog tree msg)
    (let ((msg (list->vector `((tree . ,tree) (timestamp . ,(current-seconds)) (message . ,msg) (filename . ,"") (lineno . ,0) (block . ,"") (linetxt . ,"")))))
      (nn-send auvlog-sock (call-with-output-string (lambda (port) (json-write msg port))))
    )
  )

  (define (entity-set entity key val) (must (bowl:eset entity key val)))

  ;; Deferred that becomes determined when the specified reference matches the specified condition, filled with the value of the specified reference at the time of condition fulfillment.
  ;; Note: SHM variables are consistently named; there should be some way to quote-splice this.
  (define (on name name-ref func)
    (begin
      (define var (new-ivar))
      (define (wait-inner)
        (>>= (changed name)
          (lambda (_) 
            (let ([current (name-ref)])
              (if (func current)
                (begin
                  (ivar-fill! var current)
                  (return 0))
                (wait-inner)
                )   
              )   
            )   
          )   
        )   
      (begin
        (wait-inner)
        (ivar-read var)
        )   
      )   
    )

  (define rnd-st (rnd:init))

  (define (vary-get name-ref sigma)
    (define (get)
      (let ((val (name-ref)) (rval (rnd:randn! rnd-st)))
        (+ val (* sigma rval))
      )
    )
    (lambda () (get))
  )

  (define (vary-set name-ref sigma)
    (define (set val)
      (let ((rval (rnd:randn! rnd-st))) 
        (name-ref (+ val rval))
      )
    )
    (lambda (val) (set val))
  )

  (define (rgauss mu sig)
    (+ mu (* sig (rnd:randn! rnd-st)))
  )

  (define-syntax dump
  (syntax-rules ()
    ((_ e e* ...)
     (begin
       (write (quote e)) 
       (newline)
       (display e)
       (newline)
       (dump e* ...)))
    ((_)
     '())))

  (define (lookup k l) (cdr (assoc k l)))
  (define locale (lookup "CUAUV_LOCALE" (get-environment-variables)))
  (define sw (lookup "CUAUV_SOFTWARE" (get-environment-variables)))
  (define lfile (open-input-file (string-append sw (string-append "/conf/" (string-append locale ".json")))))

  ; https://gist.github.com/dhess/52681
  (define (json-read-fixup jro)
    (cond ((null? jro) jro)
          ((vector? jro) (json-read-fixup (vector->list jro)))
          ((pair? jro) (cons (json-read-fixup (car jro))
                         (json-read-fixup (cdr jro))))
          (else jro)))

  (define ldata (lookup "objects" (json-read-fixup (json-read lfile))))

  (define (pos obj) (list->vector (take (lookup "initial_position" (car (filter (lambda (v) (string= obj (lookup "name" v))) ldata))) 3)))

  (define (rguard name name-ref min-val max-val msg)
    (on name name-ref (lambda (val) (or (< val min-val) (> val max-val))) (lambda (_) (fail msg)))
    )

  (define (guard north-min north-max east-min east-max depth-min depth-max) 
    (rguard 'kalman.north kalman.north-ref north-min north-max "North out of bounds!")
    (rguard 'kalman.east kalman.east-ref east-min east-max "East out of bounds!")
    (rguard 'kalman.depth kalman.depth-ref depth-min depth-max "Depth out of bounds!")
  )

  )
