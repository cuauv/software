; Utility procedures for peacock tests.
; (Not for utility procedures used by peacock - those go in peacock-internal).
; Note: assumes peacock and peacock-geometry have already been imported.
(module peacock-util (@ @+ q
                      >>!

                      (syntax: goals goals*)
                      (syntax: complete complete*)
                      (syntax: completed completed*)
                      (syntax: completed? completed?*)
                      )
  (import scheme chicken)
  (use extras)

  (use srfi-18 srfi-69)
  (use matchable)

  (use raisin cuauv-shm fishbowl)

  (import peacock peacock-geometry)

  (define (@ #!optional (entity vehicle))
    (let ((m (eget entity 'x)))
      (match m
        (#('EGOT 'x #(x y z))
         (vector x y z)))))

  (define (@+ a #!optional b)
    (let* ((entity (if b a vehicle))
           (x* (if b b a))
           (x (match (eget entity 'x)
                (#('EGOT 'x x) x)))
           (q (if (eq? entity vehicle)
                (vector (kalman.q0-ref) (kalman.q1-ref) (kalman.q2-ref) (kalman.q3-ref))
                (match (eget entity 'q)
                  (#('EGOT 'q q) q)))))
      (++ x (qr q x*))))

  ; If the entity is the vehicle, the user probably wants it in the model frame
  ; instead of the model frame, which the simulator uses. EGET q will return it
  ; in the body frame, so we instead read from shm for that special case...
  (define (q #!optional (entity vehicle))
    (if (eq? entity vehicle)
      (vector (kalman.q0-ref)
              (kalman.q1-ref)
              (kalman.q2-ref)
              (kalman.q3-ref))
      (let ((m (eget entity 'q)))
        (match m
          (#('EGOT 'q #(w x y z))
           (vector w x y z))))))

  (define-syntax >>!
    (syntax-rules (!!)
      ((_ (!! a a* ...) b ...)
       (begin
         (use matchable)
         (define-syntax intersperse
           (ir-macro-transformer
             (lambda (x i c)
               (match x
                 ((_ g d . fs)
                  `(>>= ,d
                        ,@(map (lambda (f)
                                 `(lambda (x)
                                    (if (,g x)
                                      (return x)
                                      (,f x))))
                               fs)))))))
         (let ((i (new-ivar))
               (done #f))
           (>>= a
                (lambda (x)
                  (if done
                    (return x)
                    (begin
                      (set! done #t)
                      (>>= (return x)
                           a* ...
                           (lambda (x)
                             (ivar-fill! i x)
                             (return '())))))))
           (bind (intersperse
                   (lambda (_) done)
                   b ...)
                 (lambda (x)
                   (when (not done)
                     (set! done #t)
                     (ivar-fill! i x))
                   (return '())))
           (ivar-read i))))))

  (define (make-goal-exn fmt . args)
    (make-composite-condition
      (make-property-condition 'exn 'message (apply format fmt args))
      (make-property-condition 'goal)))

  (define current-goals (make-parameter (vector (make-hash-table) 0)))

  (define (goals* gs)
    (if (null? gs)
      (abort (make-goal-exn "You must specify at least one goal.")))
    (let ((ht (make-hash-table)))
      (current-goals (vector ht (length gs))) 
      (for-each (lambda (g)
                  (hash-table-set! ht g (vector (new-ivar) #f)))
                gs)))

  (define-syntax goals
    (syntax-rules ()
      ((_ goal ...)
       (goals* '(goal ...)))))

  (define (complete* g)
    (match-let ((#(ht n) (current-goals)))
      (if (not (hash-table-exists? ht g))
        (abort (make-goal-exn "Goal ~A does not exist." g)))
      (match-let ((#(i complete) (hash-table-ref ht g)))
        (if complete 
          (abort (make-goal-exn "Goal ~A was already completed." g)))
        (ivar-fill! i #t)
        (vector-set! (hash-table-ref ht g) 1 #t)
        (vector-set! (current-goals) 1 (- n 1)))
      (if (= (vector-ref (current-goals) 1) 0)
        ; Bind pass to a deferred so we don't immediately complete -- let the
        ; user output notes, for example.
        (bind (return '())
              (lambda (_)
                (pass "All goals complete!")))
        (return '()))))

  (define-syntax complete
    (syntax-rules ()
      ((_ goal)
       (complete* (quote goal)))))

  (define (completed* g)
    (match-let ((#(ht n) (current-goals)))
      (if (not (hash-table-exists? ht g))
        (abort (make-goal-exn "Goal ~A does not exist." g)))
      (match-let ((#(i _) (hash-table-ref ht g)))
        (ivar-read i))))

  (define-syntax completed
    (syntax-rules ()
      ((_ goal)
       (completed* (quote goal)))))

  (define (completed?* g)
    (match-let ((#(ht n) (current-goals)))
      (if (not (hash-table-exists? ht g))
        (abort (make-goal-exn "Goal ~A does not exist." g)))
      (match-let ((#(i complete) (hash-table-ref ht g)))
        complete)))

  (define-syntax completed?
    (syntax-rules ()
      ((_ goal)
       (completed?* (quote goal)))))

  )
