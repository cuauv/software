(module peacock-geometry (pi e

                          deg->rad rad->deg

                          ~= ~== ++
                          mask
                          hpr->quat
                          quat->hpr

                          q* q- qr qd qd2 q~=
                          qd<
                          dot 

                          mod
                          )
  (import scheme chicken)

  (define pi 3.1415926535897932384626433832795028841971693993751)
  (define e 2.7182818284590452353602874713526624977572470936999)

  (define (deg->rad d)
    (* d (/ (* 2 pi) 360)))

  (define (rad->deg r)
    (* r (/ 360 (* 2 pi))))

  (define (~= epsilon)
    (lambda (a b)
      (<= (abs (- a b)) epsilon)))

  (define-syntax vector-unpack
    (syntax-rules ()
      ((_ v (x y z))
       (begin
         (define x (vector-ref v 0))
         (define y (vector-ref v 1))
         (define z (vector-ref v 2))))))

  (define axes-ids #(x y z))

  (define (~== r #!optional axes #!key (squared #f))
    (lambda (a #!optional b)
      (if (not b)
        (set! b (make-vector (vector-length a) 0)))
      (let ((r2 (if squared (* r r) r))
            (s2 (let loop ((i 0)
                           (s2 0))
                  (if (= i (vector-length b))
                    s2
                    (if (and axes (not (member (vector-ref axes-ids i) axes)))
                      (loop (+ i 1) s2)
                      (let ((d (- (vector-ref a i) (vector-ref b i))))
                        (loop (+ i 1) (+ s2 (* d d)))))))))
        (<= s2 r2))))

  ; Creates a vector where all entries are [_] except for those at indices in
  ; [m] where the entry is not [_].
  ; For use with [~==], which ignores differences where vector entries are [_].
  (define (mask v m)
    (let ((v* (make-vector (vector-length v) '_)))
      (let loop ((i 0))
        (when (< i (vector-length m))
          (if (not (eq? (vector-ref m i) '_))
            (vector-set! v* i (vector-ref v i)))
          (loop (+ i 1))))
      v*))

  (: ++ ((vector number number number) (vector number number number) -> (vector number number number)))
  (define (++ a b)
    (vector-unpack a (ax ay az))
    (vector-unpack b (bx by bz))
    (vector (+ ax bx) (+ ay by) (+ az bz)))

  (: -- ((vector number number number) (vector number number number) -> (vector number number number)))
  (define (-- a b)
    (vector-unpack a (ax ay az))
    (vector-unpack b (bx by bz))
    (vector (- ax bx) (- ay by) (- az bz)))

  (define (quat->hpr q)
    (let ((w (vector-ref q 0))
          (x (vector-ref q 1))
          (y (vector-ref q 2))
          (z (vector-ref q 3)))
      (vector (atan (* 2 (+ (* w z) (* x y)))
                    (- 1 (* 2 (+ (* y y) (* z z)))))
              (asin (* 2 (- (* w y) (* z x))))
              (atan (* 2 (+ (* w x) (* y z)))
                    (- 1 (* 2 (+ (* x x) (* y y))))))))

  (define (hpr->quat w)
    (let ((hh (/ (vector-ref w 0) 2))
          (pp (/ (vector-ref w 1) 2))
          (rr (/ (vector-ref w 2) 2)))
      (vector (+ (* (cos rr) (cos pp) (cos hh)) (* (sin rr) (sin pp) (sin hh)))
              (- (* (sin rr) (cos pp) (cos hh)) (* (cos rr) (sin pp) (sin hh)))
              (+ (* (cos rr) (sin pp) (cos hh)) (* (sin rr) (cos pp) (sin hh)))
              (- (* (cos rr) (cos pp) (sin hh)) (* (sin rr) (sin pp) (cos hh))))))

  ; Quaternion product.
  (define (q* a b)
    (let ((aw (vector-ref a 0))
          (ax (vector-ref a 1))
          (ay (vector-ref a 2))
          (az (vector-ref a 3))
          (bw (vector-ref b 0))
          (bx (vector-ref b 1))
          (by (vector-ref b 2))
          (bz (vector-ref b 3)))
      (vector (- (* aw bw) (* ax bx) (* ay by) (* az bz))
              (+ (* aw bx) (* ax bw) (* ay bz) (* -1 az by))
              (+ (* aw by) (* -1 ax bz) (* ay bw) (* az bx))
              (+ (* aw bz) (* ax by) (* -1 ay bx) (* az bw)))))

  ; Quaternion conjugate.
  ; If the quaternion is a unit quaternion, equivalent to the inverse.
  (define (q- a)
    (vector (vector-ref a 0)
            (- (vector-ref a 1))
            (- (vector-ref a 2))
            (- (vector-ref a 3))))

  ; Quaternion rotation.
  ; q must be a unit quaternion, while v must be a vector.
  (define (qr q v)
    (let* ((vq (vector 0 (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))
           (rq (q* (q* q vq) (q- q))))
      (vector (vector-ref rq 1)
              (vector-ref rq 2)
              (vector-ref rq 3))))

  ; Dot product of two vectors.
  (define (dot a b)
    (+ (* (vector-ref a 0) (vector-ref b 0))
       (* (vector-ref a 1) (vector-ref b 1))
       (* (vector-ref a 2) (vector-ref b 2))))

  ; Difference between two unit quaternions a and b.
  (define (qd a b)
    (q* a (q- b)))
  
  ; Shorthand for finding the norm of the euler angles vector resulting
  ; from [(qd a b)] divided by sqrt(3pi^2).
  ; Should be used as an easy way to calculate the difference between the
  ; orientations expressed by two versors if you don't really care about the
  ; specific differences in (h, p, r).
  ; A difference of ~1.8 deg in one axis -> ~0.00577.
  (define (qd2 a b)
    (let ((e (quat->hpr (qd a b))))
      (sqrt (/ (let loop ((i 0)
                          (a 0))
                 (if (= i (vector-length e))
                   a
                   (let ((x (vector-ref e i)))
                     (loop (+ i 1) (+ a (* x x))))))
               (* 3 (* pi pi))))))

  (define (qd< a b v)
    (call/cc
      (lambda (return)
        (let* ((d (quat->hpr (qd a b))))
          (let loop ((i 0))
            (if (= i (vector-length d))
              (return #t))
            (if (>= (abs (vector-ref d i)) (vector-ref v i))
              (return #f)
              (loop (+ i 1))))))))

  (define default-max-qd 0.01)

  (define (q~= #!optional r)
    (if (not r)
      (set! r default-max-qd))
    (lambda (a b)
      (<= (qd2 a b) r)))

  ; From FMOD(3):
  ; These functions compute the floating-point remainder of dividing x by y. The
  ; return value is x - n * y, where n is the quotient of x / y, rounded toward
  ; zero to an integer.
  (define (mod x y)
    (let ((n (truncate (/ x y))))
      (- x (* n y))))
  )
