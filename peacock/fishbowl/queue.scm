(module queue (new-queue
               queue-length
               queue-empty?
               enq
               deq)
  (import scheme chicken)

  (define-record queue
    length
    head    ; (head-value . next) or '()
    tail    ; (tail-value . '()) or '()
    )

  (define (new-queue)
    (make-queue 0 '() '()))

  (define (enq q x)
    (let ([cell (cons x '())])
      (if (null? (queue-head q))
        (begin
          (queue-head-set! q cell)
          (queue-tail-set! q cell))
        (begin
          (set-cdr! (queue-tail q) cell)
          (queue-tail-set! q cell))))
    (queue-length-set! q (+ (queue-length q) 1)))

  (define (queue-empty? q)
    (null? (queue-head q)))

  (define (deq q)
    (if (null? (queue-head q))
      (abort "Trying to deq from empty queue.")
      (let ([x (car (queue-head q))])
        (queue-head-set! q (cdr (queue-head q)))
        (if (null? (queue-head q))
          (queue-tail-set! q '()))
        (queue-length-set! q (- (queue-length q) 1))
        x)))
  )
