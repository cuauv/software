(include "fishbowl")
(import fishbowl)
(use srfi-18)

(connect 7772 on-stream: (lambda (m) (write m) (newline)))
(thread-start!
  (make-thread
    (lambda ()
      (poll!))))

;(eadd m: 1 r: 1 I: #(1 1 1) btom-rq: #(1 0 0 0) x: #(4 0 0) q: #(1 0 0 0) corporeal: #f)
;(eadd m: 1 r: 1 I: #(1 1 1) btom-rq: #(1 0 0 0) x: #(0 0 0) q: #(1 0 0 0) corporeal: #f)
;
;(eset 1 'v #(-1 0 0))
;
;;(cadd entity: 0 q: #(1 0 0 0) x: #(0 0 0) f: 0.035 enabled: #t)
;
;(cstadd 1 2)

(write (ptadd 0 #(0 0 0) #(5 0 0) 1))
(newline)
(run)

(print "what")
