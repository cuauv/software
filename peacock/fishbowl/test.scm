(use fishbowl)
(connect 7772)
(thread-start! (make-thread (lambda ()(poll!))))
(oadd "turbulence"
      #(-10 -10 -10) #(10 10 10) #(-2.5 2.5)
      #(-5 -5 -5) #(5 5 5) #(-1 1))
