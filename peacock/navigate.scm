(include "peacock-prelude")

(peacock "Navigation Test"
  (setup
    (vehicle-set!
      x: #(0 0 0)
      q: (hpr->quat #(0 0 0))
      )

    (>>= (after 120)
         (lambda (_)
           (fail "Timed out.")))

    (camera down
      q: (hpr->quat (vector 0 (- (/ pi 2)) 0))
      w: 1024 h: 768 f: 440)

    (camera forward
      q: (hpr->quat (vector 0 0 0))
      w: 1020 h: 1020 f: 440)

    (goals
      start-buoy
      end-buoy
      )

    (entity start-buoy
      x: #(2 0 0)
      r: 1.0
      corporeal: #f)
    
    (entity end-buoy
      x: #(4 0 0)
      r: 1.0
      corporeal: #f)

    (collision vehicle**start-buoy
      vehicle ** start-buoy)

    (collision vehicle**end-buoy
      vehicle ** end-buoy)

    (>>= (triggered vehicle**start-buoy)
      (lambda (_)
        (complete start-buoy)
        (triggered vehicle**end-buoy))
      (lambda (_)
        (complete end-buoy)))

    (camera-stream forward>>start-buoy
      forward >> start-buoy
      proc: (lambda (x y r d)
        (print x y r d)))

    (camera-stream forward>>end-buoy
      forward >> end-buoy
      proc: (lambda (x y r d)
        (print x y r d)))

    )

  (mission
    module: "navigate"
    task: "NavigateChannel")

  (options
    debug: #t)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
