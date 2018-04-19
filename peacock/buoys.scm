(include "peacock-prelude-debug")

(peacock "Buoys"
  (setup
    (vehicle-set!
      x: #(0 0 0)
      )

    (>>= (after 120)
         (lambda (_)
           (fail "Timed out.")))

    (define red-buoy-z 1.0)
    (define yellow-buoy-z 0.75)
    (define green-buoy-z 0.7)

    (camera forward
      q: (hpr->quat (vector 0 0 0))
      w: 1024 h: 768)

    (shape forward >> red-buoy
      (red-buoy-results.probability-set!
       red-buoy-results.center-x-set!
       red-buoy-results.center-y-set!)
      x: (vector 2.5 -1.2 red-buoy-z) r: 0.2)

    (shape forward >> green-buoy
      (green-buoy-results.probability-set!
       green-buoy-results.center-x-set!
       green-buoy-results.center-y-set!)
      x: (vector 2.5 1.2 green-buoy-z) r: 0.2)

    (shape forward >> yellow-buoy
      (yellow-buoy-results.probability-set!
       yellow-buoy-results.top-x-set!
       yellow-buoy-results.top-y-set!)
      x: (vector 2.5 0.0 yellow-buoy-z) r: 0.2)

    (collision vehicle**red-buoy
      vehicle ** red-buoy)

    (collision vehicle**green-buoy
      vehicle ** green-buoy)

    (collision vehicle**yellow-buoy
      vehicle ** yellow-buoy)

    (red-buoy-results.percent-frame-set! 0)
    (green-buoy-results.percent-frame-set! 0)
    (yellow-buoy-results.percent-frame-set! 0)

    (>>= (triggered vehicle**red-buoy)
         (lambda (_)
           (complete red-buoy)
           (note "Red buoy collision!")
           (red-buoy-results.percent-frame-set! 100)
           (triggered vehicle**green-buoy))
         (lambda (_)
           (complete green-buoy)
           (note "Green buoy collision!")
           (green-buoy-results.percent-frame-set! 100)
           (triggered vehicle**yellow-buoy))
         (lambda (_)
           ;;(complete yellow-buoy)
           (note "Yellow buoy collision!")
           (yellow-buoy-results.percent-frame-set! 100)
           (return #t)
           )
         )

    ;;(>>= (triggered vehicle**yellow-buoy)
    ;;     (lambda (_)
    ;;       (complete yellow-buoy)
    ;;       (note "Yellow buoy collision!")
    ;;       (yellow-buoy-results.percent-frame-set! 100)
    ;;       (return #t)
    ;;       )
    ;;     )

    (goals
      red-buoy
      green-buoy
      yellow-buoy
      )
    )

  (mission
    module: "buoys"
    task: "red")

  (options
    debug: #t)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
