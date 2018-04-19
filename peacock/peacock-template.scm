(include "peacock-prelude")

(peacock "New Peacock Test"
  (setup
    (vehicle-set!
      x: #(0 0 0)
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
      win
      )
    )

  (mission
    module: "dummy"
    task: "Dummy")

  (options
    debug: #f)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
