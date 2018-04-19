(include "peacock-prelude")

(peacock "Basic buoy test"
  (setup
    (vehicle-set!
      x: #(1 2 3)
      )

    (write (++ (@ vehicle) #(3 5 7)))
    (newline)
    )

  (mission
    module: "dummy"
    task: "Dummy")

  (options
    debug: #t)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
