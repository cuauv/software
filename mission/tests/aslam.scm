(include "peacock-prelude")

(peacock "ASLAM Demonstration"
  (setup
    (note "ASLAM demonstration - will simulate <noise> while ramming several buoys in succession")

    (define timeout 240)
    (define sub-initial-pos #(0 0 1))

    (define a-pos #(5 5 1.5)) 
    (define b-pos #(-4 -3 1.5))
    (define c-pos #(5 -5 0.5))
    (define d-pos #(-2 2 1))

    ; (>>= (on 'kalman.depth kalman.depth-ref (lambda (val) (< val 0.1))) (lambda (_) (fail "Surfaced, oh no!")))
    (>>= (after timeout) (lambda (_) (fail "Mission timed out!")))

    (vehicle-set!
      x: sub-initial-pos
      )

    (camera forward
      q: (hpr->quat (vector 0 0 0))
      w: 1024 h: 1024 f: 512
      ; f_p corresponds to 1cm sensor size, 0.5cm focal length, also see Peacock documentation.
    )

    (entity a-buoy-ent
      x: a-pos r: 0.2
      corporeal: #f
      xattrs: '(("render". "a_buoy_real"))
    )
  
    (entity b-buoy-ent
      x: b-pos r: 0.2
      corporeal: #f
      xattrs: '(("render". "b_buoy_real"))
    )

    (entity c-buoy-ent
      x: c-pos r: 0.2
      corporeal: #f
      xattrs: '(("render". "c_buoy_real"))
    )

    (entity d-buoy-ent
      x: d-pos r: 0.2
      corporeal: #f
      xattrs: '(("render". "d_buoy_real"))
    )

    (goals
      ram-a-buoy
      ram-b-buoy
      ram-c-buoy
      ram-d-buoy
    )

    (shape forward >> a-buoy
      (a-buoy-results.probability-set!
        a-buoy-results.center-x-set!
        a-buoy-results.center-y-set!)
      x: a-pos r: 0.2
      f: (lambda (x y r d)
            (a-buoy-results.radius-set! r)
            (list x y r d))
      )

    (shape forward >> b-buoy
      (b-buoy-results.probability-set!
       b-buoy-results.center-x-set!
       b-buoy-results.center-y-set!)
      x: b-pos r: 0.2
      f: (lambda (x y r d)
            (b-buoy-results.radius-set! r)
            (list x y r d))
    )

    (shape forward >> c-buoy
      (c-buoy-results.probability-set!
       c-buoy-results.center-x-set!
       c-buoy-results.center-y-set!)
      x: c-pos r: 0.2
      f: (lambda (x y r d)
            (c-buoy-results.radius-set! r)
            (list x y r d))
    )

    (shape forward >> d-buoy
      (d-buoy-results.probability-set!
       d-buoy-results.center-x-set!
       d-buoy-results.center-y-set!)
      x: d-pos r: 0.2
      f: (lambda (x y r d)
            (d-buoy-results.radius-set! r)
            (list x y r d))
    )

    (collision vehicle**a-buoy vehicle ** a-buoy)
    (collision vehicle**b-buoy vehicle ** b-buoy)
    (collision vehicle**c-buoy vehicle ** c-buoy)
    (collision vehicle**d-buoy vehicle ** d-buoy)

    (>>= (triggered vehicle**a-buoy) (lambda (_)
        (note "Rammed a-buoy!")
        (complete ram-a-buoy)
        )
      )

    (>>= (triggered vehicle**b-buoy) (lambda (_)
        (note "Rammed b-buoy!")
        (complete ram-b-buoy)
        )
      )

    (>>= (triggered vehicle**c-buoy) (lambda (_)
        (note "Rammed c-buoy!")
        (complete ram-c-buoy)
        )
      )

    (>>= (triggered vehicle**d-buoy) (lambda (_)
        (note "Rammed d-buoy!")
        (complete ram-d-buoy)
        )
      )
 
  )

  (mission
    module: "aslam_buoys"
    task: "Demo")

  (options
    debug: #t)
)
