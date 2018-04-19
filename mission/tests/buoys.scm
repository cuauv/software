(include "peacock-prelude")

(peacock "Buoy Mission Test"
  (setup
    (note "Buoy ramming test begun! Will check that the sub rams the red then green buoys and drags down the yellow buoy.")

    (define timeout 120)
    (define sub-initial-pos #(-4 -4 1))
    (define red-buoy-pos #(4.0 1.0 0.5))
    (define yellow-buoy-pos #(4 0 1))
    (define yellow-buoy-tow-depth 1.5)
    (define green-buoy-pos #(4 2 1))

    ; (>>= (after 5.0) (lambda (_) (>>= (on 'kalman.depth kalman.depth-ref (lambda (val) (< val 0.1))) (lambda (_) (fail "Surfaced, oh no!")))))
    (>>= (after timeout) (lambda (_) (fail "Mission timed out!")))

    (vehicle-set!
      x: sub-initial-pos
      )

    (entity red-buoy-ent
      x: red-buoy-pos r: 0.2
      corporeal: #f
      xattrs: '(("render". "red_buoy")) ; tie to visualizer
    )

    (entity yellow-buoy-ent
      x: yellow-buoy-pos r: 0.2
      q: (hpr->quat (vector 0 0 (/ pi -2)))
      corporeal: #f
      xattrs: '(("render". "yellow_buoy")) ; tie to visualizer
    )

    (entity green-buoy-ent
      x: green-buoy-pos r: 0.2
      corporeal: #f
      xattrs: '(("render". "green_buoy")) ; tie to visualizer
    )

    (goals
      ram-red-buoy
      ram-green-buoy
      ram-yellow-buoy
      tow-yellow-buoy
    )

    (collision vehicle**red-buoy vehicle ** red-buoy-ent)
    (collision vehicle**green-buoy vehicle ** green-buoy-ent)
    (collision vehicle**yellow-buoy vehicle ** yellow-buoy-ent)

    (>>= (triggered vehicle**red-buoy) (lambda (_) (complete ram-red-buoy)))

    ; Ordering: red must be rammed first. At the moment, this is done by ordering the goals; in the future a points-based system could be used.
    (>>= (completed ram-red-buoy) (lambda (_)
      (note "Rammed red buoy!")
      (red-buoy-results.percent-frame-set! 100)
      (>>= (triggered vehicle**green-buoy) (lambda (_) (complete ram-green-buoy)))
      ))

    (>>= (completed ram-green-buoy) (lambda (_)
      (note "Rammed green buoy!")
      (green-buoy-results.percent-frame-set! 100)
      (return #t)
      ))

    ; No particular ordering of red/green vs. yellow buoy ramming seems to be required.
    (>>= (triggered vehicle**yellow-buoy) (lambda (_) (complete ram-yellow-buoy)))

    (>>= (completed ram-yellow-buoy) (lambda (_)
      (note "Rammed yellow buoy!")
      (yellow-buoy-results.percent-frame-set! 100)
      (yellow-buoy-results.center-y-set! 2000)
      (>>=
        (on 'kalman.depth kalman.depth-ref (lambda (val) (> val yellow-buoy-tow-depth)))
        (lambda (_)
          (note "Towed down ship cutout!")
          (complete tow-yellow-buoy)
          )
        )
      ))
  )

  (mission
    module: "aslam_buoys"
    task: "AllBuoys")

  (options
    debug: #f)
)
