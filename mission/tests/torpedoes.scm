(include "peacock-prelude")

(peacock "Torpedoes Peacock Test"
  (setup
    (define timeout 300)
    (define torpedos-pos #(1 0 2))
        
    ; The 4 holes
    (define tl-pos #(1 -0.4 1.6))
    (define tr-pos #(1 0.4 1.6))
    (define bl-pos #(1 -0.4 2.4))
    (define br-pos #(1 0.4 2.4))
    (define hole-radius 0.1)
     
    (define torpedo-trigger 'actuator-desires.trigger-01)
     
    (vehicle-set!
      x: #(-1 0 0.4)
      )
        
    (note "Beginning single-hole torpedo test.")
    (>>= (after timeout) (lambda (_) (fail "Timed out.")))
        
    (>>= (after 5.0) (lambda (_) (>>= (on 'kalman.depth kalman.depth-ref (lambda (val) (< val 0.1))) (lambda (_) (fail "Surfaced, oh no!")))))

    (goals win)

    (entity board
      x: torpedos-pos r: 1.3
      corporeal: #f
      xattrs: '(("render". "torpedo_board"))
    )

    (entity top-left
      x: tl-pos r: hole-radius
      corporeal: #f
      xattrs: '(("render". "torpedo_cover"))
    )

    (entity top-right
      x: tr-pos r: hole-radius
      corporeal: #f
    )

    (entity bottom-left
      x: bl-pos r: hole-radius
      corporeal: #f
    )

    (entity bottom-right
      x: br-pos r: hole-radius
      corporeal: #f
    )

    (collision vehicle**board vehicle ** board)
    (collision vehicle**top-left vehicle ** top-left)

   (define sway_at_hole 0)

    (>>= (triggered vehicle**board)
         (lambda (_)
           (note "Found board.")

         (on 'kalman.east kalman.east-ref
           (lambda (val) (and (< val (+ -0.4 0.05)) (> val (- -0.4 0.05)) (< (kalman.depth-ref) (+ 1.6 0.05)) (> (kalman.depth-ref) (- 1.6 0.05))))))
           (lambda (_)
             (note "Found top left hole.")
             (set! sway_at_hole (kalman.sway-ref))

         (on 'kalman.sway kalman.sway-ref
           (lambda (val) (or (< val (- sway_at_hole 0.25)) (> val (+ sway_at_hole 0.25))))))
           (lambda (_)
             (note "Slid cover.")

         (on 'kalman.sway kalman.sway-ref
           (lambda (val) (and (< val (+ sway_at_hole 0.1)) (> val (- sway_at_hole 0.1))))))
           (lambda (_)
             (note "Returned to hole.")

         (on 'actuator-desires.trigger-01 actuator-desires.trigger-01-ref
           (lambda (val) (equal? val 1))))
           (lambda (_)
             ((note "Fired torpedo!")
             (complete win))))
  )

  (mission
    module: "torpedoes"
    task: "SetCourse")

  (options
    debug: #f)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
