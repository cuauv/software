(include "peacock-prelude-debug")

(peacock "Bins"
  (setup
    (vehicle-set!
      )

    ;(>>= (after 120)
    ;     (lambda (_)
    ;       (fail "Timed out.")))

    ; The area where bins are in TRANSDEC is 16 ft deep (the wall curves down
    ; and in the center it goes to 38 ft)
    ; The bins are 1-2 ft from the bottom of the pool (0.3-0.6 m)
    ; 16 ft is 4.8768 m
    ; 4.8768 m - 0.45 m = 4.4268 m
    ; 36.5 ft is 11.1252 m
    ; They're about .5 m apart, center to center (15 cm border on one side)
    ; Center black rectangles are 30 cm by 60 cm, average is 45 cm, radius ~0.2mo

    (define dropper-pos #(0.6 -0.15 0))
    (define grabber-pos #(-0.3 -0.15 0.3))
    (define down-camera-pos #(0.3 0.15 -0.15))
    (define bin-z 4.4268)
    
    (camera down
      q: (hpr->quat (vector 0 (- (/ pi 2)) 0))
      x: down-camera-pos
      w: 1024 h: 768)

    (shape down >> banana
      x: (vector 2.5 -1.5 bin-z) r: 0.2)

    (shape down >> soda
      x: (vector 2.5 -0.5 bin-z) r: 0.2)

    (shape down >> bijection
      x: (vector 2.5 .5 bin-z) r: 0.2)

    (shape down >> lightning
      x: (vector 2.5 1.5 bin-z) r: 0.2)

    (define primary banana)
    (define secondary lightning)
    (define primary-stream down>>banana)
    (define secondary-stream down>>lightning)
    ; Rules say the handle is perpendicular to the bins, we are having the bins
    ; along the x-axis. This assumes the handle heading is defined by the handle
    ; line. NB: This is in degrees.
    (define handle-heading 0)

    (shape down >> handle
      x: (@ primary) r: 0.2
      f: (lambda (x y r d)
           (shape-handle.hd-set! (mod (+ (- handle-heading (kalman.heading-ref)) 360) 360))
           (list x y r d)))

    (define raise-trigger 'actuator-1.trigger)
    (define lower-trigger 'actuator-2.trigger)

    (define grab-trigger 'actuator-3.trigger)
    (define open-trigger 'actuator-4.trigger)

    (define first-dropper-trigger 'actuator-5.trigger)
    (define second-dropper-trigger 'actuator-6.trigger)

    ; Initially not visible, covered by lid.
    (camera-stream-off! primary-stream)

    ((dshm:name->setter lower-trigger) #f)
    ((dshm:name->setter raise-trigger) #f)
    ((dshm:name->setter grab-trigger) #f)
    ((dshm:name->setter open-trigger) #f)
    ((dshm:name->setter first-dropper-trigger) #f)
    ((dshm:name->setter second-dropper-trigger) #f)

    (goals
      handle
      primary
      secondary
      )

    (>>= (>>$ ((changed grab-trigger to: #t)
               (lambda (_) (fail "Grab actuator enabled before lower actuator enabled.")))
              ((changed lower-trigger to: #t)))
         (lambda (_)
           (note "Grabber lowered.")
           (>>! (!! (changed raise-trigger to: #t)
                    (lambda (_)
                      (fail "Lower actuator disabled before grabbed and dropped.")))
                (changed grab-trigger to: #t)
                (lambda (_)
                  (note "I'm here")
                  (unless ((~== 0.1) (@+ grabber-pos) (@ handle))
                    (fail "Grab actuator enabled further than 0.1 m from the handle: ~A" (@+ grabber-pos)))
                  (note "Now I'm here")
                  ; Need to be perpendicular to the handle-heading.
                  ; .1 rad ~= 6 deg
                  ; .2 rad ~= 8.6 deg
                  (when (not (or (qd< (q) (hpr->quat (vector (+ (deg->rad handle-heading) (/ pi 2)) 0 0))
                                      #(.2 .1 .1))
                                 (qd< (q) (hpr->quat (vector (+ (deg->rad handle-heading) (/ pi 2) pi) 0 0))
                                      #(.2 .1 .1))))
                    (fail "Grab actuator enabled with wrong orientation: ~A" (quat->hpr (q))))
                  (down>>handle-off!)
                  (note "Handle retrieved.")
                  (changed open-trigger to: #t))))
         (lambda (_)
           (if ((~== 1 '(x y)) (@+ grabber-pos) (@ primary))
             (fail "Grab actuator disabled within 1 m horizontally of the shape the handle was covering."))
           (complete handle)
           (note "Handle discarded."))
         (lambda (_) (changed raise-trigger to: #t))
         (lambda (_) (note "Grabber raised.")))

    (define (any ds)
      (define done #f)
      (define i (new-ivar))
      (for-each
        (lambda (d)
          (bind d
                (lambda (x)
                  (unless done
                    (ivar-fill! i x)
                    (set! done #t))
                  (return '()))))
        ds)
      (ivar-read i))

    (define (all ds)
      (if (null? ds)
        (return '())
        (bind (car ds)
              (lambda (_)
                (all (cdr ds))))))

    (define-syntax >>@
      (syntax-rules ()
        ((_ (d) f ...)
         (>>= d f ...))  
        ((_ (d d* ...) f ...)
         (begin
           (>>= d f ...)
           (>>@ (d* ...) f ...)))))

    (define (never)
      (ivar-read (new-ivar)))

    (define-syntax dump
      (syntax-rules ()
        ((_ e e* ...)
         (begin
           (write (quote e))
           (newline)
           (display e)
           (newline)
           (dump e* ...)))
        ((_)
         '())))

    (>>@ ((>>$ ((completed handle)
                (lambda (_)
                  (note "Primary stream enabled.")
                  (camera-stream-on! primary-stream)
                  (never)))
               ((all (list 
                       (changed first-dropper-trigger to: #t)
                       (changed second-dropper-trigger to: #t)))
                (lambda (_)
                  (fail "Both droppers triggered before handle removed."))))
          (changed first-dropper-trigger to: #t)
          (changed second-dropper-trigger to: #t))
         (lambda (_)
           (cond
             ((and (not (completed? primary))
                   (completed? handle)
                   ((~== 0.1 '(x y)) (@+ dropper-pos) (@ primary)))
              (complete primary)
              (camera-stream-off! primary-stream)
              (note "Primary target dropped on successfully."))
             ((and (not (completed? secondary))
                   ((~== 0.1 '(x y)) (@+ dropper-pos) (@ secondary)))
              (complete secondary)
              (camera-stream-off! secondary-stream)
              (note "Secondary dropper dropped on successfully."))
             (else
               (dump
                 (++ (@) dropper-pos)
                 (completed? primary) 
                 (completed? secondary))
               (fail "Dropper triggered further than 0.1 m horizontally from either target, or over an already completed target.")))))
    )

  (mission
    module: "bins"
    task: "Bins")

  #|
  (mission
    module: "dummy"
    task: "Dummy")
  |#
  (options
    debug: #t)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
