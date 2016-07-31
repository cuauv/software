(include "peacock-prelude")

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

    ; TODO(jyc): Update these parameters in tandem with the bins mission as we
    ; find more about how the sub is layout. At this time, the arm is not yet
    ; installed.

    (define dropper-pos #(0.6 -0.15 0))
    (define grabber-pos #(-0.3 -0.15 0.3))
    (define down-camera-pos #(0.3 0.15 -0.15))
    (define bin-z 4.4268)
    
    (camera down
      q: (hpr->quat (vector 0 (- (/ pi 2)) 0))
      x: down-camera-pos
      w: (camera.downward-width-ref) h: (camera.downward-height-ref))

    ;       x
    ;     1 | 2
    ;       |
    ; -y ---+--- y
    ;
    ; bins are 60 cm x 90 cm

    (shape down >> bin1
      (bin1.p-set!
       bin1.x-set!
       bin1.y-set!) 
      x: (vector 2.5 -0.5 bin-z) r: 0.3)

    (shape down >> bin2
      (bin1.p-set!
       bin1.x-set!
       bin1.y-set!) 
      x: (vector 2.5 0.5 bin-z) r: 0.3)

    ; Kind of lame that we have to do this ourselves.
    ; A macro would make sense.
    (define primary bin1)
    (define secondary bin1)
    (define primary-stream down>>bin1)
    (define secondary-stream down>>bin2)

    ; Rules say the handle is perpendicular to the bins, we are having the bins
    ; along the x-axis. This assumes the handle heading is defined by the handle
    ; line. NB: This is in degrees.
    (define handle-heading 0)

    (shape down >> handle
      x: (@ primary) r: 0.2
      f: (lambda (x y r d)
           ; hd is the apparent heading of the handle. We take the difference
           ; between our absolute heading and the handle's absolute heading. A
           ; macro for this would be nice, because it seems to be pretty
           ; common.
           (shape-handle.hd-set! (mod (+ (- handle-heading (kalman.heading-ref)) 360) 360))
           (list x y r d)))

    ; We don't actually know which triggers these are yet.

    (define raise-trigger 'actuator-desires.trigger-03)
    (define lower-trigger 'actuator-desires.trigger-04)

    (define grab-trigger 'actuator-desires.trigger-05)
    (define open-trigger 'actuator-desires.trigger-06)

    (define first-dropper-trigger 'actuator-desires.trigger-01)
    (define second-dropper-trigger 'actuator-desires.trigger-02)

    ; Initially not visible, covered by lid.
    (camera-stream-off! primary-stream)

    ((dshm:name->setter lower-trigger) #f)
    ((dshm:name->setter raise-trigger) #f)
    ((dshm:name->setter grab-trigger) #f)
    ((dshm:name->setter open-trigger) #f)
    ((dshm:name->setter first-dropper-trigger) #f)
    ((dshm:name->setter second-dropper-trigger) #f)

    ; Points are awarded for dropping the markers in the open bin, or on the
    ; outer white edge. To obtain maximum points, the vehicle must remove the
    ; cover and drop both markers in the bin that was once covered.
    
    ; TODO(jyc) Right now we have a binary pass / fail.
    ; See check-drop below. Should be abstracted.
    (goals
      handle
      drops
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
                  (note "Grabber triggered.")
                  (unless ((~== 0.1) (@+ grabber-pos) (@ handle))
                    (fail "Grab actuator enabled further than 0.1 m from the handle: ~A" (@+ grabber-pos)))
                  ; Need to be perpendicular to the handle-heading.
                  ; .1 rad ~= 6 deg
                  ; .2 rad ~= 8.6 deg
                  (unless (or (qd< (q) (hpr->quat (vector (+ (deg->rad handle-heading) (/ pi 2)) 0 0))
                                   #(.2 .1 .1))
                              (qd< (q) (hpr->quat (vector (+ (deg->rad handle-heading) (/ pi 2) pi) 0 0))
                                   #(.2 .1 .1))))
                    (fail "Grab actuator enabled with wrong orientation: ~A" (quat->hpr (q)))
                  (down>>handle-off!)
                  (note "Handle retrieved.")
                  (changed open-trigger to: #t))))
         (lambda (_)
           (if ((~== 1 '(x y)) (@+ grabber-pos) (@ primary))
               (fail "Grab actuator disabled within 1 m horizontally of the shape the handle was covering. This means that the cover might have dropped back onto the bin!"))
           (note "Handle discarded."))
         (lambda (_) (changed raise-trigger to: #t))
         (lambda (_)
           (complete handle)
           (note "Grabber raised.")))

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

    (>>= (completed handle)
         (lambda (_)
           (note "Primary stream enabled.")
           (camera-stream-on! primary-stream)
           (never)))

    (>>= (all (list (changed first-dropper-trigger to: #t)
                    (changed second-dropper-trigger to: #t)))
         (lambda (_)
           (fail "Both droppers triggered before handle removed.")))

    (define drop-count 0)
    (define (add1-drop-count)
      (set! drop-count (add1 drop-count))
      (if (= drop-count 2)
          (complete drops))
      (note (format "Incremented drop count to ~A." drop-count)))

    (>>@ ((changed first-dropper-trigger to: #t)
          (changed second-dropper-trigger to: #t))
         (lambda (_)
           (cond
             ((and (not (completed? primary))
                   (completed? handle)
                   ((~== 0.1 '(x y)) (@+ dropper-pos) (@ primary)))
              (camera-stream-off! primary-stream)
              (note "Primary target dropped on successfully.")
              (add1-drop-count))
             ((and (not (completed? secondary))
                   ((~== 0.1 '(x y)) (@+ dropper-pos) (@ secondary)))
              (complete secondary)
              (camera-stream-off! secondary-stream)
              (note "Secondary dropper dropped on successfully.")
              (add1-drop-count))
             (else
               (dump
                 (++ (@) dropper-pos)
                 (completed? primary) 
                 (completed? secondary))
               (fail "Dropper triggered further than 0.1 m horizontally from either target, or on the primary target with the cover still there.")))))
    )

  #;(mission
    module: "bins"
    task: "BinsTask")

  (mission
    module: "dummy"
    task: "Dummy")
  (options
    debug: #t)
  )

; vim: set lispwords+=peacock,vehicle,entity,camera,camera-stream,shape,collision :
