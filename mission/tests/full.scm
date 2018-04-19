(include "peacock-prelude")

(peacock "Full Course Mission Simulation"
  (setup
    (note "Full course mission simulation initialization!")

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

    ; 15 minutes - 900 seconds - to run the course
    (define timeout 900)
    
    ; Zero Kalman/ASLAM on initialization (at dock edge)
    (define sub-initial-pos #(0 0 1))

    (define (lookup k l) (cdr (assoc k l)))
    (define locale (lookup "CUAUV_LOCALE" (get-environment-variables)))
    (define sw (lookup "CUAUV_SOFTWARE" (get-environment-variables)))
    (note "Loading course layout...")
    (define lfile (open-input-file (string-append sw (string-append "/conf/" (string-append locale ".json")))))

    ; https://gist.github.com/dhess/52681
    (define (json-read-fixup jro)
      (cond ((null? jro) jro)
            ((vector? jro) (json-read-fixup (vector->list jro)))
            ((pair? jro) (cons (json-read-fixup (car jro))
                           (json-read-fixup (cdr jro))))
            (else jro)))

    (define ldata (lookup "objects" (json-read-fixup (json-read lfile))))

    (define sig 0.1)
    (define (randomize v) (rgauss v sig))

    (define (pos obj) (list->vector (map randomize (take (lookup "initial_position" (car (filter (lambda (v) (string= obj (lookup "name" v))) ldata))) 3))))

    ; Real element positions
    (define red-buoy-pos (pos "red_buoy"))
    (define green-buoy-pos (pos "green_buoy"))
    (define yellow-buoy-pos (pos "yellow_buoy"))
    (define pipe-to-buoys-pos (pos "pipe_to_buoys"))
    (define pipe-to-navigation-pos (pos "pipe_to_navigation"))
    (define navigation-pos (pos "navigation"))
    (define torpedoes-pos (pos "torpedoes"))
    (define recovery-tower-pos (pos "recovery_tower"))
    (define recovery-area-pos (pos "recovery_area"))

    ; (>>= (after 5.0) (lambda (_) (>>= (on 'kalman.depth kalman.depth-ref (lambda (val) (< val 0.1))) (lambda (_) (fail "Surfaced, oh dear! This means the run will end!")))))
    (>>= (after timeout) (lambda (_) (fail "Mission timed out! Points accumulated so far would be kept.")))

    (vehicle-set!
      x: sub-initial-pos
      )

    (entity red-buoy
      x: red-buoy-pos r: 0.05
      corporeal: #f
      xattrs: '(("render". "red_buoy"))
    )

    (entity green-buoy
      x: green-buoy-pos r: 0.05
      corporeal: #f
      xattrs: '(("render". "green_buoy"))
    )

    (entity yellow-buoy
      x: yellow-buoy-pos r: 0.05
      q: (hpr->quat (vector 0 0 (/ pi -2)))
      corporeal: #f
      xattrs: '(("render". "yellow_buoy"))
    )

    (entity pipe-to-buoys
      x: pipe-to-buoys-pos r: 0.1
      corporeal: #f
      q: (hpr->quat (vector (/ pi 2) 0 0))
      xattrs: '(("render". "pipe_to_buoys"))
    )

    (entity pipe-to-navigation
      x: pipe-to-navigation-pos r: 0.1
      corporeal: #f
      q: (hpr->quat (vector (/ pi 1.3) 0 0))
      xattrs: '(("render". "pipe_to_navigation"))
    )

    (entity navigation
      x: navigation-pos r: 0.1
      corporeal: #f
      xattrs: '(("render". "navigation"))
    )

    (goals
      follow-buoys-pipe
      ram-red-buoy
      ram-green-buoy
      ram-yellow-buoy
      follow-navigation-pipe
      navigation
    )

    (collision vehicle**red-buoy vehicle ** red-buoy)
    (collision vehicle**green-buoy vehicle ** green-buoy)
    (collision vehicle**yellow-buoy vehicle ** yellow-buoy)

    (define (trigger-touch)
      (begin
        (gpio.wall-1-set! 0)
        (note "Touch sensor triggered!")
        (>>= (after 1.0) (lambda (_) (gpio.wall-1-set! 1) (note "Touch sensor untriggered!") (return 0)))
      )
    )

    (>>= (triggered vehicle**red-buoy) (lambda (_)
        (note "Rammed red buoy!")
        (trigger-touch)
        (complete ram-red-buoy)
        (>>= (triggered vehicle**red-buoy) (lambda (_) (trigger-touch)))
        )
      )

    (>>= (triggered vehicle**green-buoy) (lambda (_)
        (note "Rammed green buoy!")
        (trigger-touch)
        (complete ram-green-buoy)
        (>>= (triggered vehicle**green-buoy) (lambda (_) (trigger-touch)))
        )
      )

    (>>= (triggered vehicle**yellow-buoy) (lambda (_)
        (trigger-touch)
        (note "Rammed yellow buoy!")
        (complete ram-yellow-buoy)
        (>>= (triggered vehicle**yellow-buoy) (lambda (_) (trigger-touch)))
        )
      )
  )

  (mission
    module: "full"
    task: "Full")

  (options
    debug: #t)
)
