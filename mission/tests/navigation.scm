(include "peacock-prelude")

(peacock "Navigation Mission Test"
  (setup
    (note "Navigation test begun! Will check that the sub performs the navigation style and does not surface/hit the bar")

    (define timeout 120)
    (define sub-initial-pos #(0 0 1.5))
    (define central-pos #(5 5 4))
    (define above-pos #(5 5 3))
    (define beyond-pos #(5.5 5.5 4))
    (define below-pos #(5 5 5))
    (define before-pos #(4.5 4.5 4))

    (define (foldl f acc l) 
      (if (null? l) 
        acc
        (foldl f (f acc (car l)) (cdr l)))
      )

    (>>= (on 'kalman.depth kalman.depth-ref (lambda (val) (< val 0.1))) (lambda (_) (fail "Surfaced, oh no!")))
    (>>= (after timeout) (lambda (_) (fail "Mission timed out!")))

    (vehicle-set!
      x: sub-initial-pos
      q: (hpr->quat #(/ (* 3 pi) 2) 0 0)
      )   

    ; Can we simulate two forward cameras?
    (camera forward 
      q: (hpr->quat (vector 0 0 0)) 
      w: 1024 h: 1024 f: 440 
    ) 

    (goals
      navigate-with-style
    )   

    (entity navigation
      x: central-pos r: 0.2 
      corporeal: #f
    )

    (entity above-target
      x: above-pos r: 0.2
      corporeal: #f
    )
    
    (entity beyond-target
      x: beyond-pos r: 0.2
      corporeal: #f
    )

    (entity below-target
      x: below-pos r: 0.2
      corporeal: #f
    )

    (entity before-target
      x: before-pos r: 0.2
      corporeal: #f
    )

    (collision vehicle**navigation vehicle ** navigation)
    (collision vehicle**above-target vehicle ** above-target)
    (collision vehicle**below-target vehicle ** below-target)
    (collision vehicle**beyond-target vehicle ** beyond-target)
    (collision vehicle**before-target vehicle ** before-target)

    (>>= (triggered vehicle**navigation) (lambda (_) (fail "Ran into navigation target!")))

    (define start (new-ivar))

    ; I would like to fold a list of deferreds. Not sure that's possible with >>= though. The lambda thing seems OK, but it complains about "expected procedure"?
    ; I actually want vehicle**above-target to happen twice (independently). How best to implement?

    (>>=
      (triggered vehicle**above-target)
      (lambda (_) (triggered vehicle**beyond-target))
      (lambda (_) (triggered vehicle**below-target))
      (lambda (_) (triggered vehicle**before-target))
      (lambda (_) (triggered vehicle**above-target))
      (lambda (_) (complete navigate-with-style))
    ) 

    ; TODO: Simulated vision. 

  )

  (mission
    module: "navigate"
    task: "loop")

  (options
    debug: #f) 
  )
