(include "peacock-prelude")

(peacock "Pipe Mission Test"
  (setup
    (note "Pipe centering test begun! Will check that the sub hovers over the pipe and centers on the current heading.")

    (define timeout 60)
    (define sub-initial-pos #(5 5 5))
    (define pipe-north 6.0)
    (define pipe-east 6.0)
    (define pipe-depth 10.0)
    (define pipe-heading 120.0)
    (define pipe-pos #(6 6 10))
    (define downcam-pos #(0 0 0))

    (define (hdiff a b) (let ([diff (mod (- a b) 360)]) (if (> diff 180) (- diff 360) diff)))

    (>>= (on 'kalman.depth kalman.depth-ref (lambda (val) (< val 0.1))) (lambda (_) (fail "Surfaced, oh no!")))
    (>>= (after timeout) (lambda (_) (fail "Mission timed out!")))

    (vehicle-set!
      x: sub-initial-pos
      )   

    (camera downward
      q: (hpr->quat (vector 0 (- (/ pi 2)) 0))
      x: downcam-pos 
      w: 1024 h: 768)

    (goals
      center-on-pipe
      align-to-pipe
    )   

    ; r shouldn't matter here; we don't care about collisions
    (entity pipe
      x: pipe-pos r: 0.1
      corporeal: #f
    )

    (>>= (on 'kalman.north kalman.north-ref (lambda (val) ((~= 0.1) val pipe-north))) (lambda (_)
      (>>= (on 'kalman.east kalman.east-ref (lambda (val) ((~= 0.1) val pipe-east))) (lambda (_)
        (complete center-on-pipe)
        ))
      ))

    (>>= (completed center-on-pipe) (lambda (_) 
      (>>= (on 'kalman.heading kalman.heading-ref (lambda (val) ((~= 10) (hdiff val pipe-heading) 0))) (lambda (_)
        (complete align-to-pipe)
        ))
      ))
  )

  (mission
    module: "pipe"
    task: "one_pipe")

  (options
    debug: #f) 
  )
