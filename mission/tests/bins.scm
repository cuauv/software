(include "peacock-prelude")

(peacock "Bins Test"
  (setup

    (define sub-initial-pos #(0 0 1.5))

    (vehicle-set!
      x: sub-initial-pos
      q: (hpr->quat #(/ (* 3 pi) 2) 0 0)
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
      markers-in-bin
      )
  
    (shape down >> covered-bin
      (bin1.p-set!
       bin1.x-set!
       bin1.y-set!)
      x: #(2 -0.5 0) r: 0.08)
  
    (shape down >> uncovered-bin
      (bin2.p-set!
       bin2.x-set!
       bin2.y-set!)
      x: #(2 0.5 0) r: 0.08)
  
    )

  (mission
    module: "bins"
    task: "BinsTask")

  (mission
    module: "bins"
    task: "BinsTask")
  (options
    debug: #f)
)
