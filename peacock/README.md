First we generate a test file:

`peacock new "Basic buoy test"`

This makes a new file, `basic-buoy-test.scm`.

Opening the file,

```
(use peacock async (prefix cuauv-sim-protocol: proto:))

(peacock "Basic buoy test"
  (setup
    (vehicle
      x: #(0 0 0) r: 0.5)

    (sequence
      (after 60)
      (fail "Timed out.")))

  (mission
    file: "mission.py"
    module: "MissionTask")

  )
```

Let's write an example test:

```
(use peacock async (prefix cuauv-sim-protocol proto:))

(peacock "Basic buoy test"
  (setup
    (vehicle
      x: '(0 0 0) r: 0.5)

    ; cameras named A<<B
    ; (a is looking at B)
    ; collisions named A**B

    ; expands to
    ; (entity orange-buoy
    ;   x: ... r: ...)
    ; (camera vehicle<<orange-buoy
    ;   vehicle << orange-buoy
    ;   default-camera-proc)
    (shape orange-buoy
      r: 0.05 x: (++ (@ (current-vehicle) #(1 0 0))))

    (shape green-buoy
      r: 0.05 x: (++ (@ (current-vehicle) #(1 1 0))))

    (collision vehicle**orange-buoy
      vehicle ** orange-buoy)

    ; Can't just macro this because what if we want to use a variable for an
    ; entity ID that doesn't correspond to its name?
    ; e.g. (current-vehicle)
    (collision vehicle**green-buoy
      vehicle ** green-buoy)

    ; current-stream-registrar
    ; ((current-stream-registrar) vehicle**green-buoy
    ;   (lambda ()
    ;     
    ;     ))
    (define (triggered trigger)
      (let ((i (new-istruct)))
        ((current-stream-registrar) vehicle**green-buoy
          (lambda ()
            (istruct-fill! i (current-time)) ; from srfi-19
            ))
        (istruct-read i)))

    (sequence
      (triggered vehicle**orange-buoy)
      (triggered vehicle**green-buoy)
      (pass "Successfully rammed orange then green buoy."))

    (sequence
      (after 10)
      (fail "Timed out."))
    )

  (mission
    file: "buoy.py"
    task: "BuoyTask")
  )
```

We run it with

`peacock run basic-buoy-test`

or

`peacock`

to run all the tests in the current directory.
