(module fishbowl (connect disconnect
                  poll! current-session
                  config
                  remove
                  elist
                  eadd
                  eset
                  eget
                  oadd
                  cadd
                  cset
                  cget
                  exget
                  exset
                  ctadd
                  cstadd
                  ptadd
                  estream
                  cstream
                  reassure
                  run
                  pause
                  )
  (import scheme chicken)
  (import extras)
  (use srfi-4 srfi-18 srfi-69)
  ; srfi-26 for cut and cute
  (use socket matchable bitstring)
  (use queue)
  (import ports)

  (define-syntax dbg
    (syntax-rules ()
      ((_ . _) #f)))

  #;(define-syntax dbg
    (syntax-rules ()
      ((_ . _)
       (with-output-to-port (current-error-port)
         (lambda ()
           (print . _))))))

  ; current-session is set to the last created session, or #f if no session was
  ; created. It is the default [sess] keyword argument to the simulator
  ; procedures.
  (define current-session (make-parameter #f))

  (define-syntax define-fishbowl-exn
    (ir-macro-transformer
      (lambda (x i c)
        (if (not (symbol? (cadr x)))
          (abort "Expected (define-fishbowl-exn name)."))
        `(define (,(i (string->symbol (format "make-~A-exn" (i (cadr x))))) message . fargs)
           (make-composite-condition
             (make-property-condition 'exn 'message (apply format message fargs))
             (make-property-condition 'fishbowl)
             (make-property-condition (quote ,(i (cadr x)))))))))

  (define-fishbowl-exn parse)
  (define-fishbowl-exn net)
  (define-fishbowl-exn invalid-arg)

  ; It is expected that after successfully connected,
  ; (poll) will be called in a new thread, where out-of-band messages will be
  ; handled and the received queue will be filled.
  
  (define-record session
    connected ; boolean
    socket    ; socket
    on-stream ; vector -> unspecified
    received  ; queue of received in-band messages, filled by poll
    mutex     ; used to synchronize access to received
    condition ; used to signal the received mutex being filled
    )

  (define (new-session socket on-stream)
    (make-session
      #t
      socket
      on-stream
      (new-queue)
      (make-mutex)
      (make-condition-variable)))

  (define (connect port #!key (on-stream (lambda () #t)))
    (let* ([so (socket af/inet sock/stream)])
      (socket-connect so (inet-address #f port))
      (let ([sess (new-session so on-stream)])
        (current-session sess)
        sess)))

  (define (disconnect #!optional (sess (current-session)))
    (socket-close (session-socket sess)))

  (define (handle-out-of-band sess m)
    ((session-on-stream sess) m))

  (define (out-of-band? type)
    (>= type 128))

  ; Undefined behavior results if more than one thread runs [poll]
  ; concurrently.
  (define (poll! #!optional (sess (current-session)))
    (dbg "+++ poll!ing")
    (condition-case
      (let* ((m* (read-message (session-socket sess)))
             (type* (vector-ref m* 0))
             (m (vector-ref m* 1))
             (type (vector-ref m 0)))
        (dbg "+++ polling -- read message")
        (cond
          ((eq? type 'COMPLETED)
           'completed)
          ((out-of-band? type*)
           (dbg "+++ streaming message")
           (handle-out-of-band sess m)
           (dbg "+++ streaming message done")
           (poll! sess))
          (else
            (let ((mutex (session-mutex sess))
                  (condition (session-condition sess)))
              (mutex-lock! mutex)
              (enq (session-received sess) m)
              (condition-variable-broadcast! condition)
              (mutex-unlock! mutex)
              (poll! sess)))))
      (e (exn fishbowl net)
         (let ((mutex (session-mutex sess))
               (condition (session-condition sess)))
           (mutex-lock! mutex)
           (session-connected-set! sess #f)
           (condition-variable-broadcast! condition)
           (mutex-unlock! mutex)
           (abort e)))))

  ; Returns pseudo DISCONNECTED message if disconnected.
  (define (read-response #!optional (sess (current-session)))
    (let ((mutex (session-mutex sess))
          (condition (session-condition sess))
          (m #f))
      (mutex-lock! mutex)
      (let ((q (session-received sess)))
        (let wait-for-message ()
          (when (queue-empty? q)
            (if (not (session-connected sess))
              (enq q #(DISCONNETED))
              (begin
                (mutex-unlock! mutex condition)
                (mutex-lock! mutex)
                (wait-for-message)))))
        (set! m (deq q)))
      (mutex-unlock! mutex)
      m))

  (define (assert-u8vector* x name)
    (if (not (u8vector? x))
      (abort (make-invalid-arg-exn "[~A] must be a u8vector." name))))

  (define-syntax assert-u8vector
    (syntax-rules ()
      ((_ x) (assert-u8vector* x (quote x)))))

  (define (parse-body-error body)
    (bitmatch body
      (((code 32) (message-len 32) (message (* 8 message-len) bitstring))
       (vector 'ERROR code (blob->string (bitstring->blob message))))
      (else (abort (make-parse-exn "Failed to parse ERROR message.")))))

  (define (parse-body-added body)
    (bitmatch body
      (((type 8) (id 32))
       ; TODO: use type information
       (vector 'ADDED id))
      (else (abort (make-parse-exn "Failed to parse ADDED message.")))))

  (define (u8vector->u32 vec start)
    (+ (* 16777216 (u8vector-ref vec start))
       (* 65536 (u8vector-ref vec (+ start 1)))
       (* 256 (u8vector-ref vec (+ start 2)))
       (u8vector-ref vec (+ start 3))))

  (define (parse-body-elisted body)
    (bitmatch body
      (((n 32) (ids** (* 32 n) bitstring))
       (define ids* (bitstring->u8vector ids**))
       (let loop ((ids '())
                  (i 0))
         (if (= i n)
           (vector 'ELISTED ids)
           (let ((id (u8vector->u32 ids* (* i 4))))
             (loop (cons id ids) (+ i 1))))))
      (else (abort (make-parse-exn "Failed to parse ELISTED message.")))))

  (define (parse-body-egot body)
    (match-let ((#(attr x) (bits->attr 'entity body)))
      (vector 'EGOT attr x)))

  (define (parse-body-exgot body)
    (bitmatch body
      (((key-len 8) (key* (* key-len 8) bitstring) (data-len 32) (data* (* data-len 8) bitstring))
       (vector 'EXGOT
               (blob->string (bitstring->blob key*))
               (blob->string (bitstring->blob data*))))
      (else (abort (make-parse-exn "Faild to parse EXGOT message.")))))

  (define (parse-body-simple type body)
    (bitmatch body
      (((id 32)) (vector type id))
      (else (abort (make-parse-exn "Failed to parse ~A message." type)))))

  (define (parse-body-edata body)
    (bitmatch body
      (((id 32) (n 8) (data* bitstring))
       (let* ((data** (bitstring->u8vector data*))
              (n* (u8vector-length data**)))
         (let loop ((data '())
                    (i 0))
           (if (= i n*)
             (vector 'EDATA id data)
             (let* ((id (u8vector-ref data** i))
                    (size (attr->size 'entity (id->attr 'entity id)))
                    (datum (bits->attr 'entity (subu8vector data** i (+ i 1 size)))))
               (loop (cons datum data) (+ i 1 size)))))))
      (else (abort (make-parse-exn "Failed to parse EDATA message header.")))))

  (define (parse-body-cdata body)
    (bitmatch body
      (((id 32) (x float big) (y float big) (r float big) (d float big))
       (vector 'CDATA id x y r d))
      (else (abort (make-parse-exn "Failed to parse CDATA message.")))))

  (define (parse-body-configed body)
    (bitmatch body
      (((a double big) (b double big) (c 8 boolean) (d 8 boolean))
       (vector 'CONFIGED a b c d))
      (else (abort (make-parse-exn "Failed to parse CONFIG message.")))))
  
  ; parse-obdy
  ; type : uint8
  ; len : uint32
  ; body : bitstring or #f if zero-length body
  (define (parse-body type len body)
    (case type
      ((0) ; OK
       #(OK))
      ((1) ; ERROR
       (parse-body-error body))
      ((2) ; ADDED
       (parse-body-added body))
      ((3) ; ELISTED
       (parse-body-elisted body))
      ((4) ; EGOT
       (parse-body-egot body))
      ((8) ; CONFIGED
       (parse-body-configed body))
      ((9) ; EXGOT
       (parse-body-exgot body))
      ((128) ; TRIGGERED
       (parse-body-simple 'TRIGGERED body))
      ((129) ; EDATA
       (parse-body-edata body))
      ((130) ; CDATA
       (parse-body-cdata body))
      ((131) ; UNTRIGGERED
       (parse-body-simple 'UNTRIGGERED body))
      ((132) ; PSTART
       (parse-body-simple 'PSTART body))
      ((133) ; PFAIL
       (parse-body-simple 'PFAIL body))
      ((134) ; PEND
       (parse-body-simple 'PEND body))
      ((255) ; COMPLETED
       #(COMPLETED))
      (else (abort (make-parse-exn "Unrecognized message type: ~A." type)))
      )
    )

  (define (try-socket-receive*! from buf i j)
    (define n (socket-receive! from buf i))
    (if (= n 0)
      (abort (make-net-exn "[from] socket was shutdown."))) 
    (if (= (+ i n) j)
      #t
      (try-socket-receive*! from buf (+ i n) j)))

  ; Can throw (exn i/o net), should be handled externally.
  ; from : socket
  ; buf : blob
  (define (try-socket-receive! from buf)
    (try-socket-receive*! from buf 0 (blob-size buf)))

  ; read-message
  ; from : socket
  (define (read-message from)
    ; 1 for type, 4 for length
    (define buf (make-blob 5))
    (dbg "read-message: receiving header")
    (try-socket-receive! from buf)
    (dbg "read-message: received header")
    (bitmatch (blob->u8vector/shared buf)
      (((type 8) (len 32))
       (if (= len 0)
           (vector type (parse-body type len #f))  
           (let ((body (make-blob len)))
             (dbg "read-message: receiving body")
             (try-socket-receive! from body)
             (dbg "read-message: received body")
             (vector type (parse-body type len body)))))
      (else (abort (make-parse-exn "Invalid header received.")))))

  (define (send* to message)
    (let ((j (/ (bitstring-length message) 8))
          (blob (bitstring->blob message)))
      (let loop ((i 0))
        (define n (socket-send to blob i))
        (if (= (+ i n) j)
          #t
          (loop (+ i n))))))

  ; send
  ; to : socket
  ; type : uint8
  ; ?body : bitstring or #f for zero-length body
  (define (send to type #!optional (body #f))
    (if body
      (let* ((len (/ (bitstring-length body) 8))
             (message (bitconstruct (type 8) (len 32) (body bitstring))))
        (send* to message))
      (let ((len 0))
        (send* to (bitconstruct (type 8) (len 32))))))

  (define-syntax define-request
    (syntax-rules (raw #!key #!rest)
      ((_ raw (name sess args ...) body ...)
       (define (name args ...)
         (if (not sess)
           (abort (make-invalid-arg-exn "The current-session has either not been initialized or has been set to #f."))
           (begin
             (match (begin body ...)
               (#(type rbody) (send (session-socket sess) type rbody))
               ((? fixnum? type) (send (session-socket sess) type))
               (else (syntax-error "The body of define-request should evaluate to type : uint8 or #(type rbody), with type : uint8 and body : bitstring or #f.")))
             (read-response sess)))))
      ((_ (name #!key args ...) body ...)
       (define-request raw (name sess #!key args ... (sess (current-session))) body ...))
      ((_ (name args ... #!rest rest) body ...)
       (define-request raw (name sess args ... #!rest rest #!key (sess (current-session))) body ...))
      ((_ (name args ...) body ...)
       (define-request raw (name sess args ... #!key (sess (current-session))) body ...))))

  (define-request (reassure)
    33)

  (define-request (remove type id)
    (let ((type* (case type
                   ((entity) 0)
                   ((object) 1)
                   ((camera) 2)
                   ((trigger) 3)
                   ((entity-stream) 4)
                   ((camera-stream) 5)
                   ((camera-broadcast) 6)
                   (else (abort (make-invalid-arg-exn "Invalid remove type.")))))) 
      (vector 0 (bitconstruct (type* 8) (id 32)))))

  (define-request (elist)
    1)

  (define-syntax req/kp
    (syntax-rules ()
      ((_ name)
       (when (not name)
         (abort (make-invalid-arg-exn "Missing required keyword argument ~A." (quote name)))))
      ((_ name ...)
       (begin
         (req/kp name) ...))))

  (define (xattr->bits xattr)
    (define key (car xattr))
    (define data (cdr xattr))
    (define key-len (string-length key))
    (define data-len (string-length data))
    (define key* (string->bitstring key))
    (define data* (string->bitstring data))
    (bitconstruct
      (key-len 8) (key* bitstring)
      (data-len 32) (data* bitstring)))

  ; `xattrs` should be an alist of key-value pairs, all strings.
  ; For example,
  ;   (("render" . "buoy") ("name" . "Spitzer"))
  ; Extended attribute values are technically opaque bytestrings, but we take
  ; them all as strings.
  (define-request (eadd #!key m r I btom-rq x q corporeal xattrs)
    (set! xattrs (or xattrs '()))
    (req/kp m r I btom-rq x q)
    (define xattrs-n (length xattrs))
    (define xattrs* (apply bitstring-append (map xattr->bits xattrs)))
    (match (list I btom-rq x q)
      ((#(Ix Iy Iz)
        #(btom-rqw btom-rqx btom-rqy btom-rqz)
        #(xx xy xz)
        #(qw qx qy qz))
       (vector 2 (bitconstruct
                   (m double big) (r double big)
                   (Ix double big) (Iy double big) (Iz double big)
                   (btom-rqw double big) (btom-rqx double big) (btom-rqy double big) (btom-rqz double big)
                   (xx double big) (xy double big) (xz double big)
                   (qw double big) (qx double big) (qy double big) (qz double big)
                   (corporeal 8 boolean)
                   (xattrs-n 8) (xattrs* bitstring))))))

  (define-request (estream id period attrs)
    (let ((attrs* (u8vector->bitstring (list->u8vector (map (cut attr->id 'entity <>) attrs))))
          (n (length attrs)))
      (vector 14 (bitconstruct
                   (id 32) (period 32)
                   (n 8)
                   (attrs* bitstring)))))

  (define-request (cstream id period target)
    (vector 15 (bitconstruct (id 32) (period 32) (target 32))))

  (define-request (run #!optional (steps 0))
    (vector 254 (bitconstruct (steps 64))))

  (define-request (pause)
    255)

  (define attr-ht (make-hash-table))
  (define id->attr-ht (make-hash-table))

  (define-syntax attr-add!
    (syntax-rules ()
      ((_ class name id size encoder decoder)
       (let ((class* (quote class))
             (name* (quote name)))
         (hash-table-set! id->attr-ht (cons class* id) name*)
         (hash-table-set! attr-ht (cons class* name*) (vector id size encoder decoder))))))

  (define (attr->id class attr)
    (if (not (hash-table-exists? attr-ht (cons class attr)))
      (abort (make-invalid-arg-exn "The given [(class, attr)] does not exist.")))
    (match-let ((#(id _ _ _) (hash-table-ref attr-ht (cons class attr))))
      id))

  (define (attr->size class attr)
    (if (not (hash-table-exists? attr-ht (cons class attr)))
      (abort (make-invalid-arg-exn "The given [(class, attr)] does not exist.")))
    (match-let ((#(_ size _ _) (hash-table-ref attr-ht (cons class attr))))
      size))

  (define (attr->bits class attr x)
    (define id (attr->id class attr))
    (match-let ((#(_ _ encoder _) (hash-table-ref attr-ht (cons class attr))))
      (define x* (encoder x))
      (bitconstruct (id 8) (x* bitstring))))

  ; Meant to be called by parsing functions.
  (define (id->attr class id)
    (if (not (hash-table-exists? id->attr-ht (cons class id)))
      (abort (make-parse-exn "Received invalid attribute id.")))
    (hash-table-ref id->attr-ht (cons class id)))

  (define (bits->attr class bits)
    (bitmatch bits
      (((id 8) (x* bitstring))
       (define attr (id->attr class id))
       (match-let* ((#(_ _ _ decoder) (hash-table-ref attr-ht (cons class attr))))
         (vector attr (decoder x*))))))

  (define (double->bits x)
    (bitconstruct (x double big)))

  (define (bits->double x*)
    (bitmatch x*
      (((x double big)) x)
      (else (abort (make-parse-exn "Failed to decode double attribute.")))))

  (define (bool->bits x)
    (bitconstruct (x 8 boolean)))

  (define (bits->bool x*)
    (bitmatch x*
      (((x 8 boolean)) x)
      (else (abort (make-parse-exn "Failed to decode boolean attribute.")))))

  (define (vec3d->bits x)
    (match x
      (#(x y z)
       (bitconstruct (x double big) (y double big) (z double big)))
      (else
        (abort (make-invalid-arg-exn "Expected #(x y z) for vec3d attribute.")))))

  (define (bits->vec3d x*)
    (bitmatch x*
      (((x double big) (y double big) (z double big)) (vector x y z))
      (else (abort (make-parse-exn "Failed to decode vec3d attribute.")))))

  ; The only vectors of four doubles we are dealing with are quaternions, hence
  ; the w x y z naming.
  (define (vec4d->bits x)
    (match x
      (#(w x y z)
       (bitconstruct (w double big) (x double big) (y double big) (z double big)))
      (else
        (abort (make-invalid-arg-exn "Expected #(w x y z) for vec4d attribute.")))))

  (define (bits->vec4d x*)
    (bitmatch x*
      (((w double big) (x double big) (y double big) (z double big)) (vector w x y z))
      (else (abort (make-parse-exn "Failed to decode vec4d attribute.")))))

  (attr-add! entity x 0 24 vec3d->bits bits->vec3d)
  (attr-add! entity v 1 24 vec3d->bits bits->vec3d)
  (attr-add! entity a 2 24 vec3d->bits bits->vec3d)
  (attr-add! entity q 3 32 vec4d->bits bits->vec4d)
  (attr-add! entity w 4 24 vec3d->bits bits->vec3d)
  (attr-add! entity corporeal 5 1 bool->bits bits->bool)
  
  (define-request (eset id attr x)
    (let ((bits (attr->bits 'entity attr x)))
      (vector 3 (bitconstruct (id 32) (bits bitstring)))))
  
  (define-request (eget id attr)
    (let ((attr* (attr->id 'entity attr)))
      (vector 4 (bitconstruct (id 32) (attr* 8)))))

  (define class-data->bits-ht (make-hash-table))

  (define (class-data->bits class data)
    (if (not (hash-table-exists? class-data->bits-ht class))
      (abort (make-invalid-arg-exn "Unknown class name: ~A" class))
      ((hash-table-ref class-data->bits-ht class) data)))

  (define (class-data->bits-add! class f)
    (hash-table-set! class-data->bits-ht class f))

  (class-data->bits-add! "turbulence"
    (lambda (data)
      (match data
        ((#(fxa fya fza) #(fxb fyb fzb) #(fda fdb)
          #(txa tya tza) #(txb tyb tzb) #(tda tdb))
         (bitconstruct
           (fxa double big) (fya double big) (fza double big)
           (fxb double big) (fyb double big) (fzb double big)
           (fda double big) (fdb double big)

           (txa double big) (tya double big) (tza double big)
           (txb double big) (tyb double big) (tzb double big)
           (tda double big) (tdb double big)))
        (else (abort (make-invalid-arg-exn "Unexpected turbulence object data: ~A" data))))))

  (define-request (exset entity key data)
    (define key-len (string-length key))
    (define key* (string->bitstring key))
    (define data-len (string-length data))
    (define data* (string->bitstring data))
    (vector 19 (bitconstruct (entity 32)
                 (key-len 8) (key* bitstring)
                 (data-len 32) (data* bitstring))))
  
  (define-request (exget entity key)
    (define key-len (string-length key))
    (define key* (string->bitstring key))
    (vector 20 (bitconstruct (entity 32) (key-len 8) (key* bitstring))))

  (define-request (oadd class #!rest data)
    (let ((class-len (string-length class))
          (data-bits (class-data->bits class data)))
      (vector 7 (bitconstruct (class-len 32) (class bitstring) (data-bits bitstring)))))

  (define-request (cadd #!key entity q x f enabled)
    (req/kp entity q x f)
    (match (list q x)
      ((#(qw qx qy qz) #(xx xy xz))
       (vector 10 (bitconstruct (entity 32)
                                (qw double big) (qx double big) (qy double big) (qz double big)
                                (xx double big) (xy double big) (xz double big)
                                (f double big) (enabled 8 boolean))))))

  (attr-add! camera q 0 32 vec4d->bits bits->vec4d)
  (attr-add! camera x 1 24 vec3d->bits bits->vec3d)
  (attr-add! camera f 2 8 double->bits bits->double)
  (attr-add! camera enabled 3 1 bool->bits bits->bool)

  (define-request (cset id attr x)
    (let ((bits (attr->bits 'camera attr x)))
      (vector 11 (bitconstruct (id 32) (bits bitstring)))))

  (define-request (cget id attr)
    (let ((attr* (attr->id 'camera attr)))
      (vector 12 (bitconstruct (id 32) (attr* 8)))))

  (define-request (tadd type config)
    (vector 13 (bitconstruct (type 8) (config bitstring))))

  (define (ctadd a b #!key (r #f))
    (let ((r (if (not r) -1 r)))
      (tadd 0 (bitconstruct (a 32) (b 32) (r double big)))))

  (define (cstadd a b)
    (tadd 1 (bitconstruct (a 32) (b 32))))

  (define (ptadd entity x0 x1 r)
    (let ((x0* (vec3d->bits x0))
          (x1* (vec3d->bits x1))) 
      (tadd 3 (bitconstruct (entity 32) (x0* bitstring) (x1* bitstring) (r double big)))))

  (define-request (config)
    17)

  )
;
;(define (on-stream m)
;  (write m)
;  (newline)
;  (printf "Received stream~%"))
;
;(import protocol)
;(use posix)
;
;(define sess (connect 7772 on-stream: on-stream))
;
;; (define-request (eadd #!key m r I btom-rq x q corporeal)
;(define (unwrap-added x)
;  (if (not (eq? (vector-ref x 0) 'ADDED))
;    (abort "Expected ADDED result.")
;    (vector-ref x 1)))
;
;(define stationary (unwrap-added (eadd m: 1 r: 0.1 I: '(1 1 1) btom-rq: '(1 0 0 0) x: '(0 0 0) q: '(1 0 0 0) corporeal: #f)))
;(define moving (unwrap-added (eadd m: 1 r: 0.1 I: '(1 1 1) btom-rq: '(1 0 0 0) x: '(1 0 0) q: '(1 0 0 0) corporeal: #f)))
;(eset moving 'v #(-0.2 0 0))
;(run)
;(ctadd stationary moving)
;(poll 1)
;(write (eget moving 'x))
;
;(disconnect)
;
; vim: set lispwords+=define-request :
