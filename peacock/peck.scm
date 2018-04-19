(use srfi-13)
(use posix files)
(use args matchable)

(define cuauv-software (get-environment-variable "CUAUV_SOFTWARE"))
(if (not cuauv-software)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "CUAUV_SOFTWARE must be set to the root of the software repository, with a trailing slash.")
      (exit 1))))

(define peacock-path (string-append cuauv-software "peacock/"))

(define new-template-path (string-append peacock-path "peacock-template.scm"))
(if (not (file-exists? new-template-path))
  (abort (format "Could not find new template file at '~A'." new-template-path)))

(define editor (get-environment-variable "EDITOR"))
(if (not editor)
  (set! editor "nano"))

; Temporarily disable #warning directives until CHICKEN stops using some
; deprecated defines that cause confusing output on compile.
(define csc-options '("-C" "-Wno-cpp"))

(define (has-updown?)
  (receive (in out pid err)
    (process* "which" '("updown"))
    (receive (pid normal code)
      (process-wait pid)
      (and normal (= code 0)))))

(define (ensure-scm-suffix path)
  (if (not (string-suffix? ".scm" path))
    (string-append path ".scm")
    path))

(define opts
  (list (args:make-option (n no-updown) #:none
          "Disable using Updown to collect output lines.")
        (args:make-option (r updown-reset-length) #:optional
          "Updown reset length (see updown --help). Defaults to 5.")
        (args:make-option (h help) #:none
          "Display this text."
          (usage))))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " <command> [options...] [files...]")
      (print "Defined commands: "
             (string-join
               (map (lambda (x) (symbol->string (caar x)))
                    commands) ", ")
             ".")
      (newline)
      (print (args:usage opts))))
  (exit 1))

(define (command-usage command short #!optional long)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " " command " " short)
      (if long
        (print long))
      (exit 1))))

(define (run bin args)
  (process-execute bin args))

(define commands
  `(((interpret i)
     ,(lambda (options operands)
        (define (run-usage #!optional long)
          (command-usage "run" "<file>" long))
        (if (null? operands)
          (run-usage))

        (define file (ensure-scm-suffix (car operands)))
        (if (not (file-exists? file))
          (run-usage (format "Could not find the file '~A'." file)))

        (when (and (has-updown?) (not (assq 'no-updown options)))
          (define updown-reset-length (or (alist-ref 'updown-reset-length options) "5"))
          (define updown-args (list (string-append "--reset-length=" updown-reset-length)
                                    "-xNOTE"))
          (run "updown" (append updown-args (list "--" "csi" "-I" peacock-path "-s" file))))

        (run "csi" (list "-I" peacock-path "-s" file))))
    ((compile c)
     ,(lambda (options operands)
        (define (run-usage #!optional long)
          (command-usage "compile" "<file>" long))
        (if (null? operands)
          (run-usage))

        (define file (ensure-scm-suffix (car operands)))
        (if (not (file-exists? file))
          (run-usage (format "Could not find the file '~A.'" file)))

        (run "csc" (append csc-options (list "-I" peacock-path file)))))
    ((new n)
     ,(lambda (options operands)
        (define (new-usage #!optional long)
          (command-usage "new" "<path>" long))
        (if (null? operands)
          (new-usage))

        (define path (ensure-scm-suffix (car operands)))
        (if (file-exists? path)
          (new-usage (format "A file already exists at path '~A'." file)))

        (file-copy new-template-path path)
        (run editor (list path))))
    ))

(define (command-ref command)
  (let loop ((rest commands))
    (if (null? rest)
      #f
      (match (car rest)
        (((names ...) proc)
         (if (member command names)
           proc
           (loop (cdr rest))))))))

(set-signal-handler! signal/int
  (lambda (sig)
    (exit 1)))

(receive (options operands) (args:parse (command-line-arguments) opts)
  (if (null? operands)
    (usage))
  (let ((c (command-ref (string->symbol (car operands)))))
    (if (not c)
      (usage))
    (c options (cdr operands))))

; vim: set lispwords+=args\:make-option,set-signal-handler! :
