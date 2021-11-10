#lang racket

;; assert triggered while running racket program
(define (assert value message)
  (if value #t (error (string-append "FurScript Error: " message))))

;; hashing function, same as on c side
(define hash-initial-fnv 2166136261)
(define hash-fnv-multiple 16777619)
(define 0xFFFFFFFF 4294967295)

(define (string-hash name)
  (define result 2166136261)
  (for ([i (if (symbol? name) (symbol->string name) name)])
    (set! result (bitwise-and (* (bitwise-xor result (char->integer i)) hash-fnv-multiple) 0xFFFFFFFF))
    )
  result
  )

;; basic c-types
(define (int8 value) (cons 'c-type-int8 value))
(define (int16 value) (cons 'c-type-int16 value))
(define (int32 value) (cons 'c-type-int32 value))
(define (float value) (cons 'c-type-float value))
(define (symbol value) (cons 'c-type-symbol value))

;; conversion of basic c-types to bytes
(define (c-type-to-bytes value)
  (assert (pair? value) "cannot convert non-pair typed value to bytes")
  (cond
    [(equal? (car value) (car (int8 0))) (integer->integer-bytes (cdr value) 1 #f)]
    [(equal? (car value) (car (int16 0))) (integer->integer-bytes (cdr value) 2 #f)]
    [(equal? (car value) (car (int32 0))) (integer->integer-bytes (cdr value) 4 #f)]
    [(equal? (car value) (car (float 0))) (real->floating-point-bytes (cdr value) 4 #f)]
    [(equal? (car value) (car (symbol 'zero))) (integer->integer-bytes (string-hash (cdr value)) 4 #f)]
    [else (assert #f (format "unknown c-type: ~v" value))]
    )
  )

;; prefixes to data
(define op-pre-func-call 1) ;; single FS script native function call
(define op-pre-func-arg 2) ;; single FS script function argument in form of variant
(define op-pre-lambda 3) ;; full script lambda (sequence of function calls)
(define seg-id-state 4) ;; segment of state (includes multiple (on (event X))
(define seg-id-state-on-event 5) ;; segment of single (on (event X))
(define seg-id-state-track 6) ;; segment of single track within (on (event X) (track 'name) ...)
(define seg-id-state-script 7) ;; segment of entire state script

;; variant - any variable with allowed type, allows for easy communication with c, limited to 4 bytes for now
(define (variant expected-type value)
   (cond
     [(list? value) value] ;; this is a nested function call
     [else (list
            (int8 op-pre-func-arg)
            (expected-type value))] ;; this is a single value argument
     )
 )

;; script callback header (call to function from look-up table of native functions)
;; always the same size (12 bytes at the moment)
(define (script-op-header op-code flags num-args)
  (list
   (int8 op-pre-func-call)
   (symbol op-code) ;; hash of the function name in look-up table on c-side
   (int32 flags) ;; use flags for some stuff like calling function on 'self
   (int32 num-args) ;; number of arguments is limited on c side (20 when I last wrote that)
   )
  )

;; syntax for native c function import: (define-c-function name (arg0 type0) ...)
;; using a c function call in script is basically outputting bytecode for that call
(define-syntax define-c-function
  (syntax-rules ()
     ;; 1 arg function
     [(_ func-name (arg0 type0))
     (define (func-name arg0)
      (list
       ;; header of the operation
       (script-op-header
        (quote func-name)
        (if (equal? arg0 'self) 1 0) ;; mark calls for 'self objects for quicker look-up
        1)

       ;; args
       (variant type0 arg0)
       )
      )]
     ;; 2 arg
     [(_ func-name (arg0 type0) (arg1 type1))
     (define (func-name arg0 arg1)
      (list
       ;; header
       (script-op-header
        (quote func-name)
        (if (equal? arg0 'self) 1 0) ;; mark calls for 'self objects for quicker look-up
        2)

       ;; args
       (variant type0 arg0) ;; type0 might be string-hash function that will hash arg0, generally type0 is like type constructor
       (variant type1 arg1)
       )
      )]
    )
 )

;; write list of pairs of (type value) code into file - recursive iteration
(define (lambda-to-bytecode code file)
  (cond
    ;; if this is the leaf pair - write to file
    [(and (pair? code) (not (pair? (car code)))) (write-bytes (c-type-to-bytes code) file)]
    ;; otherwise - keep iterating
    [else
     (cond
       [(empty? (cdr code)) (lambda-to-bytecode (car code) file)]
       [else
        (lambda-to-bytecode (car code) file)
        (lambda-to-bytecode (cdr code) file)
        ]
       )
     ]
    )
  )

;; gets lambda code list and converts to stream of bytes (p)
(define (code-to-bytes code)
  (let ((p (open-output-bytes)))
    (lambda-to-bytecode code p)
    (close-output-port p)
    (get-output-bytes p)
    )
  )

;; lambda (sequence of script function calls) header
(define (fs-segment-header id name length)
  (list
   (int8 id) ;; gives info what's the segment type
   (int8 0) ;; padding
   (int16 length) ;; number of bytes in segment, so it's easy to jump in blob of bytes in engine
   (symbol name) ;; name of the segment that uniquely defines it in blob scope (single compiled .fs file)
   )
  )

;; convert code to lambda bytecode
(define (fs-segment segment-id name code)
  (let ((b (code-to-bytes code)))
      (cons (fs-segment-header segment-id name (bytes-length b))
            code)
  )
)

(define (fs-lambda name code)
  (fs-segment op-pre-lambda name code)
  )

;; save data to file
(define (bytes-to-file path bytes)
  (let ((f (open-output-file path #:mode 'binary #:exists 'replace)))
    (write-bytes bytes f) ;; write bytes into file f
    (close-output-port f)
    )
  )

;; simple script buffer saved to single *.fs file
(define (simple-script name . code) ;; data is already in form of bytecode
  (bytes-to-file (string-append (symbol->string name) ".fs") (fs-lambda name code))
  (printf "compiled simple script ~a.fs\n" name)
 )

(define (is-func-call-code code)
  (if (and (pair? code) (pair? (car code))) (is-func-call-code (car code)) (= (cdr code) op-pre-func-call))
  )

(define (state state-name . code)
  (fs-segment seg-id-state state-name code)
  )

(define (on evt . code)
  (fs-segment (car evt) (cdr evt) (if (is-func-call-code code) (fs-lambda 'default code) code))
  )

(define (track track-name . code)
  (fs-segment seg-id-state-track track-name code)
  )

(define (start)
  (cons seg-id-state-on-event
        'start)
  )

(define (update)
  (cons seg-id-state-on-event
        'update)
  )

(define (event evt-name)
  (cons seg-id-state-on-event
        evt-name)
  )

;; script state machine
(define (define-state-script script-name . states-data)
  (let ((p (open-output-bytes)))
     (for ([i states-data])
       (write-bytes (code-to-bytes (fs-segment seg-id-state-script script-name i)) p)
     )
     (close-output-port p)
     (bytes-to-file (string-append (symbol->string script-name) ".fs") (get-output-bytes p))
     (printf "compiled state-script ~a.fs\n" script-name)
   )
 )

;; =================
;; write script here

;; --- import native c functions ---

;; (animate 'object-name 'anim-name ...), add variadic args (...) by: (animate-arg enum1) value1 (animate-arg enum2) value2
(define-c-function animate (object-name symbol) (anim-name symbol))
(define-c-function wait-animate (object-name symbol) (anim-name symbol))
(define-c-function equip-item (object-name symbol) (item-name symbol))
(define-c-function wait-seconds (num-seconds float))
(define-c-function get-variable (object-name symbol) (var-name symbol))

;; --- end of import ---

;; simple script example

(define-state-script 'zelda
  (state 'idle
         (on (start)
             [animate 'self 'zelda-funny-pose-3]
             [wait-seconds 1.0]
             [animate 'self 'zelda-funny-pose-4]
         )
  )
  (state 'run
         (on (start)
             [animate 'self 'zelda-loco-run-relaxed]
         )
  )
)

;; end of the script
;; =================