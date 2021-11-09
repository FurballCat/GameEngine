#lang racket

;; global compilation flags
(define use-debug-compilation #f)

(define (assert value message)
  (if value #t (error (string-append "FurScript Error: " message))))

;; basic c-types
(define (int8 value) (cons 'c-type-int8 value))
(define (int32 value) (cons 'c-type-int32 value))
(define (float value) (cons 'c-type-float value))

;; conversion of basic c-types to bytes
(define (c-type-to-bytes value)
  (assert (pair? value) "cannot convert non-pair typed value to bytes")
  (cond
    [(equal? (car value) (car (int8 0))) (integer->integer-bytes (cdr value) 1 #f)]
    [(equal? (car value) (car (int32 0))) (integer->integer-bytes (cdr value) 4 #f)]
    [(equal? (car value) (car (float 0))) (real->floating-point-bytes (cdr value) 4 #f)]
    [else (assert #f (format "unknown c-type: ~v" value))]
    )
  )

;; hashing function, same as on c side
(define hash-initial-fnv 2166136261)
(define hash-fnv-multiple 16777619)
(define 0xFFFFFFFF 4294967295)

(define op-pre-func-call (int8 0))
(define op-pre-func-arg (int8 1))

(define (string-hash name)
  (define result 2166136261)
  (for ([i (if (symbol? name) (symbol->string name) name)])
    (set! result (bitwise-and (* (bitwise-xor result (char->integer i)) hash-fnv-multiple) 0xFFFFFFFF))
    )
  (if use-debug-compilation name (int32 result))
  )

;; variant - any variable with allowed type, allows for easy communication with c, limited to 4 bytes for now
(define (variant expected-type value)
   (cond
     [(list? value) value] ;; this is a nested function call
     [else (list
            op-pre-func-arg
            (expected-type value))] ;; this is a single value argument
     )
 )

;; script callback header (call to function from look-up table of native functions)
;; always the same size (12 bytes at the moment)
(define (script-op-header op-code flags num-args)
  (list
   op-pre-func-call
   (string-hash op-code) ;; hash of the function name in look-up table on c-side
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
(define (write-code code file)
  (cond
    ;; if this is the leaf pair - write to file
    [(and (pair? code) (not (pair? (car code)))) (write-bytes (c-type-to-bytes code) file)]
    ;; otherwise - keep iterating
    [else
     (cond
       [(empty? (cdr code)) (write-code (car code) file)]
       [else
        (write-code (car code) file)
        (write-code (cdr code) file)
        ]
       )
     ]
    )
  )

;; simple script buffer saved to single *.fs file
(define (simple-script name . code) ;; data is already in form of bytecode
  (let ((f (open-output-file (string-append (symbol->string name) ".fs") #:mode 'binary #:exists 'replace)))
    (write-code code f) ;; write code bytes into f file
    (close-output-port f)
    )
  (printf "compiled simple script ~a.fs\n" name)
 )

;; =================
;; write script here

;; --- import native c functions ---

;; (animate 'object-name 'anim-name ...), add variadic args (...) by: (animate-arg enum1) value1 (animate-arg enum2) value2
(define-c-function animate (object-name string-hash) (anim-name string-hash))
(define-c-function wait-animate (object-name string-hash) (anim-name string-hash))
(define-c-function equip-item (object-name string-hash) (item-name string-hash))
(define-c-function wait-seconds (num-seconds float))
(define-c-function get-variable (object-name string-hash) (var-name string-hash))

;; --- end of import ---

;; simple script example
(simple-script 'idle
                   [wait-animate 'self 'zelda-loco-jump-in-place]
                   [animate 'self [get-variable 'zelda 'idle-anim-name]]
                   [wait-seconds 0.5]
                   [equip-item 'self 'sword]
)

;; end of the script
;; =================