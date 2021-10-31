#lang racket

;; global compilation flags
(define use-debug-compilation #f)

;; hashing function, same as on c side
(define hash-initial-fnv 2166136261)
(define hash-fnv-multiple 16777619)
(define 0xFFFFFFFF 4294967295)

(define (string-hash name)
  (define result 2166136261)
  (for ([i (if (symbol? name) (symbol->string name) name)])
    (set! result (bitwise-and (* (bitwise-xor result (char->integer i)) hash-fnv-multiple) 0xFFFFFFFF))
    )
  (if use-debug-compilation name result)
  )

;; data writing for script buffer
(define (to-bytes-4 value)
  (integer->integer-bytes value 4 #f) ;; convert integer value to 4 byte representation
  )

;; variant - any variable with allowed type, allows for easy communication with c, limited to 4 bytes for now
(define (variant value)
  (to-bytes-4 value)
  )

;; script callback header (call to function from look-up table of native functions)
;; always the same size (12 bytes at the moment)
(define (script-op-header op-code flags num-args)
  (list
   (to-bytes-4 (string-hash op-code)) ;; hash of the function name in look-up table on c-side
   (to-bytes-4 flags) ;; use flags for some stuff like calling function on 'self
   (to-bytes-4 num-args) ;; number of arguments is limited on c side (20 when I last wrote that)
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
       (variant (type0 arg0))
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
       (variant (type0 arg0)) ;; type0 might be string-hash function that will hash arg0, generally type0 is like type constructor
       (variant (type1 arg1))
       )
      )]
    )
 )

(define-syntax define-c-function-variadic
  (syntax-rules ()
     ;; 1 arg + variadic args
     [(_ func-name (arg0 type0))
     (define (func-name arg0 . var-args)
      (list
       ;; header
       (script-op-header
        (quote func-name)
        (if (equal? arg0 'self) 1 0) ;; mark calls for 'self objects for quicker look-up
        (+ 1 (length var-args)))

       ;; args
       (variant (type0 arg0))
       (for/list ([e (in-list var-args)])
        (variant e))
       )
      )]
     ;; 2 args + variadic args
     [(_ func-name (arg0 type0) (arg1 type1))
     (define (func-name arg0 arg1 . var-args)
      (list
       ;; header
       (script-op-header
        (quote func-name)
        (if (equal? arg0 'self) 1 0) ;; mark calls for 'self objects for quicker look-up
        (+ 2 (length var-args)))

       ;; args
       (variant (type0 arg0))
       (variant (type1 arg1))
       (for/list ([e (in-list var-args)])
        (variant e))
       )
      )]
    )
 )

;; simple script buffer saved to single *.fs file
(define (simple-script name . data) ;; data is already in form of bytecode
  (let ((f (open-output-file (string-append (symbol->string name) ".fs") #:mode 'binary #:exists 'replace)))
    (for/list ([e (in-list (flatten data))]) ;; iterate flat list of bytecode
      (write-bytes e f)) ;; write flat bytes into f file
    (close-output-port f)
    )
  (printf "compiled simple script ~a.fs\n" name)
 )

;; =================
;; write script here

;; --- import native c functions ---

;; (animate 'object-name 'anim-name ...), add variadic args (...) by: (animate-arg enum1) value1 (animate-arg enum2) value2
(define-c-function-variadic animate (object-name string-hash) (anim-name string-hash))

;; --- end of import ---

;; simple script example
(simple-script 'idle
                   [animate 'self 'zelda-idle-stand-01]
                   [animate 'self 'zelda-run-0]
)

;; end of the script
;; =================