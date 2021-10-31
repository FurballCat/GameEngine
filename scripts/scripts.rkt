#lang racket

;; global compilation flags
(define use-debug-compilation #f)

;; hashing function
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
  (integer->integer-bytes value 4 #f)
  )

(define (variant value)
  (to-bytes-4 value)
  )

(define (script-op-header op-code flags num-args)
  (list
   (to-bytes-4 (string-hash op-code))
   (to-bytes-4 flags)
   (to-bytes-4 num-args)
   )
  )

;; syntax for native c function import: (define-c-function name (arg0 type0) ...)
(define-syntax define-c-function
  (syntax-rules ()
     ;; 1 arg
     [(_ func-name (arg0 type0))
     (define (func-name arg0)
      (list
       ;; header
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
       (variant (type0 arg0))
       (variant (type1 arg1))
       )
      )]
    )
 )

;; simple script buffer saved to single *.fs file
(define (simple-script name . data)
  (let ((f (open-output-file (string-append (symbol->string name) ".fs") #:mode 'binary #:exists 'replace)))
    (for/list ([e (in-list (flatten data))])
      (write-bytes e f))
    (close-output-port f)
    )
  (printf "compiled simple script ~a.fs\n" name)
 )

;; =================
;; write script here

;; import native c functions
(define-c-function animate (object-name string-hash) (anim-name string-hash))

;; simple script example
(simple-script 'idle
                   [animate 'self 'zelda-idle-stand-01]
                   [animate 'self 'zelda-run-0]
)

;; end of the script
;; =================