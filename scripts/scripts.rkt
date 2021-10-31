#lang racket

(require ecmascript/types)

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

;; data writing - result is a list
(define (variant value)
  (to-bytes-4 value)
  )

(define (to-bytes-4 value)
  (integer->integer-bytes value 4 #f)
  )

(define (script-op-header op-code flags num-args)
  (list
   (to-bytes-4 (string-hash op-code))
   (to-bytes-4 flags)
   (to-bytes-4 num-args)
   )
  )

;; animation functions
(define (animate object-name anim-name)
  (list
   ;; header
   (script-op-header
    'animate
    (if (equal? object-name 'self) 1 0) ;; mark calls for 'self objects for quicker look-up
    2)

   ;; args
   (variant (string-hash object-name))
   (variant (string-hash anim-name))
   )
  )

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

(simple-script 'idle
                   [animate 'self 'zelda-idle-stand-01]
                   [animate 'self 'zelda-idle-stand-01]
)

;; end of the script
;; =================