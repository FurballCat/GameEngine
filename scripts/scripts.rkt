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
  (for ([i name])
    (set! result (bitwise-and (* (bitwise-xor result (char->integer i)) hash-fnv-multiple) 0xFFFFFFFF))
    )
  (if use-debug-compilation name result)
  )

;; data writing - result is a list
(define data-function-begin-id "__funcbegin")
(define data-function-end-id "__funcend")

(define (write-data-name-single param)
  (string-hash param)
  )

(define (write-data-name-pair param value)
  (list (string-hash param) (string-hash value))
  )

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
    "animate"
    (if (equal? object-name "self") 1 0)
    2)

   ;; args
   (variant (string-hash object-name))
   (variant (string-hash anim-name))
   )
  )

;; simple script
(define data-script-begin-id "__scriptbegin")
(define data-script-end-id "__scriptend")

;; state
(define begin (write-data-name-single "__begin"))

(define (on handle code)
  (list
   (write-data-name-single "__eventhandlebegin")
   handle
   code
   (write-data-name-single "__eventhandleend")
   )
  )

(define (state name event-handlers)
  (list
   (write-data-name-pair "__statebegin" name)
   event-handlers
   (write-data-name-single "__stateend")
   )
  )

;; state script
(define (define-state-script name states)
  (write-to-file
   (flatten (list
             (write-data-name-pair "__statescriptbegin" name)
             states
             (write-data-name-single "__statescriptend")
             )
    )
   (string-append name ".txt")
   #:mode 'text
   #:exists 'replace
   )
   (printf "compiled state script ~a.txt\n" name)
  )

(define (simple-script name . data)
  (let ((f (open-output-file (string-append name ".fs") #:mode 'binary #:exists 'replace)))
    (for/list ([e (in-list (flatten data))])
      (write-bytes e f))
    (close-output-port f)
    )
  (printf "compiled simple script ~a.fs\n" name)
 )

;; =================
;; write script here

(define-state-script "test"
  (state "init"
         (on begin
             [animate "zelda" "zelda-idle-stand-01"]
             )
         )
  )

(simple-script "idle"
                   [animate "self" "zelda-idle-stand-01"]
                   [animate "self" "zelda-idle-stand-01"]
)

;; end of the script
;; =================