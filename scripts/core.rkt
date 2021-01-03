#lang racket

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

;; animation functions
(define (animate object-name anim-name)
  (list
   (write-data-name-pair data-function-begin-id "animate")
   (write-data-name-single object-name)
   (write-data-name-single anim-name)
   (write-data-name-single data-function-end-id)
   )
  )

;; simple script
(define data-script-begin-id "__scriptbegin")
(define data-script-end-id "__scriptend")

(define (simple-script name code)
  (write-to-file
   (flatten (list
             (write-data-name-pair data-script-begin-id name)
             code
             (write-data-name-single data-script-end-id)
             )
    )
   (string-append name ".txt")
   #:mode 'text
   #:exists 'replace
   )
   (printf "compiled simple script ~a.txt\n" name)
  )

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

;; =================
;; write script here

(simple-script "zelda"
               [animate "zelda" "zelda-idle-stand-01"]
               )

(define-state-script "test"
  (state "init"
         (on begin
             [animate "zelda" "zelda-idle-stand-01"]
             )
         )
  )

;; end of the script
;; =================