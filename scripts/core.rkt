#lang racket

;; hashing function
(define hash-initial-fnv 2166136261)
(define hash-fnv-multiple 16777619)
(define 0xFFFFFFFF 4294967295)

(define (string-hash name)
  (define result 2166136261)
  (for ([i name])
    (set! result (bitwise-and (* (bitwise-xor result (char->integer i)) hash-fnv-multiple) 0xFFFFFFFF))
    )
  result
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

;; starting code
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
  )

;; =================
;; write script here

(simple-script "zelda"
               [animate "zelda" "zelda-idle-stand-01"]
               )

;; end of the script
;; =================