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

;; ***** C-TYPES AND CODE STRUCT GENRATION *****

;; basic c-types
(define (int8 value) (cons 'c-type-int8 value))
(define (int16 value) (cons 'c-type-int16 value))
(define (int32 value) (cons 'c-type-int32 value))
(define (float value) (cons 'c-type-float value))
(define (symbol value) (cons 'c-type-symbol value))

(define (any-fundamental-type? value)
  (cond
    [(equal? (car value) (car (int8 0))) #t]
    [(equal? (car value) (car (int16 0))) #t]
    [(equal? (car value) (car (int32 0))) #t]
    [(equal? (car value) (car (float 0))) #t]
    [(equal? (car value) (car (symbol 'zero))) #t]
    [else #f]
    )
  )

(define (c-type? value) (if (pair? value) (any-fundamental-type? value) #f))

(define (to-variant-c-type value)
  (cond
    [(exact-integer? value) int32]
    [(real? value) float]
    [(symbol? value) symbol]
    [(list? value) int32] ; type doesn't matter when it's list
    [else (assert #f (format "unknown type for variant ~v" value))]
    )
  )

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

;; type property macro for particular property access (name, C source code, etc.)
(define-syntax c-type-prop
  (syntax-rules ()
    [(_ proc index)
     (define (proc type) (list-ref type index))
     ]
    )
  )

(c-type-prop c-type-constant 0) ;; used to know that it's a type, used in c-type?
(c-type-prop c-type-name 1) ;; name as symbol
(c-type-prop c-type-source-code 2) ;; C source code as string
(c-type-prop c-type-to-byte-procedure 3) ;; save value content to bytes
(c-type-prop c-type-sizeof 4) ;; size of type in bytes

(define c-type-constant-symbol 'c-type-constant-symbol)

(define-syntax deftype
  (syntax-rules ()
    [(_ type-name ((prop-name prop-type) ...))
     (define type-name
       (list
        ;; [0] - type constant symbol to indicate that it's a c-type
        c-type-constant-symbol
        ;; [1] - type name as symbol
        (quote type-name)
        ;; [2] - C code as string
        (string-append
                    (format "typedef struct ~s {\n" (quote type-name))
                    (format "\t~s ~s;\n" (quote prop-type) (quote prop-name))
                    ...
                    (format "} ~s;\n" (quote type-name)))
        ;; [3] - to-bytes procedure
        ""
        ;; [4] - size in bytes
        (+ (c-type-sizeof prop-type) ...)
       ))])
  )

;; ***** SAVING BINARY BUFFERS AND SEGMENTS OF DATA *****

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
     [(_ func-name (arg0 type0) ...)
     (define (func-name arg0 ... . var-args)
      (list
       ;; header of the operation
       (script-op-header
        (quote func-name)
        0 ; flags
        (+ (length (list (quote arg0) ... )) (length var-args)))

       ;; args
       (if (empty? var-args)
           (list
            (variant type0 arg0)
            ...
            )
           (list
            (variant type0 arg0)
            ...
            (for/list ([e (in-list var-args)])
              (variant (to-variant-c-type e) e)
              )
            )
           )
       
       ;; variadic args
       
       ))]
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

;; segment header
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

;; save data to file
(define (bytes-to-file path bytes)
  (let ((f (open-output-file path #:mode 'binary #:exists 'replace)))
    (write-bytes bytes f) ;; write bytes into file f
    (close-output-port f)
    )
  )

;; ***** FS LAMBDA SCRIPTS *****
(define (fs-lambda name code)
  (fs-segment op-pre-lambda name code)
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

;; todo: find a better way
(define (range-values n)
  (cond
    [(= n 1) (values 0)]
    [(= n 2) (values 0 1)]
    [(= n 3) (values 0 1 2)]
    [(= n 4) (values 0 1 2 3)]
    [(= n 5) (values 0 1 2 3 4)]
    [(= n 6) (values 0 1 2 3 4 5)]
    [(= n 7) (values 0 1 2 3 4 5 6)]
    [(= n 8) (values 0 1 2 3 4 5 6 7)]
    [(= n 9) (values 0 1 2 3 4 5 6 7 8)]
    [(= n 10) (values 0 1 2 3 4 5 6 7 8 9)]
    [(= n 11) (values 0 1 2 3 4 5 6 7 8 9 10)]
    )
  )

(define-syntax defenum
  (syntax-rules ()
    [(_ enum-type-name (enum-value ...))
     (begin
       (define-values (enum-value ...) (range-values (length (list (quote enum-value) ...))))
       (define (enum-type-name x) x)
     )
    ]))

(defenum animate-arg
  (
   fade-in-curve   ;; (anim-curve-type :default (anim-curve-type uniform-s)) what kind of blend curve to use when fading in
   fade-in-sec     ;; (float :default 0.0) cross fade time in seconds
   fade-out-curve  ;; (anim-curve-type :default (anim-curve-type uniform-s)) what kind of blend curve to use when fading out
   fade-out-sec    ;; (float :default 0.0) cross fade time in seconds, used for igc
   ik-mode         ;; (anim-ik-mode :default (anim-ik-mode none))
   layer           ;; (animate-layer :default (animate-layer full-body))
   layer-name      ;; (symbol :default 0) specify exactly layer name instead of using full-body or partial
   ))

(defenum anim-curve-type
  (
   uniform-s
   linear
   ))

(defenum animate-layer
  (
   full-body
   partial
   ))

(defenum anim-ik-mode
  (
   none
   legs
   ))

;; ***** ASSET DESCRIPTIONS *****

(define asset-type-mesh 1)

(define (mesh-chunk path albedo-texture-name)
  (list path
        (symbol albedo-texture-name)
        )
  )

;; mesh
(define (mesh name path . chunks)
  (list (int16 asset-type-mesh) ; resource type id
        (int16 (length chunks)) ; num chunks
        (symbol name) ; unique mesh name to identify in the engine
        path ; path to the source file
        chunks)
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
(define-c-function go (state-name symbol))

;; --- end of import ---

;; simple script example

(define-state-script 'zelda
  (state 'idle
         (on (start)
             [animate 'self 'zelda-idle-stand-relaxed
                      (animate-arg fade-in-sec) 0.3]
             [animate 'self 'zelda-upper-wind-protect
                      (animate-arg fade-in-sec) 0.3
                      (animate-arg layer) (animate-layer partial)]
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