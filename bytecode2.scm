;;;; Bytecode2.scm
;;;;
;;;; Second version of the bytecode VM

(use clojurian-syntax)
(use svec)

(define +address-size+ 4)

(define-record-type <bytecode-sequence>
  (-make-bytecode-sequence)
  bytecode-sequence?
  (symbol-name   bytecode-symbol  bytecode-symbol!)
  (length        bytecode-length  bytecode-length!)
  (code          bytecode-code    bytecode-code!))

(define (make-bytecode seq)
  (let  [(bs (-make-bytecode-sequence))]
    (bytecode-symbol! bs 'undef)
    (bytecode-length! bs (length seq))
    (bytecode-code! bs seq)))

(define-record-type <bytecode-machine>
  (-make-bytecode-machine)
  bytecode-machine?
  (registers   bm-registers   bm-registers!)
  (pc          bm-pc          bm-pc!)
  (sp          bm-sp          bm-sp!)
  (flags       bm-flags       bm-flags!)
  (memory      bm-memory      bm-memory!))

(define (bm-reset m . opts)
  (let-optionals opts [(type 'soft)]
                 (cond
                  ((eq? type 'hard)
                   (begin
                     (bm-pc! m 0)
                     (bm-registers! m (make-vector 16 0))
                     (bm-flags! m 0)
                     (bm-sp! m #xFFFFFFFF)))
                  (else
                   (bm-pc! m 0)))))

(define (make-bytecode-machine)
  (let [(m  (-make-bytecode-machine))]
    (bm-reset m 'hard)
    m))

(define (memory-write-word mem addr val size)
  (let ((%ior  bitwise-ior)
        (%and  bitwise-and)
        (%shift arithmetic-shift))
  (case size
    ((4)
     (svec-set! mem addr (%and val #xFF))
     (svec-set! mem (+ addr 1) (%and (%shift val -8) #xFF))
     (svec-set! mem (+ addr 2) (%and (%shift val -16) #xFF))
     (svec-set! mem (+ addr 3) (%and (%shift val -24) #xFF)))
    ((2)
     (svec-set! mem addr (%and val #xFF))
     (svec-set! mem (+ addr 1) (%and (%shift val -8) #xFF)))
    ((1)
     (svec-set! mem addr (%and val #xFF)))
    )))

(define (memory-read-word mem addr size)
  (let [(%ior  bitwise-ior)
        (%and  bitwise-and)
        (%shift arithmetic-shift)]
    (case size
      ((4)
       (%ior (%shift (svec-ref mem (+ addr 3)) 24)
             (%shift (svec-ref mem (+ addr 2)) 16)
             (%shift (svec-ref mem (+ addr 1)) 8)
             (svec-ref mem addr)))
      ((2)
       (%ior (%shift (svec-ref mem (+ addr 1)) 8)
             (svec-ref mem-addr)))
      ((1)
       (svec-ref mem addr)))))
