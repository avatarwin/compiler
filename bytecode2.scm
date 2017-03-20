(use svec)

(define-record-type <bytecode-chunk>
  (-make-bytecode-chunk)
  bytecode-chunk?
  (symbol-name  bytecode-symbol  bytecode-symbol!)
  (length       bytecode-length  bytecode-length!)
  (code         bytecode-code    bytecode-code!))

(define (make-bytecode seq)
  (let ((bs (-make-bytecode-chunk)))
    bs ))

(define-record-type <bytecode-machine>
  (-make-bytecode-machine)
  bytecode-machine?
  (endianness bm-endian    bm-endian!)
  (registers  bm-registers bm-registers!)
  (pc         bm-pc        bm-pc!)
  (sp         bm-sp        bm-sp!)
  (flags      bm-flags     bm-flags!)
  (memory     bm-memory    bm-memory!))

(define (bm-reset m . opts)
  (let-optionals opts ((type 'soft))
                 (if (equal? type 'hard)
                     (begin
                       (bm-pc! m 0)
                       (bm-registers! m (make-vector 16 0))
                       (bm-flags! m 0)                       
                       (bm-sp! m #xFFFFFFFF))
                     (begin
                       (bm-pc! m 0)))))

(define (make-bytecode-machine)
  (let ((m (-make-bytecode-machine)))
    (bm-endian! m 'little)
    (bm-memory! m (make-svec 0))    
    (bm-reset m 'hard)
    m ))

(define (int->list data size endian)
  (let ((%ior  bitwise-ior)
        (%and  bitwise-and)
        (%shift arithmetic-shift)
        (v1 0)
        (v2 0)
        (v3 0)
        (v4 0))
    (set! v1 (%and data #xFF))         
    (when (member size '(2 4))
      (set! v2 (%shift (%and data #xFF00) -8)))
    (when (= size 4)
      (set! v3 (%shift (%and data #xFF0000) -16))
      (set! v4 (%shift (%and data #xFF000000) -24)))
    (case size
      ((4) (if (equal? endian 'little)
               (list v1 v2 v3 v4)
               (list v4 v3 v2 v1)))
      ((2) (if (equal? endian 'little)
               (list v1 v2)
               (list v2 v1)))
      (else (list v1)))))

(define (list->int v endian)
  (let ((size  (length v))
        (%ior  bitwise-ior)
        (%and  bitwise-and)
        (%shift arithmetic-shift)
        (ret   0))
    (case size
      ((4)
       (if (equal? endian 'little)
           (%ior (%and (first v) #xFF)
                 (%shift (%and (second v) #xFF) 8)
                 (%shift (%and (third  v) #xFF) 16)
                 (%shift (%and (fourth v) #xFF) 24))
           (%ior (%and (fourth v) #xFF)
                 (%shift (%and (third v) #xFF) 8)
                 (%shift (%and (second  v) #xFF) 16)
                 (%shift (%and (first v) #xFF) 24))))          
      ((2)
       (if (equal? endian 'little)
           (%ior (%and (first v) #xFF)
                 (%shift (%and (second v) #xFF) 8))
           (%ior (%and (second v) #xFF)
                 (%shift (%and (first v) #xFF) 8))))
      (else
       (%and (first v) #xFF)))))
        
        

(define (bm-write m addr data size)
  (let* ((b (int->list data size (bm-endian m)))
         (mem (bm-memory m))
         (write (lambda (offset)
                  (svec-set! mem (+ addr offset) (list-ref b offset)))))
    (for-each write (iota size))))

(define (bm-read m addr size)
  (let* ((mem  (bm-memory m))
         (rd   (lambda (off)
                 (svec-ref mem (+ addr off))))
         (b    (map rd (iota size))))
    (list->int b (bm-endian m))))

