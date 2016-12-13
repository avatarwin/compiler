;;;
;;; Bytecode
;;;
;;; (upper)
;;; byte     function
;;; 0        nop                           (full word = 0)
;;; 1        breakpoint
;;; 2        load immediate to register
;;; 3        load address   to register
;;; 4        store register to address
;;; 5        add immediate to register
;;; 6        divide register by immediate
;;; 7        multiply register by immediate
;;; 8        jump relative
;;; 9        call
;;; 10       push register
;;; 11       pop  register
;;; 12       return

(use srfi-41)

(define +registers+
  (make-vector 16 0))

(define +flags+
  #x00)

(define +pc+ 0)

(define +memory+ (make-sparse-vector 0))

(define (load-memory base data)
  (if  (not (null? data))
         (begin
           (sparse-vector-set! +memory+ base (car data))
           (load-memory (add1 base) (cdr data)))))

(define (load-bytecode filename)
  (port->stream (open-input-file filename)))

(define (make-jump-instructions)
  (let ((bits  (list '(#x1 z)
                     '(#x9 nz)
                     '(#x2 c)
                     '(#xA nc)))
        (sa (lambda (x y)
              (string->symbol
               (string-append (symbol->string x)
                              (symbol->string y))))))
    (map (lambda (x)
           (list (+ #x80 (first x))
                 (sa 'jr- (second x)) 4)) bits)))

(define bytecode-instructions
  (list '(0 nop 0)
        '(1 breakpoint 0)
        '(2 loadi 5)
        '(3 loada 3)
        '(4 storem 5)
        '(5 addi 5)
        '(6 divi 5)
        '(7 muli 5)
        '(9 call 0)
        '(10 push 1)
        '(11 pop  1)
        '(12 ret  0)
        '(13 movrr' 2)
        '(14 addrr' 2)
        (make-jump-instructions)
       ))

(define (lookup-bytecode bc)
  (find (lambda (x) (eqv? (first x) bc)) bytecode-instructions)
  )

(define (flags-test dat)
  (if (= 0 dat)
      1
      0))

(define (read-noinc)
  (print  (format #f "read/ni ~A" +pc+))
  (sparse-vector-ref +memory+ +pc+))

(define (read-and-inc)
  (format #t "read/i ~A" +pc+)
  (let ((ret (sparse-vector-ref +memory+ +pc+)))
    (set! +pc+ (add1 +pc+))
    ret))

(define (read-varlen len)
  (print (format #f "varlen mode = ~A" len))
  (case len
    ((#x00 #x80)
     (read-and-inc))
    
    ((#xC0)
     (bitwise-ior (arithmetic-shift (read-and-inc) 8)
                  (read-and-inc)))
    ((#xE0)
     (bitwise-ior (arithmetic-shift (read-and-inc) 24)
                  (arithmetic-shift (read-and-inc) 16)                  
                  (arithmetic-shift (read-and-inc) 8)
                  (read-and-inc)))))

(define (process-loadi)
  (let* ((register   (bitwise-and (read-noinc) #x1F))
         (mode       (bitwise-and (read-and-inc) #xE0))
         (data       (read-varlen mode)))
    (print (format #f "loadi : ~A ~A ~A" register mode data))
    (vector-set! +registers+ register data)
    (set! +flags+  (flags-test data))))


;;; 'loada' is a bit of a misnomer, it's really just a 16bit (32bit?) load immediate
  


(define (process-loada)
  (let* ((register   (bitwise-and (read-and-inc) #x1F))
         (data       (read-varlen #xC0)))
    (print (format #f "loada : ~A ~A" register data))
    (vector-set! +registers+ register data)))


(define (bytecode-parse)
  (let* ((head (sparse-vector-ref +memory+ +pc+))
         (res  (if head (lookup-bytecode head) #f)) )
    (if res
        (begin
          (set! +pc+ (add1 +pc+))
          (case (second res)
            ((nop)
             (begin
               (display "nop...")))
            ((loadi)
             (begin
               (process-loadi)))
            ((loada)
             (begin               
               (process-loada))))))))

(define (reset-cpu)
  (set! +pc+ 0))
