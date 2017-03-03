;;;
;;; Bytecode VM
;;; pseudo-32bit virtual machine intended to be used as a base 'assembler' for the
;;; compiler, which can then be translated to actual real assembler.
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
;;; 13       system

(use fmt)
(use svec)
(use srfi-41)

(define +address-size+ 4)

(define-record-type bytecode-block
  (impl-make-bytecode symbol code signature)
  bytecode?
  (symbol    bytecode-symbol    bytecode-symbol!)
  (code      bytecode-code      bytecode-code!)
  (signature bytecode-signature bytecode-signature!))

(define (make-bytecode-block)
  (impl-make-bytecode 'undef (make-vector 0) '()))


(define-record-type memory-region
  (impl-make-mem-region)
  mem-region?
  (start    memregion-start memregion-start!)
  (end      memregion-end   memregion-end))

(define +memory-pool+ '())
(define +memory-allocated+ '())

(define-record-type machine
  (impl-make-machine)
  machine?
  (registers machine-registers machine-registers!)
  (pc        machine-pc        machine-pc!)
  (sp        machine-sp        machine-sp!)
  (flags     machine-flags     machine-flags!)
  (memory    machine-memory    machine-memory!))

(define (make-machine)
  (let [(m  (impl-make-machine))]
    (machine-pc! m 0)
    (machine-sp! m #xFFFFFFFF)
    (machine-registers! m (make-vector 16 0))
    (machine-memory! m (make-svec 0))
    (machine-flags! m 0)
    m))

(define (machine-load-memory m base data)
  (letrec [(mem (machine-memory m))
           (fn  (lambda (addr x)
                  (if (not (null? x))
                      (begin
                        (svec-set! mem addr (car x))
                        (fn (add1 addr) (cdr x))))))]
    (fn base data)))

(define (machine-reset m)
  (machine-pc! m 0))

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
  (list '(0 nop 0 0)
        '(1 breakpoint 0 0)
        '(2 loadi 5)
        '(3 loadm 3)
        '(4 storem 5)
        '(5 addi 5)
        '(6 divi 5)
        '(7 muli 5)
        '(9 call 4)
        '(10 push 1)
        '(11 pop  1)
        '(12 ret  0)
        '(13 movrr 2)
        '(14 addrr 2)
        '(15 system 4)
        '(16 lookup 4)
        (make-jump-instructions)
       ))

(define (lookup-bytecode bc)
  (find (lambda (x) (eqv? (first x) bc)) bytecode-instructions)
  )

(define (flags-test dat)
  (if (= 0 dat) ;; check zero
      #b1
      #b0))

(define (machine-read-byte/noi m)
  (let [(mem (machine-memory m))
        (pc  (machine-pc m))]
    (svec-ref mem pc)))

(define (machine-read-byte/i m)
  (let* [(mem (machine-memory m))
         (pc  (machine-pc m))
         (res (svec-ref mem pc))]
    (machine-pc! m (add1 pc))
    res))

(define (machine-read-multi m size)
  (case size
    ((1)
     (machine-read-byte/i m))
    ((2)
     (bitwise-ior (arithmetic-shift (machine-read-byte/i m) 8)
                  (machine-read-byte/i m)))
    ((4)
     (bitwise-ior (arithmetic-shift (machine-read-byte/i m) 24)
                  (arithmetic-shift (machine-read-byte/i m) 16)
                  (arithmetic-shift (machine-read-byte/i m) 8)
                  (machine-read-byte/i m)))))

(define (machine-read-multi/a m addr size)
  (case size
    ((1)
     (machine-read-byte/a m addr))
    ((2)
     (bitwise-ior (arithmetic-shift (machine-read-byte/a addr) 8)
                  (machine-read-byte/a (+ addr 1))))
    ((4)
     (bitwise-ior (arithmetic-shift (machine-read-byte/a m addr) 24)
                  (arithmetic-shift (machine-read-byte/a m (+ 1 addr)) 16)
                  (arithmetic-shift (machine-read-byte/a m (+ 2 addr)) 8)
                  (machine-read-byte/a m (+ 3 addr))))))

(define (machine-write-byte/a m addr data)
  (let [(mem (machine-memory m))]
    (svec-set! mem addr data)))

(define (machine-push-byte m data)
  (let [(mem (machine-memory m))
        (sp  (machine-sp m))]
    (svec-set! mem sp (bitwise-and data #xFF))
    (machine-sp! m (- sp 1))
    data))

(define (machine-push-multi m size data)
  (case size
    ((1)
     (machine-push-byte m data))
    ((2)
     (machine-push-byte m (bitwise-and (arithmetic-shift data -8) #xFF))
     (machine-push-byte m (bitwise-and data #xFF)))
    ((4)
     (machine-push-byte m (bitwise-and (arithmetic-shift data -24) #xFF))
     (machine-push-byte m (bitwise-and (arithmetic-shift data -16) #xFF))
     (machine-push-byte m (bitwise-and (arithmetic-shift data -8) #xFF))
     (machine-push-byte m (bitwise-and data #xFF)))))

(define (machine-pop-multi m size)
  (case size
    ((1)
     (machine-pop-byte m))
    ((2)
     (bitwise-ior (arithmetic-shift (bitwise-and (machine-pop-byte m) #xFF) 8)
                  (bitwise-and (machine-pop-byte m) #xFF)))
    ((4)
     (bitwise-ior (arithmetic-shift (bitwise-and (machine-pop-byte m) #xFF) 24)
                  (arithmetic-shift (bitwise-and (machine-pop-byte m) #xFF) 16)
                  (arithmetic-shift (bitwise-and (machine-pop-byte m) #xFF) 8)
                  (bitwise-and (machine-pop-byte m) #xFF)))))

(define (machine-pop-byte m)
  (let* [(mem (machine-memory m))
         (sp  (add1 (machine-sp m)))
         (data (svec-ref mem sp))]
    (machine-sp! m sp)
    data))

(define (machine-write-multi/a m addr size data)
  (case size
    ((1)
     (machine-write-byte/a m addr (bitwise-and data #xFF)))
    ((2)
     (machine-write-byte/a m addr (bitwise-and (arithmetic-shift data -8) #xFF))
     (machine-write-byte/a m (+ addr 1) (bitwise-and data #xFF)))
    ((4)
     (machine-write-byte/a m addr (bitwise-and (arithmetic-shift data -24) #xFF))
     (machine-write-byte/a m (+ addr 1) (bitwise-and
                                         (arithmetic-shift data -16) #xFF))
     (machine-write-byte/a m (+ addr 2) (bitwise-and
                                         (arithmetic-shift data -8) #xFF))
     (machine-write-byte/a m (+ addr 3) (bitwise-and data #xFF)))))


(define (machine-read-byte/a m addr)
  (let [(mem (machine-memory m))]
    (svec-ref mem addr)))

(define (decode-width-encoding c)
  (case (bitwise-and c #x1F)
    ((#x00 #x80) 1)
    ((#xC0) 2)
    ((#xE0) 4)))

(define (decode-register c)
  (bitwise-and c #x1F))

(define (machine-process-loadi m)
  (let* [(mem (machine-memory m))
         (enc-reg (machine-read-byte/i m))
         (regno   (decode-register enc-reg))
         (c       (decode-width-encoding enc-reg))
         (data     (machine-read-multi m c))]
    (vector-set! (machine-registers m) regno data)
    (machine-flags! m (flags-test data))
    m))

(define (machine-process-loadm m)
  (let* [(mem (machine-memory m))
         (enc-reg (machine-read-byte/i m))
         (reg (decode-register enc-reg))
         (c   (decode-width-encoding enc-reg))
         (addr (machine-read-multi m +address-size+))
         (data (machine-read-multi/a m addr c))]
    (print (fmt #f "reading from " (pad-char #\0 (fit/left 4 (num addr 16)))
                " (" (pad-char #\0 (fit/left 2 (num data 16))) ") "
                "to register " (pad-char #\0 (fit/left 2 (num reg)))))
    (vector-set! (machine-registers m) reg data)))

(define (machine-process-storem m)
  (let* [(mem (machine-memory m))
         (enc-reg (machine-read-byte/i m))
         (reg (decode-register enc-reg))
         (c   (decode-width-encoding enc-reg))
         (addr (machine-read-multi m +address-size+))
         (data (vector-ref (machine-registers m) reg))]
    (print (fmt #f "writing register " (pad-char #\0 (fit/left 2 (num reg)))
                " (" (pad-char #\0 (fit/left 2 (num data 16))) ") "
                "to " (pad-char #\0 (fit/left 4 (num addr 16)))))
    (machine-write-multi/a m addr c data)))

(define (machine-process-addi m)
  (let* [(mem (machine-memory m))
         (enc-reg (machine-read-byte/i m))
         (regno   (decode-register enc-reg))
         (c       (decode-width-encoding enc-reg))
         (data     (machine-read-multi m c))
         (rd       (vector-ref (machine-registers m) regno))]
    (vector-set! (machine-registers m) regno (+ rd data))
    (machine-flags! m (flags-test data))
    m))

(define (machine-process-pop m)
  (let* [(enc-reg (machine-read-byte/i m))
         (regno   (decode-register enc-reg))
         (c       (decode-width-encoding enc-reg))
         (data    (machine-pop-multi m c))]
    (print (fmt #f "pop'ed " (num data)))
    (vector-set! (machine-registers m) regno data)
    (machine-flags! m (flags-test data))))

(define (machine-process-push m)
  (let* [(enc-reg (machine-read-byte/i m))
         (regno   (decode-register enc-reg))
         (c       (decode-width-encoding enc-reg))
         (data    (vector-ref (machine-registers m) regno))]
    (machine-push-multi m c data)
    (machine-flags! m (flags-test data))))

(define (machine-process-ret m)
  (let* [(new-pc (machine-pop-multi m +address-size+))]
    (machine-pc! m new-pc)))

(define (machine-process-call m)
  (let* [(new-pc (machine-read-multi m +address-size+))
         (old-pc (machine-pc m))]
    (machine-push-multi m +address-size+ old-pc)
    (machine-pc! m new-pc)))

(define (machine-run-step m)
  (let* [(insn (machine-read-byte/i m))
         (bc   (lookup-bytecode insn))]
    (case (second bc)
      ((nop)    (void))
      ((loadi)  (machine-process-loadi m))
      ((loadm)  (machine-process-loadm m))
      ((storem) (machine-process-storem m))
      ((addi)   (machine-process-addi m))
      ((push)   (machine-process-push m))
      ((pop)    (machine-process-pop m))
      ((call)   (machine-process-call m))
      ((ret)    (machine-process-ret m))
      (else     (print "unimplemented opcode")))))

(define (machine-pstate m)
  (let [(registers (machine-registers m))
        (pc        (machine-pc m))
        (sp        (machine-sp m))
        (flags     (machine-flags m))]
    (print (fmt #f "pc: " (pad-char #\0 (fit/left 8 (num pc 16))) "  "
                "sp: " (pad-char #\0 (fit/left 8 (num sp 16))) "  "
                "flags: " (pad-char #\0 (fit/left 8 (num flags 2)))))
    (print (apply string-append
                  (map
                   (lambda (x)
                     (fmt #f "r" (pad-char #\0 (fit/left 2 (num x)))
                          ": " (pad-char #\0 (fit/left 2
                                 (num (vector-ref registers x) 16)))
                          (if (= x 7) "\n" "  "))) (iota 16))))
    ))

