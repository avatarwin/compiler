;;;
;;
;; IL-asm should roughly mirror the specifications of the target architecture
;; as such we need several IL-<arch> definitions
;;
;; IL-x64
;; registers: 0 - 15 * 64bit
;; 
;; IL-cortex
;; registers: 0 - 12 * 32bit
;;
;; IL-pic
;; registers: 0 - x 8-bit

;; instructions
;; inc
;; dec
;; load
;; store
;; call
;; push
;; pop
;; ret
;;

(define (translate-to-native il-code native-choice)
  #f)

(define (make-il-emu il-machine)
  (lambda (x)
    (run-il-code il-machine x)))


(define-syntax reg
  (syntax-rules (< >)
    ((reg <num>)
     num)))
      
(define (il-machine)
  (let* [(registers (make-u32vector 12 0))
         (memory    (make-u32vector 65526 0))
         (fun (lambda (instr)
                #f))]
    fun))


(define <cpu> (send Object :copy))
(send <cpu> :rename "CPU emulation")
(send <cpu> :answer :setup ()
      (let ((new-registers (make-u32vector 12 0))
            (new-memory    (make-u8vector (* 64 1024) 0)))
        (send self :set 'regs new-registers)
        (send self :set 'mem  new-memory)
        (send self :set 'pc   0)
        (send self :set 'heap-pointer #xFFFF)
        (send self :set 'flags #b00000000)
        (display "CPU initialised\n")
       ))
        
(send <cpu> :answer :load-memory (start data)
      (let ((memory (send self :get 'mem)) ;; we have a reference here, so no need to :set
            (addr   (lambda (o) (+ start o))))
        (for-each (lambda (n)
                    (u8vector-set! memory (addr n) (u8vector-ref data n)))
                  (iota (u8vector-length data)))))

(send <cpu> :answer :step1 ()
      (let* ((memory (send self :get 'mem))
             (regs   (send self :get 'regs))
             (pc     (send self :get 'pc))
             (heap   (send self :get 'heap-pointer))
             (op     (u8vector-ref memory pc)))
        (display (format #f "not implemented yet (OP=~A @~A)\n" op pc))
        (send self :set 'pc (add1 pc))
        ))
