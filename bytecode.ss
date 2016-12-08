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
        '(3 loadm 5)
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

(define (bytecode-parse stream)
  (let* ((head (stream-ref stream 0))
         (res  (if head (lookup-bytecode head) #f)))
    (if (not res)
        stream
        (case (second res)
          ((nop)
           (begin
             (display "nop...")
             (stream-drop 1 stream)))
          ))
    ))
