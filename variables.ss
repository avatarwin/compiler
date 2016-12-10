(use clojurian-syntax)


(define (gen-int-tag bits)
  (case bits
      ((8)  #b0000)
      ((16) #b0001)
      ((32) #b0010)))


(define (emit-header)
  (print "")
  )

(define (emit-int-boilerplate name bits)
  (let ((f (lambda (n str . a)
             (print (apply format (append (list #f str n) a))))))
    (doto name
          (f ".global ~A")
          (f ".type   ~A, @object")
          (f ".size   ~A, ~A" (/ bits 8)))
    #t))

               
(define (emit-integer bits name val)
  (let* [(int-sym  (symbol->string name))
         (int-data (string-append int-sym "_dat"))
         (int-meta (string-append int-sym "_meta"))
         (storage  (case bits
                     ((8) "byte")
                     ((16) "word")
                     ((32) ".long")))
         (f (lambda (str . a)
              (print (apply format (append (list #f str ) a)))))]
    
    (print "         .section bss")
    (emit-int-boilerplate int-sym bits)
    (emit-int-boilerplate int-meta bits)

    (f ".~A:     .~A ~A" int-data storage val)
    (f ".~A:     .byte ~A  /* tag */" int-meta (gen-int-tag bits))
    (f "         .byte 0   /* gc data */")))



(call-with-output-file "/home/nicola/src/avr-tests/output.s"
  (lambda (p)
    (parameterize ((current-output-port p))
      (emit-integer 16 'test 0)
      (emit-integer 8  'test2 0))
    (close-output-port p)))
