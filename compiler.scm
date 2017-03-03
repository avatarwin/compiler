(use srfi-1)
(use comparse)

(define (na/read)
  (read))

(define (na/eval string)
  (parse parse-expr string))

(define (na/print obj)
  (case (car obj)
    ((number-integer)
     (print (cadr obj)))
    ((number-real)
     (print (cadr obj)))
    ((char)
     (print (string-append "#\\" (cadr obj))))
    ))

                            
    
