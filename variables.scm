(use coops)

(define-class <environment> ()
  ((symbols accessor: environment-symbols
				initform: '())))

(define (make-environment)
  (make <environment>))

(define (environment-lookup e n)
  (assoc n (environment-symbols e)))

(define (environment-add-sym e n v)
  (let [(old-syms (environment-symbols e))
		  (existing (environment-lookup  e n))]
	 (if (eq? #f existing) 
		  (set! (environment-symbols e)
			 (append old-syms (list (cons n v))))
		  (set-cdr! existing v))))
				  
