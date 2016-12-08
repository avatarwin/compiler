(define (make-error e m)
  (vector 'error e m))

(define (is-error? e)
  (and (vector? e) (eq? 'error (vector-ref e 0))))

(define (make-number n)
  (vector 'number n))

(define (make-string s)
  (vector 'string s))

(define (make-bool b)
  (vector 'bool b))

(define (make-atom s)
  (vector 'atom s))

(define (make-list . vals)
  (vector 'list vals))

(define (make-dotted x xs)
  (vector 'dotted x xs))

(define (make-primfunc f n)
  (vector 'primitive-func f n))

(define (primitive-helper-function fun narary args)
  (cond ((= narary 1)
			(apply fun (list (extract-value (car args)))))
		  ((= narary 2)
			(apply fun (list (extract-value (car args))
								  (extract-value (cdr args)))))))

(define (make-func a v body)
  (vector 'function a v body))

(define (make-environment)
  (list (cons #f #f)))

(define (env-set! e n v)
  (let ((sym (assoc n e)))
	 (if (not sym)
		  (make-error 'not-found "Symbol not found")
		  (begin
			 (set-cdr! sym v)
			 e))))

(define (env-define! e n v)
  (let ((sym (assoc n e)))
	 (if sym
		  (set-cdr! sym v)
		  (append! e (list (cons n v))))))



(define (env-lookup e n)
  (let ((sym (assoc n e)))
	 (if (and sym (pair? sym))
		  (cdr sym)
		  (make-error 'not-found "Symbol not found"))))

