(define (test my-string)
  (let ((outer-cs (char-set-union char-set:punctuation char-set:symbol)))
	 (map (lambda (ch)
			  (if (char-set-contains? outer-cs ch)
					2
					(if (char-set-contains? char-set:graphic ch)
						 1
						 0)))
			(string->list my-string))))


(define (build my-string)
  (letrec ((start-codes (test my-string))
			  (res   '())
			  (curr  "")
			  (last-c -1)
			  (process (lambda (s c)
							 (if (null? s)
								  (begin
									 (set! res (append res (list curr)))
									 res)
								  (let ((ts (car s))
										  (tc (car c))
										  (rs (cdr s))
										  (rc (cdr c)))
									 (if (eq? tc last-c)
										  (begin
											 (set! curr (string-append curr (string ts)))
											 ;;(display "-")
											 ;;(display curr)
											 ;;(newline)
											 (process rs rc))
										  (begin
											 (if (not (eq? last-c 0))
												  (set! res (append res (list curr)))
												  '())
											 (set! curr (string ts))
											 ;;(display "+")										
											 ;;(display curr)
											 ;;(newline)
											 (set! last-c tc)
											 (process rs rc))))))))
	 (begin
		(process (string->list my-string) start-codes)
		(cdr res))))
												


(define (parse inp)
  (letrec [(broken-input (string->list inp))
			  (emit (lambda (token)
						 (begin
							(display (string-concatenate "emit token: "
																  token
																  "\n")))))
			  (get-code (lambda (ch)
							  (cond ((eq? #\" ch)
										'quote)
								     ((char-set-contains? char-set:punctuation ch)
										'punct)
									  ((char-set-contains? char-set:symbol ch)
										'symbol)
									  ((char-set-contains? char-set:graphic ch)
										'normal)									  
									  (#t
										'whitespace))))
			  (run-string-literal (lambda (x)
											'string-literal))
			  (run-normal-string  (lambda (x) '()))
			  (eat-whitespace     (lambda (x)
											(if (eq? 'whitespace (get-code (first x)))
												 (eat-whitespace (cdr x))
												 'whitespace )))
			 
			  (run-main (lambda (x) (if (null? x)
												 '()
												 (let [(code (get-code (car x)))]
													(cond [(eq? 'quote code)
															 (run-string-literal x)]
															[(eq? 'normal code)
															 (run-normal-string x)]
															[(eq? 'whitespace code)
															 (eat-whitespace x)])))))]
	 (run-main broken-input)))
