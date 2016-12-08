
(define (digits n)
  (as-number (repeated digit n)))
(define date-year (digits 4))
(define date-month (digits 2))
(define date-day (digits 2))

(define date-date
  (sequence* ((day date-day)
				  (_ (is #\-))
				  (month date-month)
				  (_ (is #\-))
				  (year date-year))
	 (result (list 'date day month year))))

(define time-hour
  (bind (digits 2)
		  (lambda (h)
			 (if (<= 0 h 23)
				  (result h)
				  fail))))

(define time-minute
  (bind (digits 2)
		  (lambda (m)
			 (if (<= 0 m 59)
				  (result m)
				  fail))))

(define time-seconds time-minute)

(define time-time
  (sequence* ((hour time-hour)
				  (_ (is #\:))
				  (minutes time-minute)
				  (_ (is #\:))
				  (seconds time-seconds))
				 (result (list 'time hour minutes seconds))))


(define time-or-date
  (any-of date-date
			 time-time))


(define digit
  (in char-set:digit))

(define symbol
  (in char-set:symbol))

(define letter
  (in char-set:letter))

(define parse-atom
  (sequence* ((head  (any-of letter symbol))
				  (rest  (zero-or-more (any-of letter digit symbol))))
				 (result (list 'atom (append (list head) rest)))))

(define parse-string
  (sequence* ((_ (is #\"))
				  (c (as-string (zero-or-more (satisfies (lambda (x)
														  (not (eqv? x #\")))))))
				  (_ (is #\")))
				 (result (list 'string c))))

(define parse-expr
  (any-of parse-atom
			 parse-string))
