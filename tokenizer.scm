(define test-string-1
  "(+ 1 2)")

(define test-string-2
  "(display \"test\"")

;;; Code follows...

(define <whitespace-set> 
  " \n")
(define <symbol-set> 
  "!#@$%^&+-/_")

(define <left-parens-set>
  "([{")

(define <right-parens-set>
  "}])")

(define <quote-character> #\")

(define (is-one-of? ch set)
  (any (lambda (x) (char=? ch x)) (string->list set)))

(define (is-lparens? ch)
  (is-one-of? ch <left-parens-set>))

(define (is-rparens? ch)
  (is-one-of? ch <right-parens-set>))

(define (is-symbol? ch)
  (is-one-of? ch <symbol-set>))

(define (is-whitespace? ch)
  (is-one-of? ch <whitespace-set>))


(define-record-type <navalue>
  (make-navalue type val)
  navalue?
  (type navalue-type navalue-type-set!)
  (val  navalue-val  navalue-val-set!))

(define-record-type <naenv> ;; environment
  (make-naenv symbols)
  naenv?
  (symbols naenv-symbols naenv-symbols-set!))

(define-record-type <token>
  (make-token type value attribs)
  token?
  (type token-type)
  (value token-value)
  (attribs token-attributes))

(define-record-type <input-marker>
  (make-input-marker filename line-number)
  input-marker?
  (filename input-marker-filename input-marker-filename-set!)
  (line-number input-marker-line-number input-marker-line-number-set!))

(define-record-type <tokenizer-state>
  (make-<tokenizer> input-queue output-queue state temp)
  tokenizer?
  (input-queue tokenizer-inqueue tokenizer-inqueue-set!)
  (output-queue tokenizer-outqueue tokenizer-outqueue-set!)
  (state tokenizer-state tokenizer-state-set!)
  (temp tokenizer-temp tokenizer-temp-set!))

(define (make-tokenizer)
  (make-<tokenizer> '() '() 'normal '()))

;; TODO: This should take a filename (source) and line count to allow error reporting
;; this should be inserted into the input stream as an object that is then moved
;; into the 'currently processing' slot in the tokenizer, so that the
;; tokenizer/parser can generate errors
(define (tokenizer-add-input t s)
  (cond [(null? (tokenizer-inqueue t))
			(begin
			  (tokenizer-inqueue-set! t (string->list s))
			  t )]
		  [(list? (tokenizer-inqueue t))
			(begin
			  (tokenizer-inqueue-set! t (append (tokenizer-inqueue t) (string->list s)))
			  t )]
		  [#t
			(raise 'invalidstate)]))


(define (tokenizer-emit-token t type val attribs)
  (cond [(null? (tokenizer-outqueue t))
			(tokenizer-outqueue-set! t (list (make-token type val attribs)))]
		  [(list? (tokenizer-outqueue t))
			(tokenizer-outqueue-set! t (append (tokenizer-outqueue t)
														  (list (make-token type val attribs))))]
		  [#t
			(raise 'invalidstate)]))


(define (tokenizer-getch t)
  (cond [(null? (tokenizer-inqueue t))
			'()]
		  [#t
			(let* [(queue (tokenizer-inqueue t))
					 (hd    (first queue))]
			  (tokenizer-inqueue-set! t (cdr queue))
			  hd )]))

(define (tokenizer-peekch t)
  (cond [(null? (tokenizer-inqueue t))
			'()]
		  [#t
			(let* [(queue (tokenizer-inqueue t))
					 (hd    (first queue))]
			  hd )]))

;; behaviour of character parser:
;;normal-state:
;; if string-quote => enter string-literal mode
;; if lparens or rparens => emit parenthesis token
;; if whitespace =>
;;   if inside token => finish token & emit
;;   else => eat-whitespace
;; if symbol or alpha =>
;;   if inside token => append to token
;;   else => start new token
;;string-literal-state
;; if string-quote => exit string-literal mode
;; anything else => append to string literal token

(define (tokenizer-tokenize t)
  (let* [(state (tokenizer-state t))
			(next  (tokenizer-peekch t))
			(emit-inprogress
			 (lambda ()
				(if (not (null? (tokenizer-temp t)))
					 (begin
						(tokenizer-emit-token
						 t 'token  (list->string (tokenizer-temp t))
						 '())
									 (tokenizer-temp-set! t '()))
								  '())))
				 ]
		  (cond [(null? (tokenizer-inqueue t))
					(begin
					  (emit-inprogress)
					  '())]
				  [(eq? state 'normal)
					(cond [(is-lparens? next)
							 (begin
								(tokenizer-getch t)
								(emit-inprogress)
								(tokenizer-emit-token t 'lparen #f '()))]
							[(is-rparens? next)
							 (begin
								(tokenizer-getch t)
								(emit-inprogress)
								(tokenizer-emit-token t 'rparen #f '()))]
							[(char=? next <quote-character>)
							 (begin
								(tokenizer-getch t)
								(tokenizer-state-set! t 'string-literal))]
							[(is-whitespace? next)
							 (begin
								(emit-inprogress)
								(tokenizer-getch t))]
							[#t  ;TODO: 
							 (begin
								(tokenizer-temp-set! t (append (tokenizer-temp t) (list next)))
								(tokenizer-getch t)
								;(display (string-append "eating " (string (tokenizer-getch t))))
													 ;(newline))]
								)]
							)]
				  [(eq? state 'string-literal)
					(if (char=? next <quote-character>)
						 (begin
							(tokenizer-state-set! t 'normal)
							(tokenizer-emit-token t
							  'string-literal (list->string (tokenizer-temp t))
							  '())
							(tokenizer-temp-set! t '())
							(tokenizer-getch t))						 
						 (begin
							(tokenizer-temp-set! t
									(append (tokenizer-temp t)
											  (list (tokenizer-getch t))))
							))])))

(define (tokenizer-run-til-empty t)
  (letrec [(iter (lambda (x)
						 (if (null? (tokenizer-tokenize x))
							  '()
							  (iter x))))]
	 (iter t)))


(define (post-process t e)
  (let [(process
			(lambda (x)
			  (cond [(not (token? x))
						'()]
					  [(eq? (token-type x) 'token)
						
