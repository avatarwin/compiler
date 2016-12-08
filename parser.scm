(define digit
  (in char-set:digit))

(define (as-number parser)
  (bind (as-string parser)
		  (o result string->number)))

(define my-symbols
  (string->char-set "!#$%&|*+-/:<=>?@^_~"))

(define symbol
  (in my-symbols))

(define letter
  (in char-set:letter))

(define parse-atom
  (sequence* ((head  (any-of letter symbol))
				  (rest  (zero-or-more (any-of letter digit symbol))))
				 (result (list 'atom (list->string (append (list head) rest))))))

(define parse-number
  (sequence* ((num  (as-number (one-or-more (in char-set:digit)))))
				 (result (list 'number num))))

(define parse-string
  (sequence* ((_ (is #\"))
				  (c (as-string (zero-or-more (satisfies (lambda (x)
														  (not (eqv? x #\")))))))
				  (_ (is #\")))
				 (result (list 'string c))))

(define parse-quoted
  (sequence* ((_ (is #\'))
				  (c parse-expr))
				 (result (list 'quoted c))))

(define parse-expr
  identity)

(define parse-dottedlist
  identity)

(define parse-contained
  identity)

(define seperatedBySpace
  (followed-by parse-expr
					(in char-set:whitespace)))

(define word
  (as-string (one-or-more
				  (satisfies (lambda (x)
									(not (char-set-contains? char-set:whitespace x)))))))
(define whitespace
  (bind (one-or-more (in char-set:whitespace))
		  (lambda (x) (result 'whitespace))))

(define parse-list
  (zero-or-more parse-expr))

(define parse-containedlist
  (bind (enclosed-by (is #\()
							parse-list 
							(is #\)))
		  (lambda (x) (result (list 'list x)))))

(define parse-containeddotted
  (bind (enclosed-by (is #\()
							parse-dottedlist
							(is #\)))
;		  (lambda (x) (result (list 'dotted-list x)))))
		  (lambda (x) (result x))))
  
(define parse-dottedlist
  (sequence* ((hd (one-or-more parse-expr))
;				  (_ whitespace)
				  (_ (is #\.))
				  (_ whitespace)
				  (tail parse-expr))
		(result (list 'dotted-list (list hd tail)))))
  
(define parse-expr
  (sequence*  ((expr (any-of parse-quoted
                              parse-atom
                              parse-string
                              parse-number
                              ;;			 whitespace
                              
                              


                              ))
               (_  whitespace))
              (result expr)
              ))

(parse (bind (enclosed-by (is #\()
                          parse-expr
                          (is #\)))
             (lambda (x) (result x))) "(test )")
