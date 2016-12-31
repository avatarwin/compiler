;;;; testing...

(use comparse srfi-14)

(define digit
  (in char-set:digit))

(define letter
  (in char-set:letter))                 

(define symbol
  (in char-set:symbol))

(define ws-cs
  (in char-set:whitespace))

(define ws
  (one-or-more ws-cs))


(define begin-list
  (is #\())

(define end-list
  (is #\)))



(define integer
  (as-string  (one-or-more digit)))

(define fractional
  (sequence (is #\.) integer))

(define e
  (any-of (char-seq "e+")
          (char-seq "e-")
          (char-seq "E+")
          (char-seq "E-")
          (is #\e)
          (is #\E)))

(define exponent
  (sequence e integer))

(define pnumber
  (bind (as-string
         (sequence (maybe (is #\-)) (any-of (sequence integer fractional exponent)
                                            (sequence integer exponent)
                                            (sequence integer fractional)
                                            integer)))
   (lambda (x)
     (result (list 'number x)))))

(define not-dquote
  (none-of* (is #\")
            item))

(define not-whitespace
  (none-of* ws-cs
            item))

(define pstring
  (bind
   (enclosed-by (is #\")
                (as-string (zero-or-more not-dquote))
                (is #\"))
   (lambda (x)
     (result (list 'string x)))))

(define pchar
  (sequence* [(_  (char-seq "#\\"))
              (ch (as-string (one-or-more not-whitespace)))]
             (result (list 'char ch))))


(define patom
  (bind
   (as-string (sequence (any-of letter symbol)
                        (zero-or-more (any-of letter symbol digit))))
   (lambda (x) (result (list 'atom x)))))

(define pquoted
  (sequence* [(_ (is #\'))
              (expr patom)]
             (result (list 'quoted expr))))

(define pquotenumstr
  (sequence* [(_ (is #\'))
              (expr (any-of pnumber
                            pstring))]
             (result  expr)))

(define plist
  (recursive-parser
   (bind
    (enclosed-by begin-list
                 (any-of (sequence* [(first parse-expr)
                                     (more  (zero-or-more
                                             (preceded-by ws
                                                          parse-expr)))]
                                    (result (cons first more)))
                         (result '()))
                 end-list)
    (lambda (x) (result (list 'list x))))))

;;; another test, go here?

(define parse-expr
  (any-of
          pnumber
          pchar
          pquoted
          pquotenumstr
          pstring
          plist
          patom
          ))

