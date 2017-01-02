;;;; testing...

(use comparse srfi-14)

(define digit
  (in char-set:digit))

(define hex-alpha-digit
  (in (string->char-set "abcdefABCDEF")))

(define octal-digit
  (in (string->char-set "01234567")))

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

(define binary-prefix
  (char-seq "#b"))

(define binary-digits
  (as-string (one-or-more (any-of (is #\0) (is #\1)))))

(define hex-prefix
  (char-seq "#x"))

(define hex-digits
  (as-string (one-or-more (any-of digit hex-alpha-digit))))

(define octal-prefix
  (char-seq "#o"))

(define octal-digits
  (as-string (one-or-more (any-of octal-digit))))

(define decimal-prefix
  (char-seq "#d"))

(define exponent
  (sequence e integer))

(define ratio
  (sequence integer (is #\/) integer))

(define pnumber-real
  (bind (as-string
         (sequence (maybe (is #\-)) (any-of (sequence integer fractional exponent)
                                            (sequence integer exponent)
                                            (sequence integer fractional))))
        (lambda (x)
          (result (list 'number-real x)))))

(define pnumber-radix
  (bind (as-string
         (sequence  (any-of
                     (sequence decimal-prefix  (maybe (is #\-)) integer)
                     (sequence octal-prefix  (maybe (is #\-)) octal-digits)
                     (sequence binary-prefix (maybe (is #\-)) binary-digits)
                     (sequence hex-prefix (maybe (is #\-)) hex-digits))))
        (lambda (x)
          (result (list 'number-radix x)))))

(define pnumber-ratio
  (bind (as-string
         (sequence (maybe (is #\-)) ratio))
        (lambda (x)
          (result (list 'number-ratio x)))))
  
(define pnumber-integer
  (bind (as-string
         (sequence (maybe (is #\-)) integer))
        (lambda (x)
          (result (list 'number-integer x)))))

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

(define parse-expr
  (any-of pnumber-real
          pnumber-radix
          pnumber-ratio
          pnumber-integer
          pchar
          pquoted
          pquotenumstr
          pstring
          plist
          patom
          ))

