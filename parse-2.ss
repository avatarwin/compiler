(use comparse srfi-14)

(define digit
  (in char-set:digit))

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

(define whitespace
  (in char-set:whitespace))

(define not-whitespace
  (none-of* whitespace
            (is #\()
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
  (bind (as-string (one-or-more not-whitespace))
        (lambda (x)
          (result (list 'atom x)))))

(define pquoted
  (sequence* [(_ (is #\'))
              (expr patom)]
             (result (list 'quoted expr))))

(define pquotenumstr
  (sequence* [(_ (is #\'))
              (expr (any-of pnumber
                            pstring))]
             (result  expr)))

(define inlist1
  (any-of pquotenumstr
          pquoted
          pnumber
          pstring
          patom
          ))

;; work around greediness by grabbing one or more 'head'
;; items, and a final tail item

(define inlist*
  (sequence* [(head (one-or-more (sequence* [(ex inlist1)
                                             (_ whitespace)]
                                            (result ex))))
              (tail inlist1)]
             (result (append head (list tail)))))

(define plist
  (sequence* [(_ (is #\())
              (contents (any-of inlist*
                                inlist1))
              (_ (is #\)))]
             (result (list 'list contents))))



(define parse-expr
  (any-of  pquoted
           pquotenumstr
           pnumber
           pchar
           pstring
           patom
           plist
          ))

