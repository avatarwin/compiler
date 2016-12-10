(use (prefix prcc prcc:))

(define +parsers+
  '())

(define pnumber
  (prcc:<r> "\\d+"))


(define inlist
  (apply prcc:sel +parsers+)
)

(define plist
  (prcc:<and> (prcc:<c> #\()
              (prcc:rep+_ inlist)
              (prcc:<c> #\))))

(set! +parsers+ (list pnumber plist))
