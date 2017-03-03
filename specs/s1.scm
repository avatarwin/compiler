
(define-syntax define-target
  (syntax-rules (register registers)
    ((_ target-name ())
     (quote target-name))
    ((_ target-name (r ...))
     (list (quote r) ...))))
 
