
(define (log128 n)
  (let loop [(v n)
             (x 0)]
    (if (< v 128)
        (+ x 1)
        (loop (/ v 128) (+ 1 x)))))

#| (define (make-bignum n)
  (let [(s (log256 n))
        (r (make-u8vector s))]
    
|#
