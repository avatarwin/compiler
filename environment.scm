;;; Environment structure and support

(define-record-type env-entry
  (make-env-entry name boxed value flags)
  env-entry?
  (name ee-name (setter ee-name))
  (boxed ee-boxed (setter ee-boxed))
  (value ee-value (setter ee-value))
  (flags ee-flags (setter ee-flags)))

(define-record-type environment
  (make-environment table chain)
  environment?
  (table env-table (setter env-table))
  (chain env-chain (setter env-chain)))

(define (new-tl-environment)
  (make-environment '() #f))

(define (make-chained-environment previous)
  (make-environment '() previous))
  
(define (create-new-ee env name val)
  (let ((ee (make-env-entry name #f val '())))
    (set! (env-table env)
          (append (list (cons name ee))
                  (env-table env)))))

(define (delete-ee env name)
  (let* ((found #f)
         (table (env-table env))
         (result
          (filter-map (lambda (x) (if (and (not found) (equal? (car x) name))
                                 (begin
                                   (set! found #t)
                                   #f)
                                 x)) table)))
    (set! (env-table env) result)
    result))

        
(define (lookup env name)
  (let ((val (assoc name (env-table env)))
        (chain (env-chain env)))
    (if val
        (cdr val)
        (if chain
            (lookup chain name)
            #f))))


  

