(use fmt comparse srfi-14)

(load "parse-2.scm")
(load "environment.scm")

(define (banner)
  (print "ns-scheme v0.0.0"))

(define-record-type repl-settings
  (make-repl-settings
   cwd prompt line-no handlers history run)
  repl-settings?
  (cwd repl-cwd (setter repl-cwd))
  (prompt repl-prompt (setter repl-prompt))
  (line-no repl-line-no (setter repl-line-no))
  (handlers repl-handlers (setter repl-handlers))
  (history repl-history (setter repl-history))
  (run repl-run (setter repl-run)))


(define (handle-repl-cmd rc rs)
  (let* ((cmd (substring (cadr rc) 1 (string-length (cadr rc))))
         (as  (assoc cmd (repl-handlers rs))))
    (if (equal? #f as)
        (print (fmt #f "Invalid top-level command : " cmd))
        (for-each (lambda (x) (apply x (list rc rs))) (cdr as)))))

(define (repl-quit rc rs)
  (set! (repl-run rs) #f))

(define (ns-repl)
  (let* ((tle (new-tl-environment))
         (rs (make-repl-settings ""
                                 "#;7- "
                                 0
                                 (list (list "quit" repl-quit))
                                 '()
                                 #t))
         (get-repl-settings (lambda () rs))
         (append-history (lambda (str)
                           (set! (repl-history rs)
                                 (append (repl-history rs)
                                         (list str))))))
    (banner)
    (call/cc
     (lambda (bail)
       (let outer-loop ()
         (if (not (repl-run rs))
             (bail 'quit))
         (display (fmt #f "#;(N7) " (repl-line-no rs) "> "))
         (let loop ((input-string ""))
           (let* ((new-string (read-line))
                  (_ (if (equal? new-string #!eof)
                         (bail 'eof)
                         0))
                  (ap (string-append input-string
                                     (if (zero? (string-length input-string))
                                         ""
                                         " ")
                                     new-string))
                  (pr (parse parse-expr ap)))             
             (set! (repl-line-no rs) (+ 1 (repl-line-no rs)))
             (cond ((string= ap " ")
                    (loop ""))                   
                   ((equal? (car pr) 'repl-command)
                    (handle-repl-cmd pr rs))
                   (pr             
                    (append-history ap)
                    (print pr))
                   (else
                    (loop ap)))))
         (outer-loop))))))
