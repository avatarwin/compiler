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

(define-record-type repl-handler
  (make-repl-handler
   name function description)
  repl-handler?
  (name        rh-name (setter rh-name))
  (function    rh-function (setter rh-function))
  (description rh-description (setter rh-description)))

(define (handle-repl-cmd rc rs)
  (let* ((rhl      (repl-handlers rs))
         (find-cmd (lambda (name)
                     (find (lambda (x)
                             (equal? (rh-name x) name)) rhl)))         
         (cmd (first
               (string-split
                (substring (cadr rc) 1 (string-length (cadr rc))))))
         (as  (find-cmd cmd)))
    (if (equal? #f as)
        (print (fmt #f "Invalid top-level command : " cmd))
        (let ((fun (rh-function as)))
          (if (pair? fun)
              (for-each (lambda (x) (apply x (list rc rs))) fun)
              (apply fun (list rc rs)))))))

(define (repl-quit rc rs)
  (set! (repl-run rs) #f))

(define (repl-help rc rs)
  (let ((h (repl-handlers rs)))
    (set! h (sort h (lambda (l r) (string-ci< (rh-name l) (rh-name r)))))
    (for-each (lambda (x)
                (print (fmt #f
                            ","
                            (rh-name x)
                            "\t - "
                            (rh-description x))))
              h)))

(define (ns-repl)
  (let* ((tle (new-tl-environment))
         (rs (make-repl-settings ""
                                 "#;7- "
                                 0
                                 (list (make-repl-handler "quit" repl-quit "quit the interpreter")
                                       (make-repl-handler "help" repl-help "list repl commands"))
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
                  (pr (parse parse-tl ap)))             
             (set! (repl-line-no rs) (+ 1 (repl-line-no rs)))
             (cond ((string= ap " ")
                    (loop ""))                   
                   ((and pr (equal? (car pr) 'repl-command))
                    (handle-repl-cmd pr rs))
                   (pr             
                    (append-history ap)
                    (print pr))
                   (else
                    (loop ap)))))
         (outer-loop))))))
