(define +target-list+ '())
(define-record-type target-machine
  (make-target name desc bits ramsize romsize)
  target?
  (name target-name target-name!)
  (desc target-desc target-desc!)
  (bits target-bits target-bits!)
  (ramsize target-ram target-ram!)
  (romsize target-rom target-rom!))


(define (default-print-method ))
(set! +target-list+
      (append +target-list+
              (list (make-target "avr" "Atmel AVR" 8
                                 2048 8192))))
