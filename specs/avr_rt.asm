          processor     avr
            
ramtop    EQU     1F00h
SPL       EQU     003Dh   
SPH       EQU     003Eh
;;; entry point is at 0000h
_start:
          cli                          ; disable interrupts until we're done
          ldi     r16, low(ramtop)
          out     SPL, r16
          ldi     r16, high(ramtop)
          out     SPH, r16
   
          jmp     _start

;;; alloc entry point, r1 + r2
alloc:    
          ret
free:
          ret
defrag:
          ret
run_gc:
          ret
enable_gc:
          ret
disable_gc:
          ret

;; Local Variables:
;; tab-stop-list: (0 10 20 30 40)
;; End:
