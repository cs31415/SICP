(car ''abracadabra)
;quote

; Everything after the 1st quote is treated as literal text.
; The car extracts the 1st element from the character list 
; "'abracadabra" which is "'".
; The interpreter prints "quote" because that is what it
; internally represents "'" as.