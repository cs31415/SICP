; The reason I think is that algebraic identities such as 
; A/A=1, A-A=0 etc. don't hold when applied to uncertain 
; numbers. Instead of producing a zero-width interval (1 1) 
; or (0 0) they produce an interval with small but finite 
; width. It is this finite width that introduces a variance
; in the equivalent expression.

; A system that avoids this issue would have to have the 
; ability to refactor algebraic expressions to their 
; simplest equivalent forms with non-repeating terms.
; This is certainly possible, but non-trivial. It would 
; involve writing an expression parser, and then a 
; refactoring layer on top of it that would check for 
; repeated terms and refactor them.
