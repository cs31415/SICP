n = 5

A 1
B 2
C 4
D 8
E 16

; Starting with symbol-frequency ordered pairs and
; aggregating:
((A 1) (B 2) (C 4) (D 8) (E 16))
(((A 1) (B 2) (A B) 3) (C 4) (D 8) (E 16))
((
  ((A 1) (B 2) (A B) 3) 
  (C 4) 
  (A B C)
  7)
  (D 8) (E 16))
(((
  ((A 1) (B 2) (A B) 3) 
  (C 4) 
  (A B C)
  7)
  (D 8) 
  (A B C D)
  15)
  (E 16))
; And the answer is:
((((
  ((A 1) (B 2) (A B) 3) 
  (C 4) 
  (A B C)
  7)
  (D 8) 
  (A B C D)
  15)
  (E 16)
  (A B C D E)
  31))


n = 10

A 1
B 2
C 4
D 8
E 16
F 32
G 64
H 128
I 256
J 512


; Generated using generate-huffman from exercise 2.69, since it's
; so damn tedious to do this by hand!
(((((((((((leaf A 1) (leaf B 2) (A B) 3) 
         (leaf C 4) 
         (A B C) 
         7) 
        (leaf D 8) 
        (A B C D) 
        15)
       (leaf E 16)
       (A B C D E)
       31)
      (leaf F 32)
      (A B C D E F)
      63)
     (leaf G 64)
     (A B C D E F G)
     127)
    (leaf H 128)
    (A B C D E F G H)
    255)
   (leaf I 256)
   (A B C D E F G H I)
   511)
  (leaf J 512)
  (A B C D E F G H I J)
  1023))

; Bits required to encode the least frequent symbol:
; The least frequent symbol is A whose code is 1. 
; This requires 1 bit to encode.
; The most frequent symbol is E (f=16) for n=5 and 
; J(f=512) for n=10.
; E needs log2(16)=4 bits to encode.
; J needs log2(512)=9 bits to encode

; In general, the least frequent bit requires 1 bit to encode
; while the most frequent bit requires n-1 bits to encode.
