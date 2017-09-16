;; Language recognition

;; The tape will end empty only if there it's the same amount of 0s and 1s
;; and all 0s are before the 1s.
;; like this:  0000011111
;; if instead the initial tape it's  like this: 0001011
;; will not end in an empty Tape, the final Tape will be: 10.

Initial = A
Blank   = .

Rules =

(A 0 . -> B)
(A 1 1 >< Z)
(A . . >< Z)

(B 0 0 -> B)
(B 1 1 -> B)
(B . . <- C)

(C 1 . <- D)
(C 0 0 >< Z)
(C . . >< Z)

(D 0 0 <- D)
(D 1 1 <- D)
(D . . -> A)
