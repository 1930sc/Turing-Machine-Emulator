;; Palindrome
;; it would erase the first and last symbol of the tape until either it's empty
;; or the first and the last symbol stop matching.

Initial = In
Blank   = .

Rules   =

(In 0 . -> f0)
(In 1 . -> f1)
(In . . >< ZZ)

(f0 0 0 -> f0)
(f0 1 1 -> f0)
(f0 . . <- c0)

(f1 0 0 -> f1)
(f1 1 1 -> f1)
(f1 . . <- c1)

(c0 0 . <- RR)
(c0 1 . <- ZZ)
(c0 . . >< ZZ)

(c1 1 . <- RR)
(c1 0 . <- ZZ)
(c1 . . >< ZZ)

(RR 0 0 <- RR)
(RR 1 1 <- RR)
(RR . . -> In)
