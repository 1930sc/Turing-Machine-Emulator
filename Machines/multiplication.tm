;; Multiplies two numbers in unary form.
;; 11x111 = 111111, 11x11 = 1111, 11x =  , x11 =  .
;; (Cero is indicated by an empty word)

Initial     = IN
Terminating = FI
Blank       = .

Rules =

(IN x x -> IN)
(IN 1 1 -> IN)
(IN . # <- B)

(B 1 1 <- B)
(B x x <- B)
(B . . -> Q)

(Q 1 . -> A)
(Q x . -> L)

(A 1 1 -> A)
(A x x -> C)

(C . . -> C)
(C # # <- FILL)
(C 1 . -> F)

(FILL . 1 <- FILL)
(FILL x x <- B)

(F 1 1 -> F)
(F # # -> P)

(P 1 1 -> P)
(P . 1 <- BF)

(BF . . <- BF)
(BF 1 1 <- BF)
(BF # # <- BF)
(BF x x -> C)

(L 1 . -> L)
(L # . -> FI)
