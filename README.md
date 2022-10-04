# Racket-Language-Interpreter

Some examples to try:

(eval (desugar (parse '(> 2 (* 3 2)))))

(eval (desugar (parse '(<= 2 (+ 2 3)))))

(eval (desugar (parse '(let ([x 4] [y 2] [z (+ 3 4)])
                                                  (cond [(< x y) x]
                                                        [(= x y) y]
                                                        [(< y z) (+ x z)]
                                                        [else #t])))))
 
(eval (desugar (parse '(- (* 2 5) (+ 1 3)))))
                                                        
                                                 
