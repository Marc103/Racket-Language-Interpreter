#lang racket

(provide parse
         desugar
         eval
         load-defs
         repl)


;; integer value
(struct int-exp (val) #:transparent)

;; boolean value
(struct bool-exp (bool) #:transparent)

;; arithmetic expression
(struct arith-exp (op lhs rhs) #:transparent)

;; variable
(struct var-exp (id) #:transparent)

;; let expression
(struct let-exp (ids vals body) #:transparent)

;; lambda expression
(struct lambda-exp (id body) #:transparent)

;; function application
(struct app-exp (fn arg) #:transparent)

;; if expression
(struct if-exp (cond expr1 expr2) #:transparent)

;; relational expression
(struct rel-exp (op expr1 expr2) #:transparent)


;; Parser
(define (parse sexp)
  (match sexp
    ;; integer literal
    [(? integer?)
     (int-exp sexp)]

    ;; bool literal
    [(? boolean?)
     (bool-exp sexp)]

    ;; if expression
    [(list 'if cond expr1 expr2)
     (if-exp (parse cond) (parse expr1) (parse expr2))]

    ;; relational expressions
    [(list (and op (or '= '<)) lhs rhs)
     (rel-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; arithmetic expression
    [(list (and op (or '+ '*)) lhs rhs)
     (arith-exp (symbol->string op) (parse lhs) (parse rhs))]

    ;; identifier (variable)
    [(? symbol?)
     (var-exp sexp)]

    ;; let expressions
    [(list 'let (list (list id val) ...) body)
     (let-exp (map parse id) (map parse val) (parse body))]

    ;; lambda expression -- modified for > 1 params
    [(list 'lambda (list ids ...) body)
     (lambda-exp ids (parse body))]

    ;; function application -- modified for > 1 args
    [(list f args ...)
     (app-exp (parse f) (map parse args))]

    ;; basic error handling
    [_ (error (format "Can't parse: ~a" sexp))]))


;; Desugar-er -- i.e., syntax transformer

(define (desugar exp)
  (match exp
    ;; if expression
    [(if-exp cond expr1 expr2)
     (if-exp (desugar cond) (desugar expr1) (desugar expr2))]

    ;; relational expression
    [(rel-exp op expr1 expr2)
     (rel-exp op (desugar expr1) (desugar expr2))]

    ;; subtraction
    [(app-exp (var-exp '-) (list expr1 expr2))
     (arith-exp "+" (desugar expr1)
                (arith-exp "*" (int-exp -1) (desugar expr2)))]

    ;; and
    [(app-exp (var-exp 'and) bool-exprs)
     (foldl (lambda (id rbexp)
              (if-exp (desugar id)(desugar rbexp) (bool-exp #f)))
            (bool-exp #t)
            bool-exprs)]

    ;; or
    [(app-exp (var-exp 'or) bool-exprs)
     (foldl (lambda (cond rbexp)
              (if-exp (desugar cond)(bool-exp #t) (desugar rbexp)))
            (bool-exp #f)
            bool-exprs)]
    
    ;; cond
    [(app-exp (var-exp 'cond) cond-exprs)
      (foldr (lambda (cond-expr rcexp)
               (if-exp (extractCond cond-expr)(extractExp cond-expr)(desugar rcexp)))
             (if-exp (bool-exp #t) (extractExp (first (reverse cond-exprs))) (bool-exp #t) )
             (reverse (rest (reverse cond-exprs))))]

    ;; >=
    [(app-exp (var-exp '>=) twoexprs)
     (let ([exp1 (desugar (first twoexprs))]
           [exp2 (desugar (second twoexprs))])
           (desugar (app-exp (var-exp 'or) (cons (rel-exp "<" exp2 exp1)
                                    (cons (rel-exp "=" exp1 exp2) '())))))]

    ;; <=
     [(app-exp (var-exp '<=) twoexprs)
     (let ([exp1 (desugar (first twoexprs))]
           [exp2 (desugar (second twoexprs))])
           (desugar (app-exp (var-exp 'or) (cons (rel-exp "<" exp1 exp2)
                                    (cons (rel-exp "=" exp1 exp2) '())))))]

    ;; >
    [(app-exp (var-exp '>) twoexprs)
     (let ([exp1 (desugar (first twoexprs))]
           [exp2 (desugar (second twoexprs))])
           (desugar (rel-exp "<" exp2 exp1)))]
    
    ((arith-exp op lhs rhs)
     (arith-exp op (desugar lhs) (desugar rhs)))
    
    ((let-exp ids vals body)
     (let-exp ids (map desugar vals) (desugar body)))
    
    ((lambda-exp ids body)
     (foldr (lambda (id lexp) (lambda-exp id lexp))
            (desugar body)
            ids))

    ((app-exp f args)
     (foldl (lambda (id fexp) (app-exp fexp id))
            (desugar f)
            (map desugar args)))
    
    (_ exp)))

;; Desugar helper functions
(define (extractCond cond-expr)
  (match cond-expr
    [(app-exp cond exp)
     cond]
    )
  )

(define (extractExp cond-expr)
  (match cond-expr
    [(app-exp cond exp)
     (first exp)]
    )
  )


;; function value + closure
(struct fun-val (id body env) #:prefab)


;; Interpreter
(define (eval expr [env '()])
  (match expr
    ;; int literal
    [(int-exp val) val]

    ;; bool literal
    [(bool-exp bool) bool]

    ;; if expression
    [(if-exp cond expr1 expr2)
     (if (eval cond env) (eval expr1 env) (eval expr2 env))]

    ;; relational expression
    [(rel-exp "=" expr1 expr2)
     (= (eval expr1 env) (eval expr2 env))]
    [(rel-exp "<" expr1 expr2)
     (< (eval expr1 env) (eval expr2 env))]

    ;; arithmetic expression
    [(arith-exp "+" lhs rhs)
     (+ (eval lhs env) (eval rhs env))]
    [(arith-exp "*" lhs rhs)
     (* (eval lhs env) (eval rhs env))]         
          
    ;; variable binding
    [(var-exp id)
     (let ([pair (assoc id env)])
       (if pair (cdr pair) (error (format "~a not bound!" id))))]

    ;; let expression
    [(let-exp (list (var-exp id) ...) (list val ...) body)
     (let ([vars (map cons id
                      (map (lambda (v) (eval v env)) val))])
       (eval body (append vars env)))]

    ;; lambda expression
    [(lambda-exp id body)
     (fun-val id body env)]

    ;; function application
    [(app-exp f arg)

     ;;(print env)
     (match-let ([(fun-val id body clenv) (eval f env)]
                 [arg-val (eval arg env)])
       
       (eval body (cons (cons id arg-val) clenv)))]

    ;; basic error handling
    [_ (error (format "Can't evaluate: ~a" expr))]))


;; load definitions (returning env)
(define (load-defs filename)
  (let* ([sexps (file->list filename)]
         [fn-names (map (compose first second) sexps)]
         [emptyEnvs (foldr (lambda (fn_name sexp res) (cons
                                       (cons fn_name
                                             (eval(desugar(parse
                                             (cons 'lambda
                                             (cons (rest (second sexp))
                                             (cons (third sexp) '())))))))
                                             
                                       res))
                                           '()
                                           fn-names
                                           sexps)])
    ;;(print emptyEnvs)
    (let* ([ph (make-placeholder '())]
         [env (insertEnvs emptyEnvs ph)])
    (placeholder-set! ph env)
    (make-reader-graph env))
    ))

(define (insertEnvs fnpairs partialEnvs )
  
  (if (empty? fnpairs) '()
      (cons
       (cons (car (first fnpairs)) (insertEnv (cdr (first fnpairs)) partialEnvs))
       (insertEnvs (rest fnpairs) partialEnvs))))


(define (insertEnv fnval cycenv)
  (match fnval [(fun-val id body env) (fun-val id body cycenv)]
  ))

;;(eval (desugar (parse (cons 'lambda (cons  (rest (second sexp))(cons (third sexp)  '()))))))
;; REPL
(define (repl [filename #f])
  (let loop ([env (if filename (load-defs filename) '())])
    (let ([stx (desugar (parse (read)))])
      (when stx
        (println (eval stx env))
        (loop env)))))