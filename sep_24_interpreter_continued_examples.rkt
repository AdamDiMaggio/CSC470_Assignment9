;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname sep_24_interpreter_continued_examples) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;constructor
(define empty-env
  (lambda () '(empty-env)))

;adds name-value to current environment
(define extend-env
  (lambda (var-name var-value env)
    (list 'extend-env var-name var-value env)))

(define extend-env*
  (lambda (lon lov env)
    (if (null? lon)
        env
        (extend-env* (cdr lon) (cdr lov) (extend-env (car lon) (car lov) env)))))

     
;is this env empty?
(define empty-env?
  (lambda (env)
    (eq? (car env) 'empty-env)))

;is this env not empty?
(define extend-env?
  (lambda (env)
    (eq? (car env) 'extend-env)))

;getters
;return variable name
(define get-var-name
  (lambda (env)
    (car (cdr env))))

;return variable value
(define get-var-value
  (lambda (env)
    (car (cdr (cdr env)))))
;return next environment
(define get-next-env
  (lambda (env)
    (car (cdr (cdr (cdr env))))))

;returns value associated  w/ var-name or #f if not found
(define apply-env
  (lambda (var-name env)
    (cond
      ((empty-env? env) #f)
      ((eq? var-name (get-var-name env)) (get-var-value env))
      (else (apply-env var-name (get-next-env env))))))

;checks if a variable is "bound"(means being used already)
(define has-binding?
  (lambda (var-name env)
    (not (eq? (apply-env var-name env) #f)))) ;if apply-env comes back not false,variable is being used

;test code
(define env (extend-env 'a 5 (extend-env 'b 7 (empty-env))))

(has-binding? 'a env)
(has-binding? 'c env)






;GRAMMAR constructors

(define var-exp
  (lambda (s)
    (list 'var-exp s)))

(define lambda-exp
  (lambda (s lc-exp)
    (list 'lambda-exp s lc-exp)))

(define app-exp ;basically a function call
  (lambda (lambda-exp param-value)
         (list 'app-exp lambda-exp param-value)))


;Grammar extractors

(define lc-exp->type
  (lambda (lc-exp)
    (car lc-exp)))
;***************************

;var-exp
(define var-exp->var-name 
  (lambda (var-exp)
    (cadr var-exp)))
;***************************
;lambda-exp
(define lambda-exp->bound-var
  (lambda (lambda-exp)
    (cadr lambda-exp)))

(define lambda-exp->body
  (lambda (lambda-exp)
    (caddr lambda-exp)))
;***************************
;app-exp
(define app-exp->lambda-exp
  (lambda (app-exp)
    (cadr app-exp)))

(define app-exp->lambda-input
  (lambda (app-exp)
    (caddr app-exp)))


;Grammar predicates
(define var-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'var-exp)))

(define lambda-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'lambda-exp)))

(define app-exp?
  (lambda (lc-exp)
    (eq? (lc-exp->type lc-exp) 'app-exp)))



;Parse/Unparse
;(func (x) y) <- gets x, does y with x
;(run (func (x) y) param) <- uses param for x
;(getval 'A) <- resolves to value of A

;codex extractors
;func->x
(define func->x
  (lambda (codex)
    (caadr codex)))

;func->y
(define func->y
  (lambda (codex)
    (caddr codex)))

;run->lambda
(define run->lambda
  (lambda (codex)
    (cadr codex)))

;run->param
(define run->param
  (lambda (codex)
    (caddr codex)))

;getval->var
(define getval->var
  (lambda (codex)
    (cadr codex)))




(define parse-exp
  (lambda (codex)
    (cond
      ((eq? (car codex) 'getval)(var-exp (getval->var codex)))
      ((eq? (car codex) 'func) (lambda-exp
                                (func->x codex) ;param
                                (parse-exp(func->y codex)))) ;logic
      ((eq? (car codex) 'run) (app-exp
                               (parse-exp (run->lambda codex)) ;(func (x) x)
                               (parse-exp (getval->var codex))))))) ;parameter 

(define unparse-exp
  (lambda (lc-exp)
    (cond
      ((var-exp? lc-exp) (list 'getval
                               (var-exp->var-name lc-exp)))
      ((lambda-exp? lc-exp) (list 'func
                             (list (lambda-exp->bound-var lc-exp))
                             (unparse-exp (lambda-exp->body lc-exp))))
      ((app-exp? lc-exp) (list 'run
                               (unparse-exp (app-exp->lambda-exp lc-exp))
                               (unparse-exp (app-exp->lambda-input lc-exp)))))))



(define HolyCodex '(run (func (x) (getval x)) (getval c)))
HolyCodex
(parse-exp HolyCodex)
(unparse-exp (parse-exp HolyCodex))









