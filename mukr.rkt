#lang racket

; logic variables represented with vectors
(define (var c) (vector c))
(define (var? x) (vector? x))
(define (var=? x1 x2) (= (vector-ref x1 0) (vector-ref x2 0)))

; walk function to search for a variable's value
; u - variable to look for
; s - state to check
(define (walk u s)
  (let* ([pr (and (var? u) (assf (lambda (v) (var=? u v)) s))])
    (if pr (walk (cdr pr) s) u)))

; extend a state without checks for circularities
; x - variable index
; v - variable value
(define (ext-s x v s) `((,x . ,v) . ,s))

; streams
(define mzero '())
(define (unit s/c) (cons s/c mzero)) ; lifts state into stream

(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(and (var? u) (var? v) (var=? u v)) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let* ([s (unify (car u) (car v) s)])
         (and s (unify (cdr u) (cdr v) s)))
       ]
      [else (and (eqv? u v) s)]
      )))

; combining streams (can be empty, immature, or matured)
(define (mplus s1 s2)
  (cond
    [(null? s1) s2]
    [(procedure? s1) (lambda () (mplus s2 (s1)))] ; switch and process immature stream
    [else (cons (car s1) (mplus (cdr s1) s2))]))

(define (bind s g)
  (cond
    [(null? s) mzero]
    [(procedure? s) (lambda () (bind (s) g))]
    [else (mplus (g (car s)) (bind (cdr s) g))]))

(define (call/fresh f)
  (lambda (s/c) ; state/counter
    (let* ([c (cdr s/c)])
      ; return with goal with new var and updated state with counter
      ((f (var c)) `(,(car s/c) . ,(+ c 1))) 
      )))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

; recover miniKanren

; inverse-eta-delay (snooze)
(define-syntax Zzz
  (syntax-rules ()
    [(Zzz g) (lambda (s/c) (lambda () (g s/c)))]))

(define-syntax conj+
  (syntax-rules ()
    [(conj+ g) (Zzz g)]
    [(conj+ g0 g ...) (conj (Zzz g0) (conj+ g ...))]))

(define-syntax disj+
  (syntax-rules ()
    [(disj+ g) (Zzz g)]
    [(disj+ g0 g ...) (disj (Zzz g0) (disj+ g ...))]))

(define-syntax conde
  (syntax-rules ()
    [(conde (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...)]))

(define-syntax fresh
  (syntax-rules ()
    [(fresh () g0 g ...) (conj+ g0 g ...)]
    [(fresh (x0 x ...) g0 g ...) (call/fresh (lambda (x0) (fresh (x ...) g0 g ...)))]))

; stream-to-list
(define (pull s) (if (procedure? s) (pull (s)) s))

(define (take-all s)
  (let* ([s (pull s)])
         (if (null? s) '() (cons (car s) (take-all (cdr s))))))

(define (take-n n s)
  (if (zero? n) '()
    (let* ([s (pull s)])
      (cond
        [(null? s) '()]
        [else (cons (car s) (take (- n 1) (cdr s)))]))))

;reification
(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))
(define (reify-s v s)
  (let* ([v (walk v s)])
    (cond
      [(var? v)
       (let* ([n (reify-name (length s))])
         (cons `(,v . ,n) s))]
      [(pair? v) (reify-s (cdr v) (reify-s (car v) s))]
      [else s])))
(define (walk* v s)
  (let* ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v) (cons (walk* (car v) s) (walk* (cdr v) s))]
      [else v])))

(define (mK-reify s/c*)
  (map reify-state/1st-var s/c*))
(define (reify-state/1st-var s/c)
  (let* ([v (walk* (var 0) (car s/c))])
    (walk* v (reify-s v '()))))

(define empty-state '(() . 0))
(define (call/empty-state g) (g empty-state))

; run macros
(define-syntax run
  (syntax-rules ()
    [(run n (x ...) g0 g ...)
     (mK-reify (take-n n (call/empty-state (fresh (x ...) g0 g ...))))]))
(define-syntax run*
  (syntax-rules ()
    [(run* (x ...) g0 g ...)
     (mK-reify (take-all (call/empty-state (fresh (x ...) g0 g ...))))]))
