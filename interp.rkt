#!/usr/bin/racket
#lang racket

;; https://www.cs.indiana.edu/cgi-pub/c311/lib/exe/fetch.php?media=pmatch.scm
;; pmatch included here ******************
(define-syntax pmatch
  (syntax-rules ()
    ((_ v c ...) (pmatch-who #f v c ...))))

(define-syntax pmatch-who
  (syntax-rules (else guard)
    ((_ name (rator rand ...) c ...)
     (let ((v (rator rand ...)))
       (pmatch-aux '(rator rand ...) name v c ...)))
    ((_ name v c ...)
     (pmatch-aux 'v name v c ...))))

(define-syntax pmatch-aux
  (syntax-rules (else guard quasiquote)
    ((_ w name v)
     (begin
       (if 'name
           (printf "pmatch ~s failed\n" 'name)
           (printf "pmatch failed\n"))
       (printf "with input ~s evaluating to ~s\n" w v)
       (error 'pmatch "match failed")))
    ((_ w name v (else e0 e ...)) (begin e0 e ...))
    ((_ w name v ((quasiquote pat) (guard g ...) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux w name v cs ...))))
       (ppat v pat (if (and g ...) (begin e0 e ...) (fk)) (fk))))
    ((_ w name v ((quasiquote pat) e0 e ...) cs ...)
     (let ((fk (lambda () (pmatch-aux w name v cs ...))))
       (ppat v pat (begin e0 e ...) (fk))))))

(define-syntax ppat
  (syntax-rules (unquote __)
    ((_ v __ kt kf) kt)
    ((_ v (unquote var) kt kf) (let ((var v)) kt))
    ((_ v (x . y) kt kf)
     (if (pair? v)
         (let ((vx (car v)) (vy (cdr v)))
           (ppat vx x (ppat vy y kt kf) kf))
         kf))
    ((_ v lit kt kf) (if (equal? v (quote lit)) kt kf))))
;; pmatch included here ******************

;; I can haz tracing?
(require racket/trace)

(define eval-expr
  (lambda (expr env)
    (pmatch expr
            [`(if ,c ,p ,a)
             (if (eval-expr c env)
                 (eval-expr p env)
                 (eval-expr a env))]
            [`,x (guard (or (boolean? x) (number? x)))
                 x]
            [`,x (guard (symbol? x))
                 (env x)]
            [`(lambda (,x) ,body)
             (lambda (arg)
               (eval-expr body (lambda (y)
                                 (if (eq? x y)
                                     arg
                                     (env y)))))]
            [`(,rator ,rand1 ,rand2)
             ((eval-expr rator env)
              (eval-expr rand1 env)
              (eval-expr rand2 env))]
            [`(,rator ,rand)
             ((eval-expr rator env)
              (eval-expr rand env))])))

(define environment
  (lambda (y) (error "oops")))

(trace eval-expr)

(eval-expr '(((lambda (rf)
                (lambda (n)
                  ((rf rf) n)))
              (lambda (!)
                (lambda (n)
                  (if (zero? n)
                      1
                      (* n ((! !) (sub1 n)))))))
             5)
           (lambda (arg)
             (cond
               ((eq? arg 'zero?) zero?)
               ((eq? arg 'sub1) sub1)
               ((eq? arg '*) *)
               ((eq? arg '+) +)
               (else (environment arg)))))



(require racket/trace)

(define sum
  (lambda (n)
    (cond
      ((= n 0) 0)
      (else (+ n (sum (- n 1)))))))

(trace sum)

(sum 4)
