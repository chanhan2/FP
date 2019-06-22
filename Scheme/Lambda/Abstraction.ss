#lang racket

; set macro
(define my 1)

; finds the nested list depth
(define (list-depth xs)
  (define (helper lst depth)
    (foldl (λ (x z)
             (if (list? x)
                 (max z (helper x (+ 1 depth)))
                 (max z depth)))
      depth
      lst))
  (helper xs 0))

; fibonacci function
(define (slow-fib n)
  (match n
    [0 0]
    [1 1]
    [n (+ (slow-fib (- n 1)) (slow-fib (- n 2)))]))

; foldl influence of summing the list
(define (my-sum-v2 lst)
  (define (g accum lst)
    (match lst
       ['() accum]
       [(cons hd tl) (g (+ accum hd) tl)]))
  (g 0 lst))

; foldl influence of calculating the function f(x)
(define (poly-eval-v2 coeffs x)
  (define (g accum lst)
    (match lst
      ['() accum]
      [(cons hd tl) (g (+ (* x accum) hd) tl)]))
  (g 0 coeffs))

; foldl influence of reversing the list
(define (my-reverse lst)
  (define (g accum x)
    (match x
      ['() accum]
      [(cons hd tl) (g (cons hd accum) tl)]))
  (g '() lst))

;--------------------------------------------Below shows lambda circuits--------------------------------------------
; This is to show how lambda can be used to replace or hide certain unnesscary details that masks what is nesscary
; and allows for certain small portion of abstraction through dynamic typing.

; Same as the one defined above for the fibonacci function, but by altering the function signature through embedding 
; lambda to manually enforce dynamic computation.

(define slow-fibv2
  (lambda (n)
    (match n
      [0 0]
      [1 1]
      [n (+ (slow-fibv2 (- n 1)) (slow-fibv2 (- n 2)))])))

(define slow-fib-test
  (lambda (f)
    (lambda (n)
      (match n
        [0 0]
        [1 1]
        [n (+ ((slow-fib-test f) (- n 1)) ((slow-fib-test f) (- n 2)))]))))

(slow-fib-test "wtf")
(lambda (n)
  (match n
    [0 0]
    [1 1]
    [n (+ ((slow-fib-test "wtf") (- n 1)) ((slow-fib-test "wtf") (- n 2)))]))

(define slow-fib-testv3
  (lambda (f)
    (lambda (n)
      (match n
        [0 0]
        [1 1]
        [n (+ ((f f) (- n 1)) ((f f) (- n 2)))]))))

(define summer
  (lambda (self)
    (lambda (xs)
      (match xs
        ['() 0]
        [(cons hd tl) (+ hd ((self self) tl))]))))

;------------------Below shows removal of recursion through Lambda Abstraction and Diagonalization------------------
; A small set of functions where their signature has been altered to mask the function computation and entails for 
; function defintion instead of computational flow.

; same as the one defined above which sums the list in foldl fashion, but is described through function definition
(define (summerv2 clone xs)
  (match xs
    ['() 0]
    [(cons hd tl) (+ hd (clone tl))]))

(define summerv2-test
  (lambda (clone)
    (lambda (xs)
      (summerv2 (clone clone) xs))))

; some circuits defined above where their function defined and conguested through diagonalization
(define recursionFreeFib (slow-fib-test slow-fib-test))
(define sums (summerv2-test summerv2-test))

