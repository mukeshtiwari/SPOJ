#lang racket
(require math/number-theory
         math/matrix)

;; This is bottle neck 
(define (factorial-mod l h p)
  (for/fold ([acc 1])
            ([i (in-range l (+ h 1))])
    (modulo (* acc i) p)))


(define (prime-base n p)
  (cond
    [(< n p) (cons n null)]
    [else
     (let-values ([(quot rem) (quotient/remainder n p)])
       (cons rem (prime-base quot p)))]))


(define (zip-fun mp np)
  (match (list mp np)
    [(list '() '()) '()]
    [(list (cons m mrest) '()) (cons (cons m 0) (zip-fun mrest '()))]
    [(list '() (cons n nrest)) (cons (cons 0 n) (zip-fun '() nrest))]
    [(list (cons m mrest) (cons n nrest)) (cons (cons m n) (zip-fun mrest nrest))]))


(define (inverse-euler n p)
  (modular-inverse n p))


(define (binom-mod mi ni p)
  (if (< mi ni) 0
      (let* ([k (max (- mi ni) ni)]
             [l (min (- mi ni) ni)]
             [uval (factorial-mod (+ k 1) mi p)]
             [lval (factorial-mod 1 l p)])
        (modulo (* uval (inverse-euler lval p)) p))))

;(define (binom-mod mi ni p)
;  (modulo (binomial mi ni) p))

(define (lucas-binom-mod-prime m n p)
  (let* ([mp (prime-base m p)]
         [np (prime-base n p)]
         [mnp (zip-fun mp np)])
    (foldl (λ(xy init) (modulo (* init (binom-mod (car xy) (cdr xy) p)) p))
           1 mnp)))

;(lucas-binom-mod-prime 950 100 7) ; 2
;(lucas-binom-mod-prime 10 2 13); 6
;(lucas-binom-mod-prime 1000 900 13); 8
;(lucas-binom-mod-prime (expt 10 9)  (expt 10 8) (+ 7 (expt 10 9)))

;; M is square free
(define (lucas-binom-mod-square-free-num m n M)
  (let* ([fact (prime-divisors M)]
         [luca (map (λ(p) (lucas-binom-mod-prime m n p)) fact)])
    (solve-chinese luca fact)))


(let loop ()
  (define a (string->number (read-line (current-input-port) 'any)))
  (let/ec break
    (define (repeat cnt)
      (cond
        [(>= cnt a) (break)]
        [else
         (define b (read-line (current-input-port) 'any))
         (define in (map string->number (string-split b)))
         (define m (car in))
         (define n (cadr in))
         (define M (caddr in))
         (fprintf (current-output-port) "~a\n"
                  (lucas-binom-mod-square-free-num m n M))
         (repeat (+ 1 cnt))]))
    (repeat 0)))

;; Tested for 
;; https://www.hackerrank.com/contests/infinitum10/challenges/cheese-and-random-toppings/problem
