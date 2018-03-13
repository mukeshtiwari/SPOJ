#lang racket
(require math/number-theory)

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
  (modular-expt n (- p 2) p))

(define (factorial-mod n p)
  (for/fold ([acc 1])
            ([i (in-range 1 (+ n 1))])
    (modulo (* acc i) p)))
        

(define (binom-mod mi ni p)
  (modulo (* (factorial-mod mi p) (inverse-euler (factorial-mod ni p) p) (inverse-euler (factorial-mod (- mi ni) p) p)) p)) 
  
;(define (binom-mod mi ni p)
;  (modulo (binomial mi ni) p))
  
(define (lucas-binom m n p)
  (let* ([mp (prime-base m p)]
         [np (prime-base n p)]
         [mnp (zip-fun mp np)])
   (foldl (λ(xy init) (modulo (* init (binom-mod (car xy) (cdr xy) p)) p)) 1 mnp)))

(lucas-binom 950 100 7) ; 2
(lucas-binom 10 2 13); 6
(lucas-binom 1000 900 13); 8
