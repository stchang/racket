#lang racket
;; UNCOMMENT:
;(require htdp/show-queen)

;; n-queens, in generate-and-filter style, with naive laziness, ie lazy lists only
;; - syntax is from esop2013 paper: 
;;   - lcons for lists
;;   - cons for pairs
;;   - first/rest instead of car/cdr
;;   - delay creates thunks
;; - semantics (after analysis):
;;   - use Racket car/cdr bc first/rest don't work with improper lists
;;   - first, rest, null? *explicitly* forced
;;   - use Racket "lazy" to create thunks
;; - result is displayed using htdp show-queen library

;; *** this file is the same as nqueens-lazy-naive.rkt,
;;   except with requires and macros commented out, to work with tool

;; DIRECTIONS: 
;; 1) press "Fix Laziness" button
;;    - tool will add delay in foldr
;;    - tool will add explicit forces
;;    - result is equiv to nqueens-lazy-proper.rkt
;; 2) uncomment commented-out code (marked with "UNCOMMENT:")
;; 3) press run
;; 4) don't save

;; UNCOMMENT: 
;(define-syntax-rule (first x) (car x))
;(define-syntax-rule (rest x) (cdr x))
;(define-syntax-rule (lcons x y) (cons x (lazy y)))
;(define-syntax-rule (delay x) (lazy x))

(define (add1 x) (+ x 1))

(define (build-list-help n f m)
  (if (= n m)
      null
      (lcons (f m) (build-list-help n f (add1 m)))))
(define (build-list n f) (build-list-help n f 0))

(define (filter p? lst)
  (if (null? lst)
      null
      (let ([head (first lst)])
        (if (p? head)
            (lcons head (filter p? (rest lst)))
            (filter p? (rest lst))))))

(define (foldl f acc lst)
  (if (null? lst)
      acc
      (foldl f (f (first lst) acc) (rest lst))))

(define (andmap f lst)
  (if (null? lst)
      true
      (and (f (first lst)) (andmap f (rest lst)))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (lcons (first lst1) (append (rest lst1) lst2))))

(define (!= x y) (not (= x y)))
(define (abs- x y) (if (< x y) (- y x) (- x y)))
(define (safe? q1 q2)
  (and (and (!= (first q1) (first q2)) 
            (!= (rest q1) (rest q2)))
       (!= (abs- (first q1) (first q2)) (abs- (rest q1) (rest q2)))))

(define (tails lst)
  (if (null? lst)
      (lcons null null)
      (lcons lst (tails (rest lst)))))

(define (map f lst)
  (if (null? lst)
      null
      (lcons (f (first lst)) (map f (rest lst)))))

(define (foldr f base lst)
  (if (null? lst)
      base
      (f (first lst) (foldr f base (rest lst)))))


(define (nqueens n)
  (let ([qu 
         (λ (i qss) 
           (foldr
            (λ (qs acc)
              (append (map (λ (k) (lcons (cons i k) qs))
                           (build-list n add1))
                      acc))
            null qss))]
        [ok? 
         (λ (lst) 
           (if (null? lst)
               true
               (andmap (λ (q) (safe? (first lst) q)) (rest lst))))])
    (let ([all-possible-solns 
           (foldl qu (lcons null null) (build-list n add1))]
          [valid? 
           (λ (lst) (andmap ok? (tails lst)))])
      (first (filter valid? all-possible-solns)))))


;; UNCOMMENT:
;(define (force-list lst)
;  (if (null? (force lst))
;      null
;      (cons (first (force lst)) (force-list (rest (force lst))))))
;
;(define (show-queens res)
;  (define resf (force-list res))
;  (define n (length resf))
;  (show-queen
;   (for/list ([i n])
;     (for/list ([j n])
;       (if (member (cons (add1 i) (add1 j)) resf)
;           #t #;(printf "Q ")
;           #f #;(printf "· ")))
;     #;(printf "\n"))))
;
;
;(show-queens (time (nqueens 8)))
(nqueens 8)