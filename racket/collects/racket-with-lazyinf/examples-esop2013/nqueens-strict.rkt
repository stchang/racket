#lang racket
(require htdp/show-queen)

;; n-queens, in generate-and-filter style, no laziness
;; - syntax is from esop2013 paper: 
;;   - cons used for both pairs and lists
;;   - first/rest instead of car/cdr
;; - semantics:
;;   - use Racket car/cdr bc first/rest don't work with improper lists
;; - result is displayed using htdp show-queen library

;; DIRECTIONS: just run
;;   (~14s on i7-2600k machine, compiled and run on cmd line)

(define-syntax-rule (first x) (car x))
(define-syntax-rule (rest x) (cdr x))

(define (add1 x) (+ x 1))

(define (build-list-help n f m)
  (if (= n m)
      null
      (cons (f m) (build-list-help n f (add1 m)))))
(define (build-list n f) (build-list-help n f 0))

(define (filter p? lst)
  (if (null? lst)
      null
      (let ([head (first lst)])
        (if (p? head)
            (cons head (filter p? (rest lst)))
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
      (cons (first lst1) (append (rest lst1) lst2))))

(define (!= x y) (not (= x y)))
(define (abs- x y) (if (< x y) (- y x) (- x y)))
(define (safe? q1 q2)
  (and (and (!= (first q1) (first q2)) 
            (!= (rest q1) (rest q2)))
       (!= (abs- (first q1) (first q2)) (abs- (rest q1) (rest q2)))))

(define (tails lst)
  (if (null? lst)
      (cons null null)
      (cons lst (tails (rest lst)))))

(define (map f lst)
  (if (null? lst)
      null
      (cons (f (first lst)) (map f (rest lst)))))

(define (foldr f base lst)
  (if (null? lst)
      base
      (f (first lst) (foldr f base (rest lst)))))


(define (nqueens n)
  (let ([qu 
         (λ (i qss) 
           (foldr
            (λ (qs acc)
              (append (map (λ (k) (cons (cons i k) qs))
                           (build-list n add1))
                      acc))
            null qss))]
        [ok? 
         (λ (lst) 
           (if (null? lst)
               true
               (andmap (λ (q) (safe? (first lst) q)) (rest lst))))])
    (let ([all-possible-solns 
           (foldl qu (cons null null) (build-list n add1))]
          [valid? 
           (λ (lst) (andmap ok? (tails lst)))])
      (first (filter valid? all-possible-solns)))))


(define (show-queens res)
  (define n (length res))
  (show-queen
   (for/list ([i n])
     (for/list ([j n])
       (if (member (cons (add1 i) (add1 j)) res)
           #t #;(printf "Q ")
           #f #;(printf "· ")))
     #;(printf "\n"))))


(show-queens (time (nqueens 8)))
