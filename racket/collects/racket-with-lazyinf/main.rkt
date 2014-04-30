#lang racket

; implements #lang racket-with-lazyinf
; put in collects/racket-with-lazyinf along with appropriate lang/reader.rkt file

(provide (except-out (all-from-out racket)) #%module-begin)

(require (only-in racket [#%module-begin r:#%module-begin]))

#;(require htdp/show-queen)

(provide #%module-begin)

(require (for-syntax redex/reduction-semantics))

(require (for-syntax (only-in "lazyinf-constraints-worklist.rkt" φ)))

(define-syntax (#%module-begin stx)
  (syntax-case stx ()
    [(_ e ...)
     (with-syntax
         ([(d ... e1)
           (datum->syntax stx (term (φ ,(syntax->datum #'(e ...)))))]
          [require-show-queen
           (datum->syntax stx (syntax->datum #'(require htdp/show-queen)))]
          [lcons
           (datum->syntax
            stx
            (syntax->datum
             #'(define-syntax (lcons stx)
                 (syntax-case stx ()
                   [(_ e1 e2) #'(cons e1 (lazy e2))]))))]
          [first
           (datum->syntax
            stx
            (syntax->datum
             #'(define-syntax-rule (first e1) (car e1))))]
          [rest
           (datum->syntax
            stx
            (syntax->datum
             #'(define-syntax-rule (rest e1) (cdr e1))))]
          [force-list-def
           (datum->syntax
            stx
            (syntax->datum
             #'(define (force-list lst)
                 (if (null? (force lst))
                     null
                     (cons (first (force lst)) (force-list (rest (force lst))))))))]
          [show-queens-def
           (datum->syntax
            stx
            (syntax->datum
             #'(define (show-queens res)
                 (define resf (force-list res))
                 (define n (length resf))
                (show-queen
                 (for/list ([i n])
                   (for/list ([j n])
                     (if (member (cons (add1 i) (add1 j)) resf)
                         #t #;(printf "Q ")
                         #f #;(printf "· ")))
                   #;(printf "\n"))))))])
       (with-syntax
           ([timed
             (datum->syntax
              stx
              (syntax->datum
               #'(show-queens (time e1))))])
       #`(r:#%module-begin
          require-show-queen
          lcons
          first
          rest
          d ...
          force-list-def
          show-queens-def
          timed
          )))]))
