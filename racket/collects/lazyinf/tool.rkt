#lang racket/base

(require racket/class
         racket/gui/base
         racket/port
         racket/unit
         drracket/tool
         drracket/arrow
         mrlib/switchable-button
         syntax/stx
         images/icons/arrow images/icons/style
         "lazyinf-constraints-worklist.rkt")
(require redex/reduction-semantics)

(provide tool@)

#;(define (lazyinf-callback fr)
  (send (send fr get-definitions-text) add-highlights-and-arrows))

#;(define lazyinf-button
  (list
   "Fix Laziness"
   (right-over-arrow-icon metal-icon-color (toolbar-icon-height))
   #;(make-bitmap 1 1)
   lazyinf-callback))


;; overrides execute-callback
(define (lazyinf-unit-frame-mixin super%)
  (class super%
    
    (inherit get-definitions-text)

    (define/public (add-highlights-and-arrows)
      (define defs-txt (get-definitions-text))
      (when (and (not (send defs-txt is-transformed?)) 
                 (not (send defs-txt locked-for-write?)))
          ; generate transformed program
;          (define-values (line col pos) (port-next-location ip))
;          (when (string=? "lazyinf" (get-text 6 (sub1 pos)))
        ; use "delay" instead of "lazy" to match paper syntax
        (define delay-str "delay") 
        (define force-str "force")
        (define delay-color "yellow")
        (define force-color "light blue")
        
        ; amt to offset pos due to previously inserted delay/forces
        (define extra-offset 0)
            
            ; inserts open paren + given str at beginning of given stx and highlights
        (define (insert-open str stx color) 
          ; off by 1? syntax pos starts at 0 and editor pos starts at 1 or something?
          (define stx-pos (syntax-position stx))
          (define pos (+ stx-pos -1 extra-offset))
          (define len (string-length str))
          (send defs-txt insert (string-append "(" str " ") pos)
          (send defs-txt save-transform pos (+ len 2))
          (send defs-txt save-transform-highlight
                (send defs-txt highlight-range (+ stx-pos extra-offset)
                      (+ stx-pos extra-offset len) color))
          (set! extra-offset (+ extra-offset (+ len 2))))
        ; insert close paren
        (define (insert-close stx) 
          ; off by 1? syntax pos starts at 0 and editor pos starts at 1 or something?
          (define pos (+ (syntax-position stx) -1 (syntax-span stx) extra-offset))
          (send defs-txt insert ")" pos)
          (send defs-txt save-transform pos 1)
          (set! extra-offset (add1 extra-offset)))
            
        (define ip (open-input-text-editor defs-txt))
        (read-language ip) ;; read #lang
        (define transformed (term (φ ,(port->list read ip))))
        (define arrow-vars (get-arrow-vars))
        (close-input-port ip)
            
        ; update defs window with transformed program
        (define ip-syn (open-input-text-editor defs-txt))
        (port-count-lines! ip-syn) ; need this to handle unicode chars properly
        (read-language ip-syn) ; read # lang
        (let L ([e (read-syntax (object-name ip-syn) ip-syn)]
                [transformed transformed]
                [transformed-no-unξ (get-not-unrenamed)])
          (unless (or (eof-object? e) (null? transformed))
            (let L2 ([e e] [trans-e (car transformed)] 
                           [no-unξ-maybe-labeled (car transformed-no-unξ)]
                           [lazy-context? #f])
              (define no-unξ (strip-label no-unξ-maybe-labeled))
              (if (pair? trans-e) 
                  (cond [(and (eq? (car trans-e) 'lazy) ; found inserted delay
                              (or (not (stx-pair? e))
                                  (not (eq? (syntax->datum (car (syntax->list e))) 'lazy))))
                         (let* ([pos (+ (syntax-position e) extra-offset)]
                                [s (send defs-txt find-snip pos 'before)]
                                [x (box #f)]
                                [y (box #f)])
                           (send defs-txt get-snip-location s x y)
                           (send defs-txt add-arrow-head! (+ 30 (unbox x)) (unbox y))
                           (insert-open delay-str e delay-color)
                           (L2 e (cadr trans-e) (cadr no-unξ) lazy-context?)
                           (insert-close e))]
                        ; found lazy position, mark lazy-context? = #t
                        [(and (stx-pair? e) 
                              (or
                               (eq? (syntax->datum (car (syntax->list e))) 'lazy)
                               (eq? (syntax->datum (car (syntax->list e))) 'lcons)))
                         (map L2 (syntax->list e) trans-e no-unξ 
                              (build-list (length trans-e) (λ _ #t)))]
                        [(and (eq? (car trans-e) 'force) ; found inserted force
                              (or (not (stx-pair? e))
                                  (not (eq? (syntax->datum (car (syntax->list e))) 'force))))
                         (insert-open force-str e force-color)
                         (L2 e (cadr trans-e) (cadr no-unξ) lazy-context?)
                         (insert-close e)]
                        [else
                         (map L2 (syntax->list e) trans-e no-unξ 
                              (build-list (length trans-e) (λ _ lazy-context?)))])
                  ; else check if this var should be arrow target
                  (when (and lazy-context?
                             (member (strip-label no-unξ) arrow-vars))
                    (let* ([pos (+ (syntax-position e) extra-offset)]
                           [s (send defs-txt find-snip pos 'before)]
                           [x (box #f)]
                           [y (box #f)])
                          (send defs-txt get-snip-location s x y)
                          (send defs-txt add-var-loc! 
                           (strip-label no-unξ) (+ 5 (unbox x)) (unbox y))))))
                (L (read-syntax (object-name ip-syn) ip-syn) 
                   (cdr transformed) (cdr transformed-no-unξ))))
            (send defs-txt set-transformed) ; mark as transformed so it will reset on edit
            ))
    
    #;(define/override (execute-callback)
      
      
      ; execute program, then rewrite
      (super execute-callback)
      
      (send defs-txt set-executed!))
    (super-new)))


(define (lazyinf-def-txt-mixin super%)
  (class super%
    (inherit begin-edit-sequence end-edit-sequence 
             ;delete insert highlight-range
             ;get-text find-snip get-snip-location locked-for-write?
             )
    
;    (define executed? #f)
    (define transformed? #f)
    (define transforms null)
    (define transform-highlights null)
    (define arrow-heads null)
    (define var->locs (make-hash))
    (define/public (add-arrow-head! x y) 
      (set! arrow-heads (append arrow-heads (list (cons x y)))))
    (define/public (add-var-loc! var x y)
      (hash-set! var->locs var (cons x y)))
;    (define/public (set-executed!) (set! executed? #t))
        
    (define/public (reset-transform start len [del? #f])
      (set! transformed? #f)
      #;(for-each 
       (λ (x) 
         (if (< start (car x))
             (delete (+ (car x) (if del? (* -1 len) len))
                     (+ (car x) (cdr x) (if del? (* -1 len) len)))
             (delete (car x) (+ (car x) (cdr x))))) 
       transforms)
      #;(for-each (λ (th) (th)) transform-highlights)
      (set! transforms null)
      (set! transform-highlights null))
    (define/public (set-transformed) (set! transformed? #t))
    (define/public (is-transformed?) transformed?)
    (define/public (save-transform pos span) 
      (set! transforms (cons (cons pos span) transforms)))
    (define/public (save-transform-highlight th)
      (set! transform-highlights (cons th transform-highlights)))

    (define/augment (on-insert start len)
      (begin-edit-sequence)
      (inner (void) on-insert start len))
    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (end-edit-sequence)
      (when #f #;transformed? (reset-transform start len)))
    
    (define/augment (on-delete start len)
      (begin-edit-sequence)
      (inner (void) on-delete start len))
    (define/augment (after-delete start len)
      (inner (void) after-delete start len)
      (end-edit-sequence)
      (when #f #;transformed? (reset-transform start len #t)))
    
    #;(define/public (add-highlights-and-arrows)
      (when executed?
        (when (and (not transformed?) (not (locked-for-write?)))
          ; generate transformed program
          (define ip (open-input-string (get-text)))
          (read-language ip) ;; read #lang
          (define-values (line col pos) (port-next-location ip))
          (when (string=? "lazyinf" (get-text 6 (sub1 pos)))
            ; use "delay" instead of "lazy" to match paper syntax
            (define delay-str "delay") 
            (define force-str "force")
            (define delay-color "yellow")
            (define force-color "light blue")
            
            ; amt to offset pos due to previously inserted delay/forces
            (define extra-offset 0)
            
            ; inserts open paren + given str at beginning of given stx and highlights
            (define (insert-open str stx color) 
              ; off by 1? syntax pos starts at 0 and editor pos starts at 1 or something?
              (define stx-pos (syntax-position stx))
              (define pos (+ stx-pos -1 extra-offset))
              (define len (string-length str))
              (insert (string-append "(" str " ") pos)
              (save-transform pos (+ len 2))
              (save-transform-highlight
               (highlight-range (+ stx-pos extra-offset)
                                (+ stx-pos extra-offset len) color))
              (set! extra-offset (+ extra-offset (+ len 2))))
            ; insert close paren
            (define (insert-close stx) 
              ; off by 1? syntax pos starts at 0 and editor pos starts at 1 or something?
              (define pos (+ (syntax-position stx) -1 (syntax-span stx) extra-offset))
              (insert ")" pos)
              (save-transform pos 1)
              (set! extra-offset (add1 extra-offset)))
            
            (define transformed (term (φ ,(port->list read ip))))
            (define arrow-vars (get-arrow-vars))
            (close-input-port ip)
            
            ; update defs window with transformed program
            (define ip-syn (open-input-string (get-text)))
            (port-count-lines! ip-syn) ; need this to handle unicode chars properly
            (read-language ip-syn) ; read # lang
            (let L ([e (read-syntax (object-name ip-syn) ip-syn)]
                    [transformed transformed]
                    [transformed-no-unξ (get-not-unrenamed)])
              (unless (or (eof-object? e) (null? transformed))
                (let L2 ([e e] [trans-e (car transformed)] 
                               [no-unξ-maybe-labeled (car transformed-no-unξ)]
                               [lazy-context? #f])
                  (define no-unξ (strip-label no-unξ-maybe-labeled))
                  (if (pair? trans-e) 
                      (cond [(and (eq? (car trans-e) 'lazy) ; found inserted delay
                                  (or (not (stx-pair? e))
                                      (not (eq? (syntax->datum (car (syntax->list e))) 'lazy))))
                             (let* ([pos (+ (syntax-position e) extra-offset)]
                                    [s (find-snip pos 'before)]
                                    [x (box #f)]
                                    [y (box #f)])
                               (get-snip-location s x y)
                               (add-arrow-head! (+ 30 (unbox x)) (unbox y))
                               (insert-open delay-str e delay-color)
                               (L2 e (cadr trans-e) (cadr no-unξ) lazy-context?)
                               (insert-close e))]
                            ; found lazy position, mark lazy-context? = #t
                            [(and (stx-pair? e) 
                                  (or
                                   (eq? (syntax->datum (car (syntax->list e))) 'lazy)
                                   (eq? (syntax->datum (car (syntax->list e))) 'lcons)))
                             (map L2 (syntax->list e) trans-e no-unξ 
                                  (build-list (length trans-e) (λ _ #t)))]
                            [(and (eq? (car trans-e) 'force) ; found inserted force
                                  (or (not (stx-pair? e))
                                      (not (eq? (syntax->datum (car (syntax->list e))) 'force))))
                             (insert-open force-str e force-color)
                             (L2 e (cadr trans-e) (cadr no-unξ) lazy-context?)
                             (insert-close e)]
                          [else
                           (map L2 (syntax->list e) trans-e no-unξ 
                                (build-list (length trans-e) (λ _ lazy-context?)))])
                      ; else check if this var should be arrow target
                      (when (and lazy-context?
                                 (member (strip-label no-unξ) arrow-vars))
                        (let* ([pos (+ (syntax-position e) extra-offset)]
                               [s (find-snip pos 'before)]
                               [x (box #f)]
                               [y (box #f)])
                          (get-snip-location s x y)
                          (add-var-loc! 
                           (strip-label no-unξ) (+ 5 (unbox x)) (unbox y))))))
                (L (read-syntax (object-name ip-syn) ip-syn) 
                   (cdr transformed) (cdr transformed-no-unξ))))
            (set-transformed) ; mark as transformed so it will reset on edit
            (set! executed? #f)
            ))))
            
    (define/override (on-paint before dc left top right bottom dx dy draw-caret)
      (when transformed?
        (define arrow-vars (get-arrow-vars))
        (for ([tl arrow-vars]
              [hd arrow-heads])
          (define-values (x1 y1) (values (car hd) (cdr hd)))
          (define loc (hash-ref var->locs tl (λ _ (cons 0 0))))
          (define-values (x2 y2) (values (car loc) (cdr loc)))
          (if (< y1 y2)
              (set! y1 (+ 17 y1)) ; arrow starts at underside when y1 is <
              (set! y2 (+ y2 15))) ; arrow points to underside when y1 is >
          (draw-arrow dc (+ x1 30) y1 x2 y2 dx dy #:pen-width 1)))
      (super on-paint before dc left top right bottom dx dy draw-caret)
      )

        
    (super-new)))


(define-unit tool@
  (import drracket:tool^)
  (export drracket:tool-exports^)
  (define (phase1) 
    (drracket:module-language-tools:add-opt-out-toolbar-button
       (λ (frame parent)
         (new switchable-button%
               (label "Fix Laziness")
               (bitmap (right-over-arrow-icon #:color metal-icon-color 
                                              #:height (toolbar-icon-height)))
               (alternate-bitmap (right-over-arrow-icon #:color metal-icon-color 
                                                        #:height (toolbar-icon-height)))
               (parent parent)
               (callback (lambda (button) (send frame add-highlights-and-arrows)))))
       'lazyinf
       #:number 71))
  (define (phase2) (void))
  (drracket:get/extend:extend-unit-frame lazyinf-unit-frame-mixin)
  (drracket:get/extend:extend-definitions-text lazyinf-def-txt-mixin))
