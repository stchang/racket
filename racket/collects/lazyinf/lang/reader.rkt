#lang s-exp syntax/module-reader
racket-with-lazyinf

#:info make-info

(define (make-info key default use-default)
  (case key
    [(drracket:toolbar-buttons)
     (list (dynamic-require 'racket-with-lazyinf/tool 'lazyinf-button))]
    [else (use-default key default)]))