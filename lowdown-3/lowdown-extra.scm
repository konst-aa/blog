(include "backwards-compatible-module" )

(backwards-compatible-module (lowdown extra)

(enable-lowdown-extra!
 inline-note
 lowdown-extra-html-conversion-rules*)

(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (use srfi-1
        comparse
        lowdown
        lowdown-lolevel
        clojurian-syntax
        (prefix fancypants fancypants-)))
  (chicken-5
   (import (chicken base)
           (srfi 1)
           (comparse)
           (lowdown)
           (lowdown lolevel)
           (clojurian syntax)
           (prefix (fancypants) fancypants-))))

(define inline-note
  (enclosed-by (char-seq "^[")
               (->> (none-of* (is #\]) inline)
                    (one-or-more)
                    (node 'inline-note))
               (char-seq "]")))

(define fancypants
  (any-of
   (map (lambda (mapping)
          (preceded-by (char-seq (car mapping))
                       (result (cdr mapping))))
        fancypants-default-map)))

(define lowdown-extra-html-conversion-rules*
  `((inline-note . ,(lambda (_ contents)
                      `(span (@ (class "note")) . ,contents)))))

(define (enable-lowdown-extra!)
  (inline-hook (cons* inline-note fancypants (inline-hook)))
  (markdown-html-conversion-rules*
   (append lowdown-extra-html-conversion-rules*
           (markdown-html-conversion-rules*)))
  (void))

)
