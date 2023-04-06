(cond-expand
  (chicken-4
    (import-for-syntax matchable chicken)
    (begin-for-syntax (use srfi-69)))
  (chicken-5
    (import (chicken syntax))
    (import-for-syntax (chicken base) matchable srfi-69)))

(define-for-syntax generics (make-hash-table))

(define-for-syntax (arity sig)
  (let loop ((sig sig)
             (arity 0))
    (cond ((pair? sig)
           (loop (cdr sig)
                 (+ arity 1)))
          ((null? sig)
           arity)
          (else
           (- arity)))))

(define-for-syntax expand-generic
  (match-lambda*
    ((name ((arg . args) . impls))
     `(define (,name ,arg . ,args)
        (cond ,@impls
              (else (error "No implementation for generic" ',(strip-syntax name) ,arg)))))))

(define-syntax define-generic
  (ir-macro-transformer
   (lambda (exp i c)
     (match exp
       ((_ (name (pred arg) . args) body ...)
        (match-let* ((name* (strip-syntax name))
                     (sig (cons arg args))
                     ((given-sig . impls) (hash-table-ref/default generics name* (list sig))))
          (unless (= (arity sig) (arity given-sig))
            (syntax-error 'define-generic
                          "Generics must have the same signature"
                          (list got: (strip-syntax sig))
                          (list expected: (strip-syntax given-sig))))
          (let ((sig+impls (cons sig (cons (cons (list pred arg) body) impls))))
            (hash-table-set! generics name* sig+impls)
            (expand-generic name sig+impls))))))))
