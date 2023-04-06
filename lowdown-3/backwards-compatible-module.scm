(define-syntax backwards-compatible-module
  (ir-macro-transformer
   (lambda (x i c)
     (cond-expand
       (chicken-4
        (let ((name (strip-syntax (cadr x))))
          `(module ,(i (if (pair? name)
                           (string->symbol
                            (string-intersperse
                             (map symbol->string name)
                             "-"))
                           name))
,@(cddr x))))
       (chicken-5
        `(module ,@(cdr x)))))))
