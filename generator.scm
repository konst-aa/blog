(import args
        ersatz
        lowdown
        (chicken file)
        (chicken port)
        (chicken io)
        (chicken process-context)
        (chicken pathname)
        (chicken string))

(define opts
  (list (args:make-option (h help) #:none "Display this text" (usage))
        (args:make-option (f folder) #:required "Specify a folder of markdown files")
        (args:make-option (t template) #:required "Specify a template file")
        (args:make-option (o output) #:required "Specify an output folder")))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [options...] [files...]")
      (newline)
      (print (args:usage opts))))
  (exit 1))

(define-values (options operands)
  (args:parse (command-line-arguments) opts))

(define template (alist-ref 't options equal? #f))

(if (not template)
  (usage))

(define output-path
  (let ((out-folder (alist-ref 'o options equal? #f)))
       (cond
         ((not out-folder) ".")
         ((directory-exists? out-folder) out-folder)
         (else
           (create-directory out-folder)
           out-folder))))

(define posts
  (let ((markdown-folder (alist-ref 'f options equal? #f)))
       (append operands (if markdown-folder
                          (map (lambda (d) (string-append markdown-folder "/" d))
                               (directory markdown-folder)) '()))))

(map (lambda (f)
       (let* ((md (with-input-from-file f (lambda () (read-string #f))))
              (html (with-output-to-string (lambda () (markdown->html md))))
              (op (string-append output-path "/" (pathname-file f) ".html")))
             (with-output-to-file op
              (lambda () (display (from-file template models: `((markdown . ,(Tstr html)))))))
             ))
     posts)
;(display (markdown->html "*Hello!*"))
