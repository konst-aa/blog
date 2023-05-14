(import args
        ersatz
        json
        srfi-69
        (chicken file)
        (chicken format)
        (chicken port)
        (chicken io)
        (chicken process)
        (chicken process-context)
        (chicken pathname)
        (chicken string))

(define opts
  (list (args:make-option (t template) #:required "Mandatory. Specify a template file")
        (args:make-option (p posts) #:required "Mandatory. Specify a json file for categories and dates")
        (args:make-option (h help) #:none "Display this text" (usage))
        (args:make-option (f folder) #:required "Specify a folder of markdown files")
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
(define posts (alist-ref 'p options equal? #f))

(if (not template)
  (usage))

(if (not posts)
  (usage))

(define output-path
  (let ((out-folder (alist-ref 'o options equal? #f)))
    (cond
      ((not out-folder) ".")
      ((directory-exists? out-folder) out-folder)
      (else
        (create-directory out-folder)
        out-folder))))

; https://gist.github.com/dhess/52681
(define (json-read-fixup jro)
  (cond ((null? jro) jro)
        ((vector? jro) (json-read-fixup (vector->list jro)))
        ((pair? jro) (cons (json-read-fixup (car jro))
                           (json-read-fixup (cdr jro))))
        (else jro)))

(set! posts (json-read-fixup (with-input-from-file posts json-read)))

; thank you to oaktownsam of the scheme discord for helping me with this fn!
(define (markdown->html md)
  (let-values (((pandoc-out pandoc-in pandoc-pid) (process "pandoc -f gfm -t html")))
    (write-string md #f pandoc-in)
    (close-output-port pandoc-in)
    (read-string #f pandoc-out)))

; create the pages that are per-category
(define by-category
  (foldl (lambda (acc post-entry)
           (foldl (lambda (acc c)
                    (alist-update c (cons post-entry (alist-ref c acc equal? '())) acc equal?))
                  acc
                  (alist-ref "categories" (cdr post-entry) equal?)))
         '()
         posts))

(define input-posts
  (let* ((markdown-folder (alist-ref 'f options equal? #f))
         (folder-posts
           (if markdown-folder
             (map (lambda (d) (string-append markdown-folder "/" d))
                  (directory markdown-folder)) '())))
    (append operands folder-posts)))

(define (link-post post-entry)
  (let* ((name (car post-entry))
         (info (cdr post-entry))
         (link (string-append (pathname-file (alist-ref "relpath" info equal?)) ".html")))
    (sprintf "[~A](~A)  \n" name link)))

(define (generate-category group port)
  (let* ((joined (apply string-append (map link-post (cdr group))))
         (html (markdown->html (string-append (sprintf "## ~A:  \n" (car group)) joined))))
    (display (from-file template models: `((markdown . ,(Tstr html)))) port)))

; write category pages
(map (lambda (category-group)
       (call-with-output-file
         (string-append output-path "/" (car category-group) ".html")
         (lambda (port) (generate-category category-group port))))
     by-category)

; write posts
(map (lambda (post)
       (let* ((md (with-input-from-file post (lambda () (read-string #f))))
              (html (markdown->html md)))
         (call-with-output-file
           (string-append output-path "/" (pathname-file post) ".html")
           (lambda (port) (display (from-file template models: `((markdown . ,(Tstr html)))) port)))))
     input-posts)

; write index.html
(call-with-output-file
  (string-append output-path "/index.html")
  (lambda (port)
    (let* ((category-string
             (map (lambda (c)
                    (sprintf "[~A](~A) " (car c) (string-append (car c) ".html")))
                  by-category))
           (md-text (apply string-append (map link-post posts)))
           (category-text (apply string-append category-string))
           (md (sprintf "## Categories:  \n~A  \n ## All Posts:  \n~A \n" category-text md-text))
           (html (markdown->html md)))
      (display (from-file template models: `((markdown . ,(Tstr html)))) port))))
