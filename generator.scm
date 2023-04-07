(import args
        ersatz
        json
        lowdown
        srfi-69
        (chicken file)
        (chicken format)
        (chicken port)
        (chicken io)
        (chicken process-context)
        (chicken pathname)
        (chicken string))

(define opts
  (list (args:make-option (h help) #:none "Display this text" (usage))
        (args:make-option (f folder) #:required "Specify a folder of markdown files")
        (args:make-option (t template) #:required "Specify a template file")
        (args:make-option (o output) #:required "Specify an output folder")
        (args:make-option (a articles) #:required "Specify a json file for categories and dates")))

(define (usage)
  (with-output-to-port (current-error-port)
    (lambda ()
      (print "Usage: " (car (argv)) " [options...] [files...]")
      (newline)
      (print (args:usage opts))))
  (exit 1))

(define-values (options operands)
  (args:parse (command-line-arguments) opts))

; https://gist.github.com/dhess/52681
(define (json-read-fixup jro)
  (cond ((null? jro) jro)
        ((vector? jro) (json-read-fixup (vector->list jro)))
        ((pair? jro) (cons (json-read-fixup (car jro))
                           (json-read-fixup (cdr jro))))
        (else jro)))

(define template (alist-ref 't options equal? #f))
(define articles (alist-ref 'a options equal? #f))

(if (not template)
  (usage))

(if (not articles)
  (usage))

(set! articles (json-read-fixup (with-input-from-file articles json-read)))

(define by-category
  (foldl (lambda (acc item)
           (foldl (lambda (acc c)
                    (alist-update c (append (list item) (alist-ref c acc equal? '())) acc equal?))
                  acc
                  (alist-ref "categories" (cdr item) equal?)))
         '()
         articles))

(define output-path
  (let ((out-folder (alist-ref 'o options equal? #f)))
       (cond
         ((not out-folder) ".")
         ((directory-exists? out-folder) out-folder)
         (else
           (create-directory out-folder)
           out-folder))))

; create the pages that are per-category
(define posts
  (let ((markdown-folder (alist-ref 'f options equal? #f)))
       (append operands (if markdown-folder
                          (map (lambda (d) (string-append markdown-folder "/" d))
                               (directory markdown-folder)) '()))))

(map (lambda (category-group)
       (with-output-to-file
         (string-append output-path "/" (car category-group) ".html")
         (lambda ()
           (let* ((joined (string-append
                            (sprintf "## ~A:  \n" (car category-group))
                            (apply
                              string-append
                              (map (lambda (article)
                                     (sprintf "[~A](~A)  \n"
                                              (car article)
                                              (string-append (pathname-file (alist-ref "relpath" (cdr article) equal?)) ".html")))
                                   (cdr category-group)))))
                  (html (with-output-to-string (lambda () (markdown->html joined)))))
                 (display (from-file template models: `((markdown . ,(Tstr html)))))))))
     by-category)


(map (lambda (f)
       (let* ((md (with-input-from-file f (lambda () (read-string #f))))
              (html (with-output-to-string (lambda () (markdown->html md)))))
             (with-output-to-file
               (string-append output-path "/" (pathname-file f) ".html")
               (lambda () (display (from-file template models: `((markdown . ,(Tstr html)))))))))
     posts)

(with-output-to-file
  (string-append output-path "/index.html")
  (lambda ()
    (let* ((md-list
             (map (lambda (a)
                    (let* ((name (car a))
                           (info (cdr a))
                           (f (string-append (pathname-file (alist-ref "relpath" info equal?)) ".html")))
                          (sprintf "[~A](~A)  \n" name f)))
                  articles))
           (category-list
             (apply
               string-append
               (map (lambda (c)
                      (sprintf "[~A](~A) " (car c) (string-append (car c) ".html")))
                    by-category)))
           (md (apply string-append
                      (append
                        (list (sprintf "## Categories:  \n~A  \n" category-list)
                              " \n## All Posts:  \n")
                        md-list)))
           (html (with-output-to-string (lambda () (markdown->html md)))))
          (display (from-file template models: `((markdown . ,(Tstr html))))))))
