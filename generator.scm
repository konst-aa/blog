(import args
        ersatz
        json
        srfi-69
        srfi-1
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

;; https://gist.github.com/dhess/52681
(define (json-read-fixup jro)
  (cond ((null? jro) jro)
        ((vector? jro) (json-read-fixup (vector->list jro)))
        ((pair? jro) (cons (json-read-fixup (car jro))
                           (json-read-fixup (cdr jro))))
        (else jro)))

(set! posts (json-read-fixup (with-input-from-file posts json-read)))

;; thank you to oaktownsam of the scheme discord for helping me with this fn!
(define (markdown->html md)
  (let-values (((pandoc-out pandoc-in pandoc-pid) (process "pandoc -f gfm -t html")))
    (write-string md #f pandoc-in)
    (close-output-port pandoc-in)
    (read-string #f pandoc-out)))

(define (alist-refp key alist)
  (alist-ref key alist equal? #f))

;; create the pages that are per-category
(define by-category
  (foldl (lambda (acc post-entry)
           (foldl
             (lambda (acc c)
               (alist-update c (cons post-entry (alist-ref c acc equal? '())) acc equal?))
             acc
             (alist-refp "categories" (cdr post-entry))))
         '()
         posts))


(define input-posts
  (let* ((markdown-folder (alist-refp 'f options))
         (folder-posts
           (if markdown-folder
             (map (lambda (d) (string-append markdown-folder "/" d))
                  (directory markdown-folder))
             '())))
    (append operands folder-posts)))

(define markdown-folder (string-append (alist-ref 'f options) "/"))

(define (date-sentence-from-date date)
  (let* ((date (string-split date "-"))
         (year (car date))
         (month (cadr date))
         (day (caddr date)))
    (string-append "*Posted on " month "/" day "/" year "*")))

(define (post-and-spoiler post-path)
  (let* ((md (with-input-from-file post-path (lambda () (read-string #f))))
         (delim-loc (or (substring-index "# ENDSPOILER" md) 0))
         (spoiler (if (= delim-loc 0)
                    ""
                    (substring (substring md 0 delim-loc) (string-length "# SPOILER"))))
         (content (if (= delim-loc 0)
                    md
                    (substring md (+ delim-loc (string-length "# ENDSPOILER"))))))
    (cons spoiler content)))

(define (link-post post-entry)
  (let* ((name (car post-entry))
         (info (cdr post-entry))
         (relpath (alist-refp "relpath" info))
         (link (string-append (pathname-file relpath) ".html"))
         (post-path (string-append markdown-folder relpath))
         (date-sentence (date-sentence-from-date (alist-refp "date-published" info)))
         (spoiler (car (post-and-spoiler post-path))))
    (sprintf "**[~A](~A)**  \n~A  \n~A  \n" name link date-sentence spoiler)))

(define (generate-category group port)
  (let* ((joined (apply string-append (map link-post (cdr group))))
         (html (markdown->html (string-append (sprintf "## ~A:  \n" (car group)) joined))))
    (display (from-file template models: `((markdown . ,(Tstr html)))) port)))

;; write category pages
(map (lambda (category-group)
       (call-with-output-file
         (string-append output-path "/" (car category-group) ".html")
         (lambda (port) (generate-category category-group port))))
     by-category)

(define (safe-take lst n)
  (if (or (null? lst) (= n 0))
    '()
    (cons (car lst) (safe-take (cdr lst) (- n 1)))))

(define (related-posts post)
  (define (recent-by-category acc category)
    (let* ((filtered-posts
             (filter (lambda (post) (not (member (car post) (car acc) equal?)))
                     (alist-refp category by-category)))
           (added-posts (safe-take filtered-posts 3))
           (rec-string
             (sprintf "## More from ~A:\n~A"
                      category
                      (string-intersperse (map link-post added-posts) "\n"))))
      (cons (append (car acc) (map car added-posts))
            (append (cdr acc) (list rec-string)))))
  (apply string-append (cdr (foldl recent-by-category '(() . ()) (alist-refp "categories" (cdr post))))))

;; write posts
(map (lambda (post)
       (let* ((info (cdr post))
              (post-path (string-append markdown-folder (alist-refp "relpath" info)))
              (date-sentence (date-sentence-from-date (alist-refp "date-published" info)))
              (content (cdr (post-and-spoiler post-path)))
              (related-posts (related-posts post))
              (html (markdown->html (string-append content "  \n" date-sentence "  \n" related-posts))))
         (call-with-output-file
           (string-append output-path "/" (pathname-file post-path) ".html")
           (lambda (port) (display (from-file template models: `((markdown . ,(Tstr html)))) port)))))
     posts)

;; write index.html
(call-with-output-file
  (string-append output-path "/index.html")
  (lambda (port)
    (let* ((category-string
             (map (lambda (c)
                    (sprintf "[~A](~A)  \n" (car c) (string-append (car c) ".html")))
                  by-category))
           (post-links (apply string-append (map link-post (reverse posts))))
           (category-text (apply string-append category-string))
           (md (sprintf "## Categories:  \n~A  \n ## All Posts:  \n~A \n" category-text post-links))
           (html (markdown->html md)))
      (display (from-file template models: `((markdown . ,(Tstr html)))) port))))
