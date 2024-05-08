(import (chicken process-context)
        (chicken pathname)
        (chicken format)
        (srfi 1)
        (chicken string))

(define args (append (command-line-arguments) (list #f)))

(if (< (length args) 2)
  (begin
    (display "Usage: blog-gen [mode] posts.scm [post.md]\n")
    (exit 1)))

(define mode (first args))
(define posts-file (second args))
(define posts (with-input-from-file posts-file read))
(define post-markdown-file (third args))

(define (read-all port)
  (define (loop lst)
    (let ((line (read-char port)))
      (if (eof-object? line)
        (reverse lst)
        (loop (cons line lst)))))
  (list->string (loop '())))

(define (alist-refp key alist)
  (alist-ref key alist equal? #f))

;; list of posts by category
(define (get-categories post)
  (alist-refp 'categories (cdr post)))

(define by-category
  (map (lambda (c)
         (cons c (filter (lambda (x) (member c (get-categories x))) posts)))
       (delete-duplicates (apply append (map get-categories posts)))))

(define (date->date-setence date)
  (let* ((date (string-split date "-"))
         (year (car date))
         (month (cadr date))
         (day (caddr date)))
    (string-append "*Posted on " month "/" day "/" year "*")))

(define (link-post post-entry)
  (let* ((relpath (car post-entry))
         (info (cdr post-entry))
         (title (alist-refp 'title info))
         (link (string-append (pathname-strip-extension relpath) ".html"))
         (date (alist-refp 'date-published info))
         (date-sentence (date->date-setence date))
         (spoiler (alist-refp 'spoiler info)))
    (sprintf "**[~A](~A)**  \n~A  \n  \n~A...  \n\n" title link date-sentence spoiler)))

(define (safe-take lst n)
  (take lst (min n (length lst))))

(define (print-category group port)
  (let* ((joined (apply string-append (map link-post group))))
    (display joined port)))


(case (string->symbol mode)
  ((index)
   ;;; write index.md
   (call-with-output-file
     "index.md"
     (lambda (port)
       (let* ((link-category 
               (lambda (c) (sprintf "[~A](~A)  \n" c (string-append c ".html"))))
              (category-links (map link-category (map car by-category)))
              (md (sprintf "## Categories:  \n~A  \n ## All Posts:  \n~A \n"
                           (apply string-append category-links)
                           (apply string-append (map link-post (reverse posts))))))
         (display md port))))
   ;;; write category markdown
   (map (lambda (category-pair)
          (call-with-output-file
            (string-append (car category-pair) ".md")
            (lambda (port)
              (print-category (cdr category-pair) port))))
        by-category))

  ((related)
   (let* ((post (alist-refp post-markdown-file posts))
          (categories (alist-refp 'categories post))
          (not-self (lambda (p) (not (equal? (car p) post-markdown-file)))))
     (map-in-order
       (lambda (c)
         (printf "## More from ~A  \n" c)
         (print-category (safe-take (filter not-self (alist-refp c by-category)) 3)
                         (current-output-port)))
       categories)))

  ((spoiler)
   (let* ((post-text (read-all (open-input-file post-markdown-file)))
          (post (alist-refp post-markdown-file posts))
          (spoiler (alist-refp 'spoiler post))
          (end-spoiler-offset (string-length "# ENDSPOILER"))
          (end-spoiler-index (substring-index "# ENDSPOILER" post-text))
          (default ""))
     (if end-spoiler-index
       (begin
         (set! spoiler (substring post-text 0 end-spoiler-index))
         (set! post-text
           (substring post-text (+ end-spoiler-index end-spoiler-offset)))))

     (set! spoiler (or spoiler default))
     (set-cdr! (last-pair post)
               (list (cons 'spoiler spoiler)))
     (call-with-output-file posts-file
                            (lambda (port) (write posts port)))
     (display post-text)))
  (else
    (display "Unknown mode. Available: indexes, related, spoiler")))

