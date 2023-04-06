;; Inspired by:
;; https://github.com/jgm/peg-markdown/blob/master/markdown_parser.leg
;; https://github.com/jgm/pandoc/blob/master/src/Text/Pandoc/Readers/Markdown.hs

(module lowdown

(markdown->sxml
 markdown->sxml*
 markdown-sxml->html-sxml
 markdown->html
 markdown-html-conversion-rules*)

(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (use data-structures
        irregex
        srfi-1
        clojurian-syntax
        comparse
        sxml-transforms
        lowdown-lolevel))
  (chicken-5
   (import (chicken base)
           (chicken irregex)
           (scheme)
           (srfi 1)
           (clojurian syntax)
           (comparse)
           (lowdown lolevel)
           (sxml-transforms))))

(define (maybe-ref key alist)
  (and-let* ((value (alist-ref key alist)))
    (and (not (and (not (car value))
                   (null? (cdr value))))
         value)))

(define (maybe-attr-ref attr attrs)
  (let ((value (maybe-ref attr attrs)))
    (if value
        (list (cons attr value))
        '())))

(define (reference-element? el)
  (and (pair? el) (eq? 'reference (car el))))

(define (normalize-label label)
  (define trim-irx '(or (: bol (+ space)) (: (+ space) eol)))
  (let ((trimmed (irregex-replace/all trim-irx (apply string-append label))))
    (irregex-replace/all '(+ space) trimmed " ")))

(define references
  (make-parameter #f))

(define (call-with-reference attrs proc)
  (let* ((ref (alist-ref 'ref attrs))
         (ref (and (pair? ref)
                   (alist-ref (normalize-label ref)
                              (references)
                              equal?))))
    (if ref
        (proc ref attrs)
        (alist-ref 'input attrs))))

(define (make-image ref #!optional attrs)
  `(img (@ (src ,(alist-ref 'href ref))
           (alt . ,(alist-ref 'label (or attrs ref)))
           . ,(maybe-attr-ref 'title ref))))

(define (make-anchor ref #!optional attrs)
  `(a (@ (href ,@(alist-ref 'href ref))
         . ,(maybe-attr-ref 'title ref))
      . ,(alist-ref 'label (or attrs ref))))

(define markdown-html-conversion-rules*
  (make-parameter
   `((explicit-link . ,(lambda (_ attrs)
                         (make-anchor attrs)))
     (reference-link . ,(lambda (_ attrs)
                          (call-with-reference attrs make-anchor)))
     (auto-link . ,(lambda (_ attrs)
                     `(a (@ (href . ,(alist-ref 'href attrs)))
                         . ,(alist-ref 'label attrs))))
     (image . ,(lambda (_ attrs)
                 (make-image attrs)))
     (reference-image . ,(lambda (_ attrs)
                           (call-with-reference attrs make-image)))
     (verbatim . ,(lambda (_ contents)
                    `(pre (code . ,contents))))
     (bullet-list . ,(lambda (_ items)
                       `(ul . ,items)))
     (ordered-list . ,(lambda (_ items)
                        `(ol . ,items)))
     (item . ,(lambda (_ contents)
                `(li . ,contents)))
     (heading . ,(lambda (_ contents)
                   (cons (->> (number->string (car contents))
                              (string-append "h")
                              (string->symbol))
                         (cdr contents))))
     (paragraph . ,(lambda (_ contents)
                     `(p . ,contents)))
     (emphasis . ,(lambda (_ text)
                    `(em . ,text)))
     (strong . ,(lambda (_ text)
                  `(strong . ,text)))
     (html-element . ,(lambda (_ contents)
                        contents))
     (comment . ,(lambda (_ contents)
                   (cons '*COMMENT* contents)))
     . ,alist-conv-rules*)))

(define (ref->alist-entry ref)
  (cons (normalize-label (alist-ref 'label (cdr ref)))
        (cdr ref)))

(define (markdown-sxml->html-sxml markdown-sxml)
  (receive (refs sxml) (partition reference-element? markdown-sxml)
    (parameterize ((references (map ref->alist-entry refs)))
      (pre-post-order* sxml (markdown-html-conversion-rules*)))))

(define (markdown->sxml* #!optional (input (current-input-port)) (memoize? #t))
  (parse document input memoize: memoize?))

(define (markdown->sxml #!optional (input (current-input-port)) (memoize? #t))
  (receive (result remainder)
    (markdown->sxml* input memoize?)
    (values (markdown-sxml->html-sxml result) remainder)))

(define html-serialization-rules*
  `((*COMMENT* . ,(lambda (_ contents)
                    (list #\< "!--" contents "--" #\> #\newline)))
    ,@universal-conversion-rules*))

(define (markdown->html #!optional (input (current-input-port)) (memoize? #t))
  (receive (result remainder)
    (markdown->sxml input memoize?)
    (values (-> result
                (pre-post-order* html-serialization-rules*)
                (SRV:send-reply))
            remainder)))

)
