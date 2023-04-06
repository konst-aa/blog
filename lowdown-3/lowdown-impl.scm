(import scheme)

(cond-expand
  (chicken-4
   (import chicken)
   (use srfi-1
        srfi-13
        srfi-14
        srfi-69
        data-structures
        irregex
        comparse
        clojurian-syntax
        sxml-transforms))
  (chicken-5
   (import (chicken base)
           (chicken irregex)
           (srfi 1)
           (srfi 13)
           (srfi 14)
           (srfi 69)
           (clojurian syntax)
           (comparse)
           (sxml-transforms))))

(define (node el parser)
  (sequence* ((content parser))
    (result (if (list? content)
                (cons el content)
                (list el content)))))

(define end-of-input*
  (none-of* item (result "\n")))

(define space-chars
  (char-set #\space #\tab))

(define space-char
  (preceded-by (in space-chars)
               (result " ")))

(define space*
  (as-string (zero-or-more space-char)))

(define space+
  (as-string (one-or-more space-char)))

(define new-line
  (any-of (preceded-by (is #\newline)
                       (result "\n"))
          (preceded-by (is #\return)
                       (maybe (is #\newline))
                       (result "\n"))))

(define space-new-line
  (sequence space* (maybe (preceded-by new-line space*))))

(define normal-line-end
  (preceded-by space* new-line))

(define terminal-line-end
  (preceded-by space* (zero-or-more new-line) end-of-input*))

(define line-break
  (preceded-by (char-seq "  ") normal-line-end (result '(br))))

(define line-end
  (any-of line-break
          terminal-line-end
          normal-line-end))

(define blank-line
  (preceded-by space* new-line))

(define non-indent-space
  (repeated (is #\space) max: 3))

(define special-chars
  (char-set #\* #\_ #\`
            #\& #\[ #\]
            #\( #\) #\<
            #\! #\# #\\
            #\' #\| #\"
            #\- #\. #\^))

(define special-char
  (in special-chars))

(define normal-chars
  (char-set-complement!
   (char-set-union
    special-chars
    space-chars
    (char-set #\newline))))

(define normal-char
  (in normal-chars))

(define non-space-char
  (none-of* space-char new-line item))

(define (parse-inlines input)
  (receive (value remainder)
    (parse (zero-or-more inline-or-normal-line-end) input)
    (if (and value (parser-input-end? remainder))
        (result value)
        fail)))

(define link-title
  (sequence* ((delimiter (in #\' #\"))
              (title (-> (preceded-by
                          (is delimiter)
                          space*
                          (any-of (is #\)) new-line))
                         (none-of* item)
                         (zero-or-more)
                         (as-string)))
              (_ (is delimiter)))
    (result title)))

(define link-label*
  (recursive-parser
   (enclosed-by (is #\[)
                (sequence* ((text (char-seq-until (char-set #\[ #\])))
                            (text (let ((len (string-length text)))
                                    (if (and (> len 0) (eq? #\\ (string-ref text (sub1 len))))
                                        fail
                                        (parse-inlines text))))
                            (label (maybe link-label*)))
                  (result (if label
                              (append text (list "[") label (list "]"))
                              text)))
                (is #\]))))

(define link-label
  (memoize link-label*))

(define explicit-link-source-contents
  (recursive-parser
   (-> (in #\( #\) #\>)
       (none-of* non-space-char)
       (one-or-more)
       (any-of (sequence (is #\() explicit-link-source-contents (is #\))))
       (zero-or-more)
       (as-string))))

(define explicit-link-source
  (any-of (enclosed-by (is #\<)
                       explicit-link-source-contents
                       (is #\>))
          explicit-link-source-contents))

(define explicit-link
  (sequence*
      ((label link-label)
       (_ space-new-line)
       (href (preceded-by (is #\()
                          explicit-link-source))
       (title (maybe (preceded-by space+ link-title)))
       (_ (preceded-by space* (is #\)))))
    (result `(explicit-link
              (href    ,href)
              (label . ,label)
              (title   ,title)))))

(define shortcut-reference-link
  (sequence* ((label (-> link-label
                         (followed-by (none-of (is #\:))))))
    (result `(reference-link
              (input "[" ,@label "]")
              (ref   . ,label)
              (label . ,label)))))

(define reference-link
  (sequence* ((label link-label)
              (space (as-string space-new-line))
              (ref   link-label))
    (result `(reference-link
              (input "[" ,@label "]"
                     ,space
                     "[" ,@ref "]")
              (ref . ,(if (null? ref)
                          label
                          ref))
              (label . ,label)))))


(define auto-link-email
  (sequence* ((_ (preceded-by (is #\<) (maybe (char-seq "mailto:"))))
              (email (-> (sequence
                           (one-or-more (in #[-a-zA-Z0-9+_./!%~$]))
                           (is #\@)
                           (one-or-more (none-of* new-line (is #\>) item)))
                         (as-string)))
              (_ (is #\>)))
    (result `(auto-link
              (href ,(string-append "mailto:" email))
              (label ,email)))))

(define auto-link-url
  (sequence* ((_ (is #\<))
              (uri (-> (sequence (one-or-more (in #[a-zA-Z]))
                                 (char-seq "://")
                                 (-> (none-of* new-line (is #\>) item)
                                     (one-or-more)))
                       (as-string)))
              (_ (is #\>)))
    (result `(auto-link
              (href ,uri)
              (label ,uri)))))

(define auto-link
  (any-of auto-link-url auto-link-email))

(define link
  (any-of explicit-link
          reference-link
          shortcut-reference-link
          auto-link))

(define (line-of char)
  (any-of (repeated (is char) min: 4)
          (-> (sequence space-char (one-or-more (is char)))
              (followed-by space-char))))

(define (surrounded-by char parser)
  (let ((close (enclosed-by
                (none-of space-char new-line)
                inline-or-normal-line-end
                parser)))
    (preceded-by (none-of (line-of char))
                 parser
                 (none-of space-char new-line)
                 (sequence* ((x (-> (none-of* close
                                              (repeated blank-line min: 2)
                                              inline-or-normal-line-end)
                                    (zero-or-more)))
                             (y close))
                   (result (append x (list y)))))))

(define emph
  (recursive-parser
   (->> (any-of (surrounded-by #\* (is #\*))
                (surrounded-by #\_ (is #\_)))
        (node 'emphasis))))

(define strong
  (recursive-parser
   (->> (any-of (surrounded-by #\* (char-seq "**"))
                (surrounded-by #\_ (char-seq "__")))
        (node 'strong))))

(define ul-or-star-line
  (any-of (line-of #\*) (line-of #\_)))

(define escaped-char
  (->> (in (string->char-set "-\\`|*_{}[]()#+.!><"))
       (preceded-by (is #\\) (none-of new-line))))

(define code
  (sequence* ((ticks (-> (is #\`)
                         (repeated min: 1 max: 5)
                         (as-string)))
              (_  space*)
              (code (->> (any-of (none-of* (is #\`) non-space-char)
                                 (none-of* (char-seq ticks) (one-or-more (is #\`)))
                                 (none-of* (preceded-by space* (char-seq ticks))
                                           (any-of space-char
                                                   (followed-by new-line (none-of blank-line)))))
                         (one-or-more)
                         (as-string)
                         (node 'code)))
              (_ (preceded-by space* (char-seq ticks))))
    (result code)))

(define alphanumeric-ascii
  (in #[a-zA-Z0-9]))

(define entity
  (->> (enclosed-by (is #\&)
                    (any-of (sequence (is #\#)
                                      (in (char-set #\x #\x))
                                      (one-or-more (in char-set:hex-digit)))
                            (sequence (is #\#)
                                      (one-or-more (in #[0-9])))
                            (one-or-more alphanumeric-ascii))
                    (is #\;))
       (as-string)
       (node '&)))

(define quoted
  (let ((delimiters (in #\" #\')))
    (sequence* ((delimiter delimiters)
                (text (zero-or-more (none-of* (is delimiter) item)))
                (_ (is delimiter)))
      (result text))))

(define html-attribute-value
  (preceded-by (is #\=)
               space-new-line
               (->> non-space-char
                    (none-of* (is #\>))
                    (one-or-more)
                    (any-of quoted)
                    (as-string))))

(define html-name
  (-> (any-of alphanumeric-ascii (is #\-))
      (one-or-more)
      (as-string)))

(define html-attribute
  (sequence* ((name html-name)
              (_ space-new-line)
              (value (maybe html-attribute-value))
              (_ space-new-line))
    (let ((name (string->symbol name)))
      (if value
          (result (list name value))
          (result name)))))

(define html-comment
  (sequence* ((_ (char-seq "<!--"))
              (text (->> (none-of* (char-seq "-->") item)
                         (zero-or-more)
                         (as-string)))
              (_ (char-seq "-->")))
    (result `(comment ,(tabs->spaces text)))))

(define (html-element el attrs #!optional (contents '()))
  (append (list 'html-element (string->symbol el))
          (if (null? attrs)
              '()
              (list (cons '@ attrs)))
          contents))

(define html-attributes
  (maybe (preceded-by space+ (zero-or-more html-attribute)) '()))

(define (html-root-element? str)
  (string= "html" str))

(define html-block-element?
  (let ((elements (make-hash-table)))
    (for-each (lambda (name)
                (hash-table-set! elements name #t)
                (hash-table-set! elements (string-upcase name) #t))
              '("article" "header" "aside" "hgroup" "blockquote" "hr" "iframe" "body"
                "li" "map" "button" "object" "canvas" "ol" "caption" "output" "col" "p"
                "colgroup" "pre" "dd" "progress" "div" "section" "dl" "table" "td" "dt"
                "tbody" "embed" "textarea" "fieldset" "tfoot" "figcaption" "th"
                "figure" "thead" "footer" "tr" "form" "ul" "h1" "h2" "h3" "h4" "h5"
                "h6" "video" "script" "style"))
    (lambda (name)
      (hash-table-exists? elements name))))

(define (html-element-name-parser pred)
  (bind html-name
        (lambda (name)
          (if (pred name)
              (result name)
              fail))))

(define html-root-element-name
  (html-element-name-parser html-root-element?))

(define html-block-element-name
  (html-element-name-parser html-block-element?))

(define html-inline-element-name
  (html-element-name-parser (complement (disjoin html-root-element? html-block-element?))))

(define (html-element-close-parser el)
  (preceded-by (is #\<) space-new-line (is #\/)
               (char-seq el) space-new-line (is #\>)))

(define (html-element-parser/contents result-element element contents-parser)
  (let ((close (html-element-close-parser element)))
    (any-of (sequence* ((contents (contents-parser close))
                        (_ space-new-line)
                        (_ close))
              (result-element contents))
            (result-element))))

(define (html-element-parser element-parser contents-parser)
  (sequence* ((_ (preceded-by (is #\<) space-new-line))
              (element element-parser)
              (attrs html-attributes)
              (_ space-new-line)
              (self-closing? (any-of (preceded-by (is #\/)
                                                  space-new-line
                                                  (is #\>)
                                                  (result #t))
                                     (preceded-by (is #\>)
                                                  (result #f)))))
    (let ((result-element (lambda args (result (apply html-element element attrs args)))))
      (if self-closing?
          (result-element)
          (html-element-parser/contents result-element element contents-parser)))))

(define (html-inline-element content-parser)
  (html-element-parser
   html-inline-element-name
   (lambda (close)
     (zero-or-more (none-of* close content-parser)))))

(define html-inline
  (recursive-parser
   (any-of html-comment (html-inline-element inline))))

(define image
  (bind (->> (any-of explicit-link reference-link)
             (preceded-by (is #\!)))
        (lambda (link)
          (result
           (if (eq? 'reference-link (car link))
               (cons 'reference-image (cdr link))
               (cons 'image (cdr link)))))))

(define inline-hook
  (make-parameter '()))

(define (inline-hook-parser input)
  ((any-of (inline-hook)) input))

(define inline
  (any-of inline-hook-parser
          (is #\>)
          (as-string (one-or-more normal-char))
          ul-or-star-line
          space+
          strong
          emph
          image
          link
          code
          html-inline
          entity
          (as-string escaped-char)
          (as-string special-char)))

(define inline-without-line-end
  (none-of* line-end inline))

(define inline-or-normal-line-end
  (any-of normal-line-end inline))

(define inlines
  (one-or-more
   (any-of inline-without-line-end
           (followed-by line-end
                        inline-without-line-end))))

(define reference-title
  (let ((delimiters '((#\( . #\)))))
    (sequence* ((delimiter (->> (in #\( #\' #\")
                                (preceded-by space-new-line)))
                (closing-delimiter (result (or (alist-ref delimiter delimiters) delimiter)))
                (title (-> (preceded-by
                            (is closing-delimiter)
                            space*
                            (any-of new-line end-of-input*))
                           (none-of* new-line item)
                           (zero-or-more)
                           (as-string)))
                (_ (is closing-delimiter)))
      (result title))))

(define reference
  (sequence* ((label (preceded-by non-indent-space
                                  (none-of (char-seq "[]"))
                                  link-label))
              (_ (preceded-by (is #\:) space-new-line))
              (href (as-string (one-or-more non-space-char)))
              (title (maybe reference-title))
              (_ (zero-or-more blank-line)))
    (result `(reference (label . ,label)
                        (href ,href)
                        (title ,title)))))

(define raw-line
  (any-of (sequence (char-seq-until (char-set #\return #\newline)) new-line)
          (sequence (one-or-more item) end-of-input*)))

(define (tabs->spaces str)
  (irregex-replace/all
   '(seq (submatch (* (~ #\tab #\newline))) #\tab)
   str
   (lambda (m)
     (let* ((prefix (irregex-match-substring m 1))
            (size   (- 4 (modulo (string-length prefix) 4))))
       (string-append prefix (make-string size #\space))))))

(define line
  (sequence* ((line (as-string raw-line)))
    (result (tabs->spaces line))))

(define indent
  (any-of (is #\tab) (char-seq "    ")))

(define indented-line
  (preceded-by indent line))

(define non-blank-indented-line
  (none-of* blank-line indented-line))

(define optionally-indented-line
  (preceded-by (maybe indent) line))

(define verbatim-chunk
  (sequence* ((blank-lines (zero-or-more (preceded-by blank-line (result "\n"))))
              (text (one-or-more non-blank-indented-line)))
    (result (append blank-lines text))))

(define verbatim
  (sequence* ((chunks (one-or-more verbatim-chunk)))
    (result `(verbatim . ,(concatenate chunks)))))

(define horizontal-rule-chars
  (char-set #\* #\- #\_))

(define horizontal-rule-char
  (in horizontal-rule-chars))

(define horizontal-rule
  (sequence* ((_ non-indent-space)
              (char horizontal-rule-char)
              (_ space*)
              (_ (repeated (preceded-by (is char) space*) min: 2))
              (_ (preceded-by space* new-line))
              (_ (one-or-more blank-line)))
    (result '(hr))))

(define enumerator
  (preceded-by non-indent-space
               (one-or-more (in char-set:digit))
               (is #\.) space+))

(define bullet-chars
  (char-set #\+ #\* #\-))

(define bullet-char
  (in bullet-chars))

(define bullet
  (enclosed-by (preceded-by non-indent-space (none-of horizontal-rule))
               bullet-char
               space+))

(define list-item-start
  (any-of bullet enumerator))

(define list-block-line
  (none-of* blank-line
            (preceded-by (maybe indent) list-item-start)
            horizontal-rule
            optionally-indented-line))

(define list-block
  (sequence (none-of* blank-line line)
            (zero-or-more list-block-line)))

(define list-continuation-block
  (sequence (zero-or-more blank-line)
            (one-or-more (preceded-by indent list-block))))

(define (list-item-node join first-block-parser first-block more-blocks)
  (->> (nth-value 0 (parse document more-blocks))
       (join (nth-value 0 (parse first-block-parser first-block)))
       (result)
       (node 'item)))

(define list-item-tight
  (recursive-parser
   (sequence* ((_ list-item-start)
               (first-block (as-string list-block))
               (more-blocks (->> list-continuation-block
                                 (none-of* blank-line)
                                 (zero-or-more)
                                 (as-string)))
               (_ (none-of list-continuation-block)))
    (list-item-node append inlines first-block more-blocks))))

(define list-tight
  (sequence* ((items (one-or-more list-item-tight))
              (_ (followed-by (zero-or-more blank-line)
                              (none-of list-item-start))))
    (result items)))

(define list-item-loose
  (recursive-parser
   (sequence* ((_ list-item-start)
               (first-block (as-string list-block))
               (more-blocks (-> (zero-or-more list-continuation-block)
                                (as-string))))
    (list-item-node cons block first-block more-blocks))))

(define list-loose
  (one-or-more
   (sequence* ((item list-item-loose)
               (_ (zero-or-more blank-line)))
     (result item))))

(define (list* start node-type)
  (->> (any-of list-tight list-loose)
       (node node-type)
       (all-of start)))

(define bullet-list
  (list* bullet 'bullet-list))

(define ordered-list
  (list* enumerator 'ordered-list))

(define atx-inline
  (none-of* new-line
            (sequence space* (zero-or-more (is #\#)) space* new-line)
            inline))

(define atx-start
  (sequence* ((level (repeated (is #\#) min: 1 max: 6)))
    (result (length level))))

(define atx-heading
  (sequence* ((level atx-start)
              (_ space*)
              (text (one-or-more atx-inline))
              (_ (maybe (preceded-by space* (zero-or-more (is #\#)) space*)))
              (_ new-line))
    (result `(heading ,level ,text))))

(define (setext-bottom char)
  (preceded-by (char-seq (make-string 3 char))
               (zero-or-more (is char))
               line-end))

(define (setext-heading* level bottom-char)
  (let ((bottom (setext-bottom bottom-char)))
    (sequence* ((text (one-or-more (none-of* line-end inline)))
                (_ (preceded-by space* new-line bottom)))
      (result `(heading ,level ,text)))))

(define setext-heading
  (any-of (setext-heading* 1 #\=)
          (setext-heading* 2 #\-)))

(define heading
  (any-of atx-heading setext-heading))

(define paragraph
  (enclosed-by non-indent-space
               (node 'paragraph inlines)
               (one-or-more blank-line)))

(define plain
  (node 'paragraph inlines))

(define blockquote
  (sequence*
      ((text (->> (sequence
                    (preceded-by (is #\>) (maybe (is #\space)) line)
                    (zero-or-more (none-of* (is #\>) blank-line line))
                    (zero-or-more blank-line))
                  (one-or-more)
                  (as-string))))
    (result (cons 'blockquote (nth-value 0 (parse document text))))))

(define html-text
  (sequence* ((str (char-seq-until (char-set #\<))))
    (if (string-null? str)
        fail
        (result str))))

(define html-inline-raw
  (recursive-parser
   (html-inline-element
    (any-of html-inline-raw
            html-text))))

(define html-root
  (html-element-parser
   html-root-element-name
   (lambda (close)
     (zero-or-more
      (any-of html-block
              html-inline-raw
              (none-of* close html-text))))))

(define html-block*
  (html-element-parser
   html-block-element-name
   (lambda (close)
     (zero-or-more
      (any-of html-block
              html-inline-raw
              (none-of* close html-text))))))

(define html-block
  (any-of html-block*
          html-comment))

(define block-hook
  (make-parameter '()))

(define (block-hook-parser input)
  ((any-of (block-hook)) input))

(define block
  (preceded-by
   (zero-or-more blank-line)
   (any-of block-hook-parser
           blockquote
           verbatim
           reference
           horizontal-rule
           heading
           ordered-list
           bullet-list
           html-block
           paragraph
           plain)))

(define document
  (any-of
   html-root
   (zero-or-more block)))
