(import scheme)

(cond-expand
  (chicken-4
    (import (except chicken define-type))
    (use data-structures
         lazy-seq
         srfi-1
         srfi-13
         srfi-14
         srfi-69
         extras
         trie
         ports
         irregex))
  (chicken-5
    (import (chicken base)
            (chicken fixnum)
            (chicken io)
            (chicken irregex)
            (chicken port)
            (chicken string)
            srfi-1
            srfi-13
            srfi-14
            srfi-69
            trie
            lazy-seq)))

(include "comparse-generics.scm")
(include "comparse-char-seq-cursor.scm")

(define-generic (%char-seq-until (char-seq-cursor? input) cs allow-end-of-input?)
  (let input-loop ((chunks '())
                   (input input))
    (let ((buf (csc-buffer input)))
      (if (not buf)
          (and allow-end-of-input?
               (cons (reverse-string-append chunks) input))
          (let ((start (csc-pos input))
                (len (string-length buf)))
            (let loop ((pos start))
              (cond ((= pos len)
                     (input-loop (cons (substring buf start len) chunks)
                                 (char-seq-cursor-next input)))
                    ((and cs (char-set-contains? cs (string-ref buf pos)))
                     (let ((chunks (cons (substring buf start pos) chunks)))
                       (cons (reverse-string-append chunks)
                             (make-char-seq-cursor buf pos (csc-next input)))))
                    (else
                     (loop (fx+ pos 1))))))))))

(define-generic (%char-seq-until (lazy-seq? input) cs allow-end-of-input?)
  (let loop ((input input)
             (result '()))
    (if (lazy-null? input)
        (and allow-end-of-input?
             (cons (reverse-list->string result) input))
        (let ((char (lazy-head input)))
          (if (and cs (char-set-contains? cs char))
              (cons (reverse-list->string result) input)
              (loop (lazy-tail input)
                    (cons char result)))))))

(define ((char-seq-until cs #!optional (allow-end-of-input? (not cs))) input)
  (%char-seq-until input cs allow-end-of-input?))

(define-generic (%char-seq-split (char-seq-cursor? input) allow-end-of-input? str len rv)
  (and (csc-buffer input)
       (let loop ((i 0)
                  (pos (csc-pos input))
                  (input input)
                  (chunks '()))
         (let ((buf (csc-buffer input)))
           (if buf
               (let ((i (string-kmp-partial-search str rv buf i char=? 0 pos)))
                 (if (fx< i 0)
                     (let* ((i (- i))
                            (chunk (substring buf pos (- i len))))
                       (cons (reverse-string-append (cons chunk chunks))
                             (make-char-seq-cursor buf i (csc-next input))))
                     (loop i 0 (char-seq-cursor-next input) (cons (substring buf pos) chunks))))
               (and allow-end-of-input?
                    (cons (reverse-string-append chunks) input)))))))

(define-generic (%char-seq-split (lazy-seq? input) allow-end-of-input? str len rv)
  (and (not (lazy-null? input))
       (let loop ((i 0)
                  (input input)
                  (chars '()))
         (if (or (fx= len i) (lazy-null? input))
             (and-let* ((chars (if (fx= len i)
                                   (drop chars len)
                                   (and allow-end-of-input? chars))))
               (cons (reverse-list->string chars) input))
             (let ((head (lazy-head input)))
               (if (char=? (string-ref str i) head)
                   (loop (fx+ 1 i)
                         (lazy-tail input)
                         (cons head chars))
                   (let ((i (vector-ref rv i)))
                     (if (fx= i -1)
                         (loop 0
                               (lazy-tail input)
                               (cons head chars))
                         (loop i
                               input
                               chars)))))))))

(define (char-seq-split str #!optional allow-end-of-input?)
  (let ((rv (make-kmp-restart-vector str))
        (len (string-length str)))
    (lambda (input)
      (%char-seq-split input allow-end-of-input? str len rv))))

(define char-seq-cursor-irregex-chunker
  (make-irregex-chunker char-seq-cursor-next*
                        csc-buffer
                        csc-pos))

(define-generic (%char-seq-match (char-seq-cursor? input) rx)
  (and-let* ((m (irregex-search/chunked rx char-seq-cursor-irregex-chunker input)))
    (let ((remainder (irregex-match-end-chunk m)))
      (cons (irregex-match-substring m)
            (make-char-seq-cursor (csc-buffer remainder)
                                  (irregex-match-end-index m 0)
                                  (csc-next remainder))))))

(define (lazy-next seq)
  (let ((tail (lazy-tail seq)))
    (and (not (lazy-null? tail)) tail)))

(define lazy-seq-irregex-chunker
  (make-irregex-chunker lazy-next (o string lazy-head)))

(define-generic (%char-seq-match (lazy-seq? input) rx)
  (and-let* ((m (irregex-search/chunked rx lazy-seq-irregex-chunker input)))
    (cons (irregex-match-substring m)
          (lazy-tail (irregex-match-end-chunk m)))))

(define (char-seq-match rx)
  (let ((rx (irregex rx)))
    (lambda (input)
      (%char-seq-match input rx))))


(define ((result value) input)
  (cons value input))

(define fail
  (constantly #f))

(define-generic (item (lazy-seq? input))
  (and (not (lazy-null? input))
       (cons (lazy-head input)
             (lazy-tail input))))

(define-generic (item (char-seq-cursor? input))
  (and-let* ((buf (csc-buffer input))
             (pos (csc-pos input))
             (next (csc-next input)))
    (if (= (string-length buf) pos)
        (item (force next))
        (cons (string-ref buf pos)
              (make-char-seq-cursor buf (fx+ pos 1) next)))))

(define ((bind parser proc) input)
  (and-let* ((value (parser input)))
    ((proc (car value)) (cdr value))))

(define (satisfies condition . args)
  (bind item (lambda (x)
               (if (apply condition x args)
                   (result x)
                   fail))))

(define (args-list parser more-parsers)
  (if (and (list? parser) (null? more-parsers))
      parser
      (cons parser more-parsers)))

(define (in collection . items)
  (if (and (null? items) (char-set? collection))
      (bind item (lambda (c)
                   (if (and (char? c) (char-set-contains? collection c))
                       (result c)
                       fail)))
      (satisfies memq (args-list collection items))))

(define (is x)
  (satisfies eqv? x))

(define-syntax sequence*
  (syntax-rules ()
    ((_ () body ...)
     (begin body ...))
    ((_ ((binding parser) more-bindings ...) body ...)
     (bind parser
           (lambda (binding)
             (sequence* (more-bindings ...) body ...))))))

(define (sequence parser . parsers)
  (let ((parsers (args-list parser parsers)))
    (let loop ((parsers parsers)
               (parts '()))
      (if (null? parsers)
          (result (reverse! parts))
          (bind (car parsers)
                (lambda (value)
                  (loop (cdr parsers)
                        (cons value parts))))))))

(define (skip parser . parsers)
  (let ((parsers (args-list parser parsers)))
    (let loop ((parsers parsers))
      (if (null? parsers)
          (result #t)
          (bind (car parsers)
                (lambda (value)
                  (loop (cdr parsers))))))))

(define (char-seq str)
  (let ((chars (string->list str)))
    (let loop ((chars chars))
      (if (null? chars)
          (result str)
          (bind item (lambda (value)
                       (if (eq? value (car chars))
                           (loop (cdr chars))
                           fail)))))))

(define (any-of parser . parsers)
  (let ((parsers (args-list parser parsers)))
    (lambda (input)
      (let loop ((parsers parsers))
        (and (not (null? parsers))
             (or ((car parsers) input)
                 (loop (cdr parsers))))))))

(define (all-of parser . parsers)
  (let ((parsers (args-list parser parsers)))
    (lambda (input)
      (let loop ((parsers parsers))
        (and-let* ((value ((car parsers) input)))
          (if (null? (cdr parsers))
              value
              (and value (loop (cdr parsers)))))))))

(define (none-of parser . parsers)
  (let ((parsers (args-list parser parsers)))
    (lambda (input)
      (let loop ((parsers parsers))
        (if (null? parsers)
            (cons #t input)
            (and (not ((car parsers) input))
                 (loop (cdr parsers))))))))

(define (preceded-by parser . parsers)
  (let loop ((parsers (args-list parser parsers)))
    (bind (car parsers)
          (lambda (value)
            (if (null? (cdr parsers))
                (result value)
                (loop (cdr parsers)))))))

(define (none-of* parser but . parsers)
  (receive (but parsers) (car+cdr (reverse (cons* parser but parsers)))
    (preceded-by (none-of parsers) but)))

(define ((followed-by parser following . more-following) input)
  (and-let* ((value (parser input)))
    (let loop ((following (args-list following more-following))
               (input (cdr value)))
      (if (null? following)
          value
          (and-let* ((value ((car following) input)))
            (loop (cdr following) (cdr value)))))))

(define (enclosed-by open content close)
  (sequence* ((_ open) (value content) (_ close))
    (result value)))

(define (->parser object)
  (cond ((procedure? object) object)
        ((char-set? object) (satisfies object))
        ((char? object) (is object))
        ((string? object) (char-seq object))
        (else (error "Don't know how to turn object into parser" object))))

(define ((zero-or-more parser) input)
  (let loop ((result '())
             (input input))
    (let ((value (parser input)))
      (if value
          (loop (cons (car value) result)
                (cdr value))
          (cons (reverse! result)
                input)))))

(define (one-or-more parser)
  (sequence* ((x parser)
              (y (zero-or-more parser)))
    (result (cons x y))))

(define (repeated-until parser end)
  (any-of (all-of end (result '()))
          (sequence* ((x parser)
                      (y (repeated-until parser end)))
            (result (cons x y)))))

(define (repeated/end min)
  (if (<= min 0)
      (result '())
      fail))

(define (repeated/min/max parser min max)
  (if (zero? max)
      (repeated/end min)
      (any-of (sequence* ((x parser)
                          (y (repeated/min/max parser (- min 1) (- max 1))))
                (result (cons x y)))
              (repeated/end min))))

(define (repeated/min parser min)
  (any-of (sequence* ((x parser)
                      (y (repeated/min parser (- min 1))))
            (result (cons x y)))
          (repeated/end min)))

(define (repeated parser #!rest args #!key (min 0) max until)
  (cond (until
         (cond (max
                (followed-by (repeated/min/max parser min max) until))
               ((zero? min)
                (repeated-until parser until))
               (else
                (sequence* ((x (repeated/min/max parser min min))
                            (y (repeated-until parser until)))
                  (result (append x y))))))
        (max
         (repeated/min/max parser min max))
        ((and (pair? args) (null? (cdr args)))
         (repeated/min/max parser (car args) (car args)))
        (else
         (repeated/min parser min))))

(define (maybe parser #!optional default)
  (any-of parser (result default)))

(define end-of-input
  (none-of item))

(define ((as-string parser) input)
  (and-let* ((result+remainder (parser input)))
    (cons (call-with-output-string
            (lambda (out)
              (let print-loop ((parts (car result+remainder)))
                (cond ((pair? parts)
                       (let pair-loop ((parts parts))
                         (cond ((pair? parts)
                                (print-loop (car parts))
                                (pair-loop (cdr parts)))
                               ((not (null? parts)) ; improper list
                                (print-loop parts)))))
                      ((and parts (not (null? parts)))
                       (write-string (->string parts) #f out))))))
          (cdr result+remainder))))

(define memo-table
  (make-parameter #f))

(define-generic (raw-input-prefix (lazy-seq? from) to)
  (let loop ((from from)
             (prefix '()))
    (if (eq? from to)
        (reverse! prefix)
        (loop (lazy-tail from)
              (cons (lazy-head from) prefix)))))

(define-generic (raw-input-prefix (char-seq-cursor? from) to)
  (let ((to-buffer (csc-buffer to)))
    (let loop ((from from)
               (prefix '()))
      (if (eq? (csc-buffer from) to-buffer)
          (let* ((offset (if (null? prefix) (csc-pos from) 0))
                 (rest (substring/shared to-buffer offset (csc-pos to))))
            (append! prefix (string->list rest)))
          (loop (char-seq-cursor-next from)
                (append! prefix (string->list (char-seq-cursor-chunk from))))))))

(define (memo-ref parser input)
  (and-let* ((memo (hash-table-ref/default (memo-table) parser #f)))
    (let loop ((memo memo)
               (input input))
      (and-let* ((result+remainder (item input))
                 (memo (trie-ref* memo (car result+remainder)))
                 (value (trie-value memo)))
        (if (null? value)
            (loop memo (cdr result+remainder))
            (cons (car value) (cdr result+remainder)))))))

(define (memo-set! parser input)
  (and-let* ((result+remainder (parser input)))
    (hash-table-update! (memo-table)
                        parser
                        (lambda (memo)
                          (trie-insert! memo
                                        (raw-input-prefix input (cdr result+remainder))
                                        (car result+remainder))
                          memo)
                        make-trie)
    result+remainder))

(define ((memoize parser) input)
  (if (memo-table)
      (or (memo-ref parser input)
          (memo-set! parser input))
      (parser input)))

(define-syntax recursive-parser
  (syntax-rules ()
    ((_ body ...)
     (lambda (input)
       (let ((parser (begin body ...)))
         (parser input))))))

(define-record-type parser-input
  (make-parser-input raw)
  parser-input?
  (raw parser-input-raw))

(define-generic (raw-input-end? (lazy-seq? input))
  (lazy-null? input))

(define-generic (raw-input-end? (char-seq-cursor? input))
  (char-seq-cursor-end? input))

(define (parser-input-end? input)
  (raw-input-end? (parser-input-raw input)))

(define-generic (raw-input->list (lazy-seq? input))
  (lazy-seq->list input))

(define-generic (raw-input->list (char-seq-cursor? input))
  (let loop ((input input)
             (result '()))
    (if (char-seq-cursor-end? input)
        (append-map! string->list (reverse! result))
        (loop (char-seq-cursor-next input)
              (cons (char-seq-cursor-chunk input) result)))))

(define (parser-input->list input)
  (raw-input->list (parser-input-raw input)))

(define-generic (raw-input->string (lazy-seq? input))
  (list->string (lazy-seq->list input)))

(define-generic (raw-input->string (char-seq-cursor? input))
  (call-with-output-string
    (lambda (out)
      (let loop ((input input))
        (and-let* ((chunk (char-seq-cursor-chunk input)))
          (display chunk out))
        (unless (char-seq-cursor-end? input)
          (loop (char-seq-cursor-next input)))))))

(define (parser-input->string input)
  (raw-input->string (parser-input-raw input)))

(define-generic (raw-input->lazy-seq (lazy-seq? input))
  input)

(define-generic (raw-input->lazy-seq (char-seq-cursor? input))
  (let loop ((input input))
    (let ((chunk (char-seq-cursor-chunk input)))
      (lazy-append (string->lazy-seq chunk)
                   (lazy-seq
                     (if (char-seq-cursor-end? input)
                         (loop (char-seq-cursor-next input))
                         lazy-null))))))

(define (parser-input->lazy-seq input)
  (raw-input->lazy-seq (parser-input-raw input)))

(define (display-parser-input input out)
  (display "#<parser-input" out)
  (if (parser-input-end? input)
      (display "-end>" out)
      (let ((raw-input (parser-input-raw input)))
        (cond ((lazy-seq? raw-input)
               (display " lazy-seq" out)
               (let loop ((n 10)
                          (seq raw-input))
                 (if (and (lazy-seq-realized? seq) (not (zero? n)))
                     (if (lazy-null? seq)
                         (display ">" out)
                         (begin
                           (display " " out)
                           (write (lazy-head seq) out)
                           (loop (- n 1) (lazy-tail seq))))
                     (display " ...>" out))))
              ((char-seq-cursor? raw-input)
               (display " char-seq-cursor " out)
               (write (char-seq-cursor-chunk raw-input) out)
               (display " ...>" out))))))

(define-record-printer (parser-input input out)
  (display-parser-input input out))

(define (->raw-input location x)
  (cond ((lazy-seq? x) x)
        ((parser-input? x) (parser-input-raw x))
        ((string? x) (char-seq-cursor x))
        ((list? x) (list->lazy-seq x))
        ((input-port? x) (char-seq-cursor x))
        (else (error location "Unable to convert object to parser input" x))))

;; deprecated
(define default-parser-input-chunk-size
  (make-parameter #f))

;; `chunk-size` is deprecated
(define (->parser-input x #!key (chunk-size #f))
  (make-parser-input (->raw-input '->parser-input x)))

(define (parse parser input #!key memoize)
  (parameterize ((memo-table (if memoize (make-hash-table) (memo-table))))
    (let* ((raw-input (->raw-input 'parse input))
           (result (parser raw-input)))
      (if result
          (values (car result) (make-parser-input (cdr result)))
          (values result (make-parser-input raw-input))))))
