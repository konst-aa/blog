(cond-expand
  (chicken-4 (use comparse test lazy-seq))
  (chicken-5 (import (chicken port) srfi-14 comparse test lazy-seq)))

(test-begin)

(define (write-to-string x)
  (call-with-output-string (lambda (out) (write x out))))

(define-syntax test-parse
  (syntax-rules ()
    ((_ expect parser input)
     (let ((expect* expect)
           (input* input))
       (test-parse expect*
                   (and (string? input*)
                        (if expect*
                            ""
                            input*))
                   parser
                   input*)))
    ((_ expect expect-remainder parser input)
     (let* ((expect* expect)
            (expect-remainder* expect-remainder)
            (expect-remainder-desc (write-to-string (list 'remainder expect-remainder*))))
       (test-group (write-to-string expect*)
         (receive (result remainder)
           (parse parser input)
           (test (write-to-string (if (or (string? input) (input-port? input))
                                      (list 'char-seq expect*)
                                      expect*))
                 expect*
                 result)
           (when expect-remainder*
             (test expect-remainder-desc
                   expect-remainder*
                   (parser-input->string remainder))))
         (when (string? input)
           (receive (result remainder)
             (parse parser (string->lazy-seq input))
             (test (write-to-string (list 'lazy-seq expect*))
                   expect*
                   result)
             (when expect-remainder*
               (test expect-remainder-desc
                     expect-remainder*
                     (parser-input->string remainder))))))))))

(define-syntax test-parse*
  (syntax-rules ()
    ((_ expect parser input)
     (test-parse* expect #f parser input))
    ((_ expect expect-remainder parser input)
     (test-parse expect expect-remainder (as-string parser) input))))

(test-group "satisfies"
  (test-parse 3 (satisfies odd?) '(3))
  (test-parse #f (satisfies odd?) '(2))
  (test-parse #\b "cd" (satisfies memq '(#\a #\b #\c)) "bcd"))

(test-group "is"
  (test-parse #\x "yz" (is #\x) "xyz")
  (test-parse #f (is #\x) "ho"))

(test-group "in"
  (test-parse #\c (in (char-set #\a #\b #\c)) "c")
  (test-parse 2 (in '(1 2 3)) '(2))
  (test-parse #\b "cd" (in #\a #\b #\c) "bcd")
  (test-parse #f (in '()) "hey"))

(test-group "one-or-more / zero-or-more"
  (test-parse* "ooooo" (one-or-more (is #\o)) "ooooom")
  (test-parse* #f (one-or-more (is #\o)) "m")
  (test-parse* "oo" (zero-or-more (is #\o)) "oommm")
  (test-parse* "" (zero-or-more (is #\o)) "m"))

(test-group "char-seq"
  (test-parse "" "hey" (char-seq "") "hey")
  (test-parse "hey" "yy"(char-seq "hey") "heyyy")
  (test-parse #f (char-seq "hey") "he"))

(test-group "followed-by"
  (let ((lol (preceded-by (followed-by item (is #\o) (is #\l)) item)))
    (test #\o (parse lol "lol"))
    (test #f  (parse lol "lxl"))))

(test-group "preceded-by"
  (test-parse 3 (preceded-by (is 1) (is 2) (is 3)) '(1 2 3)))

(test-group "sequence"
  (test-parse* "ab" (sequence (is #\a) (is #\b)) "abc")
  (test-parse* "ab" (sequence (list (is #\a) (is #\b))) "abc"))

(test-group "maybe"
  (let ((foo (preceded-by (maybe (is #\x)) (char-seq "foo"))))
    (test-parse* "foo" foo "foo")
    (test-parse* "foo" foo "xfoo")
    (test-parse* #f foo "bar")
    (test-parse* "ok" (maybe (is #\x) "ok") "y")))

(test-group "repeated"
  (test-parse* "hohoho" (repeated (char-seq "ho")) "hohoho")
  (test-parse* "ho    ho ho "
               (repeated (sequence (char-seq "ho")
                                   (zero-or-more (is #\space)))
                         min: 2)
               "ho    ho ho rofl")
  (test-parse* "foo" (repeated item until: (is #\.)) "foo.")

  (let ((ok (repeated item min: 3 until: (is #\k))))
    (test-parse* "oko" ok "okok")
    (test-parse* #f ok "ooko"))

  (let ((ok (repeated (is #\o) max: 2 until: (is #\k))))
    (test-parse* "o" ok "okay")
    (test-parse* "oo" ok "ookay")
    (test-parse* #f ok "ooookay"))

  (let ((hohoho (repeated (char-seq "ho") 3)))
    (test-parse* "hohoho" hohoho "hohoho")
    (test-parse* #f hohoho "hoho")))

(test-group "all-of"
  (test-parse #\b (all-of (none-of (is #\a)) (is #\b)) "b")
  (test-parse #f (all-of (none-of (is #\a)) (is #\b)) "a"))

(test-group "enclosed-by"
  (let ((parenthesized (enclosed-by (is #\() (is #\x) (is #\)))))
    (test-parse #\x parenthesized "(x)")
    (test-parse #f parenthesized "(x/")
    (test-parse #f parenthesized "()")))

(test-group "none-of"
  (let ((not-xy (preceded-by (none-of (is #\x) (is #\y)) item)))
    (test-parse #\a not-xy "a")
    (test-parse #f not-xy "x")
    (test-parse #f not-xy "y")))

(test-group "none-of*"
  (let ((not-xy (none-of* (is #\x) (is #\y) item)))
    (test-parse #\a not-xy "a")
    (test-parse #f not-xy "x")
    (test-parse #f not-xy "y")))

(test-group "misc"
  (test-parse* "aaba" (repeated (in #\a #\b)) "aabac")
  (test-parse* "   " (repeated (is #\space) max: 3) "      ")
  (test-parse* "" (repeated (is #\f)) "x")
  (test-parse* #f (repeated (is #\a) min: 1) "b")
  (test-parse #\a (preceded-by (none-of (is #\b) (is #\c)) item) "a")
  (test-parse* "b52" (zero-or-more (any-of (in char-set:digit) (is #\b))) "b52s")

  (test-parse #f (none-of (is #\b) (is #\a)) "a")

  (test-parse* "ab"
               (sequence* 
                   ((a (is #\a)) (b (is #\b)))
                 (result (list a b)))
               "abc"))

(test-group "char-seq-until"
  (test-parse "foo" ",bar" (char-seq-until (char-set #\: #\,)) "foo,bar")
  (test-parse #f "foo" (char-seq-until (char-set #\x)) "foo")
  (test-parse "foo" "" (char-seq-until (char-set #\x) #t) "foo")
  (test-parse '("foo" "bar") "baz" (zero-or-more (sequence* ((x (char-seq-until (char-set #\, #\:))) (_ item)) (result x))) "foo:bar,baz")
  (test-parse "foo" "" (char-seq-until #f) "foo"))

(test-group "char-seq-match"
  (test-parse "bar" " baz" (char-seq-match "bar") "foo bar baz")
  (test-parse "123" " bar" (char-seq-match '(+ num)) "foo 123 bar")
  (test-parse #f (char-seq-match '(: bos (+ num))) "foo 123 bar")
  (test-parse #f (char-seq-match '(+ num)) "foo"))

(test-group "char-seq-split"
  (test-parse "foo" "bar,baz" (char-seq-split ",") "foo,bar,baz")
  (test-parse #f (char-seq-split ".") "foo")
  (test-parse "foo" (char-seq-split "." #t) "foo")
  (test-parse '("foo" "bar" "baz") (zero-or-more (char-seq-split ", " #t)) "foo, bar, baz"))

(test-group "quoted string"
  (define (quoted-string #!key
                         (delimiter (in #\" #\'))
                         (escape (is #\\)))
    (let ((escaped-char (preceded-by escape item)))
      (sequence* ((_ (zero-or-more (in char-set:whitespace)))
                  (actual-delimiter delimiter)
                  (chars (zero-or-more
                          (any-of escaped-char
                                  (preceded-by (none-of escape (is actual-delimiter))
                                               item))))
                  (_ (is actual-delimiter)))
        (result (list->string chars)))))

  (test-assert (not (parse (quoted-string) "this ain't a string")))
  (test "nice" (parse (quoted-string) "\"nice\""))


  (receive (result rest) (parse (quoted-string) "\"string 1\" 'string 2'  some trailing ")
    (test "string 1" result)
    (receive (result rest) (parse (quoted-string) rest)
      (test "string 2" result)
      (receive (result rest) (parse (quoted-string) rest)
        (test-assert (not result))
        (test "  some trailing " (parser-input->string rest)))))

  (define singly-quoted-bang-string (quoted-string delimiter: (is #\') escape: (is #\!)))
  (test "this 'is' a string" (parse singly-quoted-bang-string "'this !'is!' a string'"))
  (test "ok\\" (parse singly-quoted-bang-string "'ok\\'"))
  (test-assert (not (parse singly-quoted-bang-string "\"check\""))))

(define (make-unbuffered-port s)
  (let ((in (open-input-string s)))
    (make-input-port (lambda () (read-char in))
                     (lambda () (char-ready? in))
                     (lambda () (close-input-port in)))))

(test-group "unbuffered ports"
  (let ((up (make-unbuffered-port "foo bar")))
    (test "foo bar" (parse (char-seq "foo bar") up))))

(test-group "parser-input"
  (receive (x rest)
    (parse item "foo")
    (test #\f x)
    (test-assert (parser-input? rest))
    (test-assert (not (parser-input-end? rest)))
    (test "oo" (parser-input->string rest))
    (test (list #\o #\o) (parser-input->list rest))
    (receive (x* rest*)
      (parse (as-string (sequence item item)) rest)
      (test "oo" x*)
      (test-assert (parser-input-end? rest*)))))

(test-end)

(test-exit)
