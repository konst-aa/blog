# SPOILER
A somewhat short program (~150 loc) to automatically differentiate s-exprs, along with a breakdown of how it works. It also comes with a REPL
# ENDSPOILER

## Automatic Differentiator 9000

It almost feels like the easiest part of this program is the actual differentiation. The rest is coming up with and writing the REPL, which took several iterations and the most time. 

Scheme's `read` procedure (which is a super buffed version of your average `input()` or `cin` function) does all the work for taking in user input.  

## Example
### differentiates (f x y) with respect to x
```
>> :f (f x y) = (* x y)
>> :d f x
(df/dx x y) = (+ (* 1 y) (* x 0))
>> :e (df/dx 5 10)
10
```

## Math
The actual math part is recursively applying the following rules:
- When differentiating an arithmetic operation, we employ the product, quotient, and distributivity over addition rules.
- When differentiating a function, plug and chug its derivative using `(deriv f)`, then use the chain rule.
- If we're differentiating with respect to x, then `d/dx[any-other-var]` is 0; that's the base case.  

Afterward, the evaluation command uses Scheme's `eval` function to evaluate the derivative at some point. We get to do this because the formula for the derivative is still lists and symbols (it went from `(* x y)` to  `(+ (* 1 y) (* x 0))`, and is therefore valid Scheme code as long as x and y are defined. That is one part of what makes Scheme so great; it's straightforward to jump from abstract stuff (such as equations) to code.

## Next Steps
With these abstractions in place, calculating Hessian matrices, discriminants, gradients, and company shouldn't be too difficult. I believe all of this is also discussed in [The Little Learner](https://mitpress.mit.edu/9780262546379/the-little-learner/); I own a copy but have yet to read it...

## Code snippet
The whole file can be found here: [good times](https://github.com/konst-aa/fun/blob/main/autodiff.scm). I made a [replit for it](Blog post: https://ka.dreadmaw.industries/autodiff-9000.html), or you can run it in [the Gambit Scheme Repl](https://try.gambitscheme.org/), but you'll need the following patch:  

```scheme
;; Patch for running on try.gambitscheme.org
;; Replace the readrepl function with this one.
(define (readrepl)
  (display ">> ")
  (flush-output-port) ; flush port to make sure the prompt is displayed
  (read))
```
**Actual snippet:**  

```scheme
;; Accessors
(define (x1 f)
  (cadr f))
(define (x2 f)
  (caddr f))
(define (rest f)
  (cddr f))

(define (deriv f)
  ;; treat (x1 f) as x
  (case (car f)
    ((expt)
     `(* ,(x2 f) (expt ,(x1 f) ,(- (x2 f) 1))))
    ((exp)
     `(exp ,(x1 f)))
    ((sqrt)
     (deriv `(expt ,(x1 f) 0.5)))
    ((sin)
     `(cos ,(x1 f)))
    ((cos)
     `(- (sin ,(x1 f))))))

(define (diff f wrt)
  (define (chain f g)
    `(* ,(deriv f)
      ,(diff g wrt)))
  (cond
    ((eq? wrt f) 1)
    ((and (pair? f)
          (null? (cddr f))
          (member (car f) '(* + /)))
     (case (car f)
       ((+ *)  ; identity ops: (* x), (+ x), etc.
        (diff (x1 f) wrt))
       ((/)
        (diff `(/ 1 ,(x1 f)) wrt))))
    ((pair? f)
     (case (car f)
       ((+) `(+ ,@(map (lambda (f) (diff f wrt)) (cdr f))))
       ((-) `(- ,(diff `(+ ,@(cdr f)) wrt)))
       ((*) `(+ (* ,(diff (x1 f) wrt) ,@(rest f))
              (* ,(x1 f) ,(diff `(* ,@(rest f)) wrt))))
       ((/) `(/ (- (* ,(diff (x1 f) wrt) ,(x2 f))
                   (* ,(x1 f) ,(diff (x2 f) wrt)))
              (expt ,(x2 f) 2)))
       (else
         ;; does the heavy lifting
         (chain f (x1 f)))
       ))
    (else 0)))
```
