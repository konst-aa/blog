(import spiffy
        intarweb
        uri-common
        srfi-1
        srfi-18
        (chicken port)
        (chicken io)
        (chicken process)
        (chicken process-context)
        (chicken string))


(define mailing-list-name (get-environment-variable "MAILING_LIST_NAME"))
(define email-mutex (make-mutex 'emails))

(define (read-emails path)
  (define emails '())
  (define (loop)
    (let ((email (read)))
      (if (not (eof-object? email))
        (begin (set! emails (cons email emails))
               (loop)))))
  (with-input-from-file path loop)
  emails)

(define (write-emails emails path)
  (with-output-to-file
    path
    (lambda ()
      (for-each (lambda (email)
                  (display email)
                  (newline))
                emails))))

(define (add-email email)
  (mutex-lock! email-mutex)
  (define emails (read-emails "emails.txt"))
  (define added #t)
  (if (not (member email emails))
    (write-emails (cons email emails) "emails.txt")
    (set! added #f))
  (mutex-unlock! email-mutex)
  added)

(define (remove-email email)
  (mutex-lock! email-mutex)
  (write-emails (filter (lambda (e) (not (equal? e email)))
                        (read-emails "emails.txt"))
                "emails.txt")
  (mutex-unlock! email-mutex))

(define (send-email to subject body)
  (let-values (((email-out email-in email-pid) (process (string-append "msmtp -a gmail " to))))
    (write-string (string-append "Subject: " subject "\n\n") #f email-in)
    (write-string body #f email-in)
    (close-output-port email-in)
    (read-string #f email-out)))

(parameterize
  ((server-port 8080)
   (handle-not-found
     (lambda (b)
       (let* ((uri (request-uri (current-request)))
              (path (uri-path uri))
              (query (uri-query uri))
              (email (alist-ref 'email query)))
         (if (and (<= (string-length email) 30) (= (length (string-split email " \"|&")) 1))
           (case (string->symbol (cadr path))
             ((subscribe)
              (if (add-email (string->symbol email))
                (send-email email
                            "Welcome!"
                            (string-append "You have subscribed to the " mailing-list-name " mailing list"))))
             ((unsubscribe)
              (remove-email (string->symbol email)))
             (else
               (display "Invalid path!")))))
       (send-status 200 "OK")
       )
     ))
  (start-server))
