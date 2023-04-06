(cond-expand
  (chicken-4
   (use posix data-structures))
  (chicken-5
   (import (chicken pretty-print)
           (chicken process)
           (chicken process-context)
           (chicken string))))

(with-output-to-file "lowdown-run"
 (lambda ()
   (print "#!/bin/sh")
   (print "#|")
   (print "exec " (cond-expand
                    (chicken-5 (executable-pathname))
                    (chicken-4 "csi"))
          " -s \"$0\" \"$@\"")
   (print "|#")
   (pretty-print
    `(begin
       ,(cond-expand
          (chicken-4
           `(use lowdown))
          (chicken-5
           `(import (chicken process-context)
                    lowdown)))
       (call-with-input-file (car (command-line-arguments)) markdown->html)))))

(system "chmod +x lowdown-run")
(process-execute "/usr/bin/env"
                 '("perl" "./MarkdownTest_1.0.3/MarkdownTest.pl"
                   "--tidy"
                   "--script" "./lowdown-run"
                   "--testdir" "MarkdownTest_1.0.3/Tests"))
