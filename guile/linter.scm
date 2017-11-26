#!/usr/bin/guile \
-e main -s
!#

;; INFO: run multiple code checkers in the current folder
;; INFO: supported programming languages (Language: linters):
;; INFO:  - Python: pep8, pylint

(load "helpers.scm")
(use-modules (helpers))

(use-modules (ice-9 ftw))
(use-modules (srfi srfi-1))
(use-modules (ice-9 match))
(use-modules (ice-9 popen))
(use-modules (ice-9 regex))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 curried-definitions))

(define python-file-regex (make-regexp ".*.py$"))
(define gitignore-file ".gitignore")

(define pep8-checker "pep8 --max-line-length=80 ~a")
(define pylint-checker "pylint3 --max-line-length=80 ~a 2> /dev/null")

(define min-pylint-score 8)
(define pylint-score-regex "1{0,1}[0-9].[0-9]")
(define pylint-score-line-regex "Your code has been rated at 1{0,1}[0-9].[0-9]")

(define pylint-message
  "
----------------------------------------
            PyLint Score: ~a
----------------------------------------
")
(define pep8-message
  "
----------------------------------------
           Pep8 Status: ~a
----------------------------------------
")
(define usage
  "
Usage: ~a <command>
Commands:
   all       check all supported source files but the ones in .gitignore
   stashed   check all stashed supported source files, useful as pre-commit hook
")

(define (main args)
  (cond ((string=? (last args) "all")
         (let* ((exclusions (gitignore-files gitignore-file))
                (files (list-included-files "." exclusions)))
           (exit (check-files files))))
        ((string=? (last args) "stashed")
         (let ((files (list-staged-files)))
           (exit (check-files files))))
        (else (format #t usage (car args))
              (exit 1))))

(define (check-files files)
  ;; For each supported file-type run the checkers
  ;; Return 0 if all checks are ok
  (let ((python (check-python-files files)))
    (+ python)))

(define (check-python-files files)
  ;; Run Pep8 and PyLint for all listed Python files
  ;; Return 0 if checks are ok, 1 otherwise
  (let ((python-files (filter (lambda (path)
                                (regexp-exec python-file-regex path))
                              files)))
    (when (not (null? python-files))
          (let ((pep8-status (run-pep8-check python-files))
                (pylint-status (run-pylint-check python-files)))
            (if (and pep8-status pylint-status) 0 1)))))

(define (run-pep8-check files)
  ;; Check the list of Python files with Pep8
  (let* ((results (run-command (format #f pep8-checker (string-join files))))
         (check-status (cond ((eq? (cdr results) 0) #t)
                             (else (display (car results)) #f))))
    (format #t pep8-message (if check-status "SUCCESS" "FAILURE"))
    check-status))

(define (run-pylint-check files)
  ;; Check the list of Python files with PyLint
  (let* ((results (run-command (format #f pylint-checker (string-join files))))
         (score (pylint-score (car results)))
         (check-status (cond ((or (eq? score 0) (> score min-pylint-score)) #t)
                             (else (display (car results)) #f))))
    (format #t pylint-message score)
    check-status))

(define (pylint-score output)
  ;; Parse PyLint output and return its score.
  ;; 0 is returned of no score was found.
  (let ((output-match (string-match pylint-score-line-regex output)))
    (if output-match
        (let* ((score-string (match:substring output-match))
               (score-match (string-match pylint-score-regex score-string)))
          (if score-match
              (string->number (string-trim (match:substring score-match)))
              0))
        0)))

(define (run-command command)
  ;; Run the given command string and return the output and exit status
  (let ((port (open-input-pipe command)))
    (cons (get-string-all port) (status:exit-val (close-pipe port)))))

(define (gitignore-files path)
  ;; List the files contained in the gitignore files at path
  (if (file-exists? path)
      (let* ((port (open-input-file path))
             (text (get-string-all port)))
        (close-port port)
        ; remove empty lines and comments
        (filter (lambda (line)
                  (and (not (string=? line ""))
                       (not (eq? (string-ref line 0) #\#))))
                (string-split text #\newline)))
      '()))

(define (list-staged-files)
  ;; List the files staged by Git
  (let ((results (run-command "git diff --cached --name-only")))
    (cond ((eq? (cdr results) 0)
           (string-split (car results) #\newline))
          ((display (car results)) '()))))

(define (list-included-files root exclusions)
  ;; List the files not excluded via .gitignore
  (filter (lambda (path)
            (not (any (lambda (exclusion)
                        (fnmatch exclusion path 0)) exclusions)))
          (traverse-filesystem (file-system-tree root))))

(define traverse-filesystem
  (match-lambda
   ((name stat) name)
   ((name stat children ...)
    (join-path name (map traverse-filesystem children)))))

(define (join-path parent children)
  (cond ((null? children) '())
        ((pair? (car children))
         (append (join-path parent (car children))
                 (join-path parent (cdr children))))
        (else (cons (string-join (list parent (car children)) "/")
                    (join-path parent (cdr children))))))
