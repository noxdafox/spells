#!/usr/bin/guile \
-e main -s
!#

(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (ice-9 popen))
(use-modules (ice-9 regex))
(use-modules (ice-9 textual-ports))
(use-modules (ice-9 curried-definitions))

(define python-file-regex ".*.py$")
(define gitignore-file ".gitignore")

(define pep8-checker
  "pep8 --max-line-length=80 --exclude=~s ~a")
(define pylint-checker
  "pylint3 --max-line-length=80 --ignore=~s ~a 2> /dev/null")

(define min-pylint-score 8)
(define pylint-score-regex "[ 0-9].[0-9]")
(define pylint-score-line-regex "Your code has been rated at [ 0-9].[0-9]")

(define (main args)
  (let ((python-files (find-python-files "."))
        (git-exclusions (gitignore-files gitignore-file)))
    ; Python project validation
    (when (not (null? python-files))
          (if (run-pep8-check python-files git-exclusions)
              (if (run-pylint-check python-files git-exclusions)
                  (exit 0)))
          (exit 1))))

(define (run-pep8-check files exclusions)
  ;; Check the list of Python files with Pep8
  (let ((results (run-checker pep8-checker files exclusions)))
    (cond ((eq? (cdr results) 0) #t)
          (else (display (car results)) #f))))

(define (run-pylint-check files exclusions)
  ;; Check the list of Python files with PyLint
  (let* ((results (run-checker pylint-checker files exclusions))
         (score (pylint-score (car results))))
    (cond ((or (eq? score 0) (> score min-pylint-score)) #t)
          (else (display (car results)) #f))))

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

(define (run-checker checker-command files exclusions)
  ;; Run the given checker command string against the given list of files
  ;; Returns a pair containing the command output and its exit status
  (let* ((command (format #f checker-command
                          (string-join exclusions ",") (string-join files)))
         (port (open-input-pipe command)))
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

(define (find-python-files root)
  ;; Search all Python files in the folder given as root
  (filter (lambda (path)
            (string-match python-file-regex path))
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
