#!/usr/bin/guile \
-e main -s
!#

;; INFO: list all Python projects at the given path
;; INFO: output in CSV form with ":" as a separator
;; INFO: Python projects are those matching the following paths
;; INFO:  - <path>/<project name>/<module name>/__init__.py
;; INFO:  - <path>/<project name>/lib/<module name>/__init__.py
;; INFO:  - <path>/<project name>/src/<module name>/__init__.py

(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))

(define max-depth 4)

;; foo/bar/baz/__init__.py
;; foo/bar/lib/baz/__init__.py
;; foo/bar/src/baz/__init__.py
(define python-project-regex
  (make-regexp "^[^/]+/[^/]+/((src|lib)/){0,1}[^/]+/__init__.py$"))
(define dirname-regex (make-regexp ".*/"))

(define (main args)
  (let* ((path (string-trim-right (last args) #\/))
         (dirname (string-trim-right
                   (match:substring (regexp-exec dirname-regex path)) #\/)))
    (display (string-join
              (join-path dirname (find-python-projects path max-depth)) ":"))))

(define (find-python-projects root max-depth)
  ;; Search all Python projects in the folder given as root
  (filter (lambda (path)
            (regexp-exec python-project-regex path))
          (traverse-filesystem (file-system-tree root) 0 max-depth)))

(define (traverse-filesystem root depth max-depth)
  ;; Traverses the filesystem from root stopping at the given max-depth
  (if (< depth max-depth)
      (match root
             ((name stat) name)
             ((name stat children ...)
              (join-path name (map (lambda (child)
                                     (traverse-filesystem
                                      child (+ 1 depth) max-depth))
                                   children))))
      ""))

(define (join-path parent children)
  (cond ((null? children) '())
        ((pair? (car children))
         (append (join-path parent (car children))
                 (join-path parent (cdr children))))
        (else (cons (string-join (list parent (car children)) "/")
                    (join-path parent (cdr children))))))