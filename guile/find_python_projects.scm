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
  (make-regexp ".*/setup.py"))
(define dirname-regex (make-regexp ".*/"))

(define (main args)
  (let ((path (string-trim-right (last args) #\/)))
    (display (string-join (find-python-projects path max-depth) ":"))))

(define (find-python-projects root max-depth)
  ;; Search all Python projects in the folder given as root
  (map (lambda (path)
         ; remove basename of both root and path
         (string-join (list (dirname root) (dirname path)) ""))
       (filter (lambda (path)
                 ; traverse the filesystem looking for python project folders
                 (regexp-exec python-project-regex path))
               (traverse-filesystem (file-system-tree root) 0 max-depth))))

(define (traverse-filesystem root depth max-depth)
  ;; Traverse the filesystem from root stopping at the given max-depth
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

(define (dirname path)
  (match:substring (regexp-exec dirname-regex path)))
