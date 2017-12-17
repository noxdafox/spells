#!/usr/bin/guile \
-e main -s
!#

;; INFO: list all Python projects at the given path
;; INFO: output in CSV form with ":" as a separator
;; INFO: Python projects are those with a setup.py file in their root folder

(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))

(define max-depth 4)
(define dirname-regex (make-regexp ".*/"))
(define python-project-regex (make-regexp ".*/setup.py"))

(define (main args)
  (let ((path (string-trim-right (last args) #\/)))
    (display (string-join (find-python-projects path max-depth) ":"))))

(define (find-python-projects root max-depth)
  ;; Search all Python projects in the folder given as root
  (map (lambda (path)
         ; remove basename of both root and path
         (string-join (list (dirname root) (dirname path)) ""))
       (filter (lambda (path)
                   (regexp-exec python-project-regex path))
               (traverse-filesystem root max-depth))))

(define (traverse-filesystem root max-depth)
  ;; Traverse the filesystem from root up until max-depth is reached
  ;; return a flat list with the content of given folder
  (let ((depth 0))
    (filesystem-paths
     (file-system-tree
      root
      (lambda (name stat)
        (cond ((< depth max-depth) (set! depth (+ depth 1)) #t)
              (else (set! depth (- depth 1))#f)))))))

(define filesystem-paths
  ;; Discard the stat information retrieved by file-system-tree and join
  ;; the path of the files. Folders are discarded.
  (match-lambda
   ((name stat) name)
   ((name stat children ...)
    (join-path name (map filesystem-paths children)))))

(define (join-path parent children)
  (cond ((null? children) '())
        ((pair? (car children))
         (append (join-path parent (car children))
                 (join-path parent (cdr children))))
        (else (cons (string-join (list parent (car children)) "/")
                    (join-path parent (cdr children))))))

(define (dirname path)
  (match:substring (regexp-exec dirname-regex path)))
