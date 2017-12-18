#!/usr/bin/guile \
-e main -s
!#

;; INFO: list all Python projects at the given path
;; INFO: output in CSV form with ":" as a separator
;; INFO: Python projects are those with a setup.py file in their root folder
;;
;; INFO: Usage: <script> <folder> <max-traversing-depth>

(use-modules (ice-9 ftw))
(use-modules (ice-9 match))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-1))

(define dirname-regex (make-regexp ".*/"))
(define python-project-regex (make-regexp ".*/setup.py"))

(define (main args)
  (let ((path (string-trim-right (second args) #\/))
        (max-depth (string->number (last args))))
    (display (string-join (find-python-projects path max-depth) ":"))))

(define (find-python-projects root max-depth)
  (let ((current-depth 0))
    (define (enter? path stat result)
      (< current-depth max-depth))

    (define (leaf path stat result)
      (if (regexp-exec python-project-regex path)
          (cons (dirname path) result)
          result))

    (define (down path stat result)
      (set! current-depth (+ current-depth 1))
      result)
    (define (up path stat result)
      (set! current-depth (- current-depth 1))
      result)

    (define (skip path stat result) result)

    (define (error path stat errno result)
      (format (current-error-port) "warning: ~a: ~a~%" path (strerror errno))
      result)

    (file-system-fold enter? leaf down up skip error '() root)))

(define (dirname path)
  (match:substring (regexp-exec dirname-regex path)))
