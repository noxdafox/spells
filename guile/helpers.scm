;; fnmatch thanks to Matt Wette

(define-module (helpers)
  #:use-module (system foreign)
  #:export (FNM_NOMATCH
            FNM_NOESCAPE
            FNM_PATHNAME
            FNM_PERIOD
            FNM_LEADING_DIR
            FNM_CASEFOLD
            FNM_IGNORECASE
            FNM_IGNORECASE
            FNM_FILE_NAME
            fnmatch
            any))

(define FNM_NOMATCH 1)
(define FNM_NOESCAPE #x01)
(define FNM_PATHNAME #x02)
(define FNM_PERIOD #x04)

(define FNM_LEADING_DIR #x08)
(define FNM_CASEFOLD #x10)
(define FNM_IGNORECASE FNM_CASEFOLD)
(define FNM_FILE_NAME FNM_PATHNAME)

(define fnmatch
  ;; (fnmatch "*.a" "foo.c" (logior FNM_PERIOD FNM_NOESCAPE))
  ;; #f
  (let ((~fnmatch (delay
		    (pointer->procedure
		     int
		     (dynamic-func "fnmatch" (dynamic-link))
		     (list '* '* int)))))
    (lambda (pattern string flags)
      (if (eq? ((force ~fnmatch)
                (string->pointer pattern)
                (string->pointer string)
                flags) 0)
          #t
          #f))))

(define (any proc lst)
  ;; return #t if any element of lst matches proc, #f otherwise
  (cond ((null? lst) #f)
        ((proc (car lst)) #t)
        (else (any proc (cdr lst)))))
