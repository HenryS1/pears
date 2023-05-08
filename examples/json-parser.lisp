(defpackage json-benchmark
  (:use :cl :pears))

(in-package :json-benchmark)

(defun json ()
  (sequential (_ (ignore-whitespace))
              (v (orp (json-string)
                      (json-number)
                      (json-object)
                      (json-array)
                      (json-boolean)
                      (json-null)))
              v))

(defun json-boolean ()
  (orp (sequential (_ (seq "true")) t) 
       (sequential (_ (seq "false")) nil)))

(defun json-null ()
  (sequential (_ (seq "null")) nil))

(defun hexp (c)
  (let ((code (char-code c)))
    (or (<= 48 code 57)
        (<= 65 code 70)
        (<= 97 code 102))))

(defun unicode-char ()
  (sequential (_ (char1 #\\))
              (_ (char1 #\u))
              (cs (manyn #'hexp 4))
              (code-char (parse-integer cs :radix 16))))

(defun escaped-character ()
  (orp 
   (unicode-char)
   (sequential (_ (char1 #\\))
               (c (orp (char1 #\")
                       (char1 #\\)
                       (char1 #\/)
                       (sequential (_ (char1 #\b)) #\backspace)
                       (sequential (_ (char1 #\f)) #\formfeed)
                       (sequential (_ (char1 #\n)) #\linefeed)
                       (sequential (_ (char1 #\r)) #\return)
                       (sequential (_ (char1 #\t)) #\tab)))
               c)))

(defun json-string ()
  (sequential (_ (char1 #\"))
              (cs (repeated (orp (many1 (lambda (c) 
                                          (and (not (char= c #\\))
                                               (not (char= c #\")))))
                                 (escaped-character))))              
              (_ (char1 #\"))
              (format nil "~{~a~}" cs)))

(defun json-key-value ()
  (sequential (_ (ignore-whitespace))
              (key (json-string))
              (_ (ignore-whitespace))
              (_ (char1 #\:))
              (value (json))
              (_ (ignore-whitespace))
              (cons key value)))

(defun alist-to-hash-table (alist)
  (let ((table (make-hash-table :test 'equal :size (length alist))))
    (mapc (lambda (e) (setf (gethash (car e) table) (cdr e))) alist)
    table))

(defun json-object ()
  (sequential (_ (ignore-whitespace))
              (_ (char1 #\{))
              (_ (ignore-whitespace))
              (keys-and-values (sep-by (json-key-value) (char1 #\,)))
              (_ (ignore-whitespace))
              (_ (char1 #\}))
              (alist-to-hash-table keys-and-values)))

(defun digits-to-int (digits)
  (loop for d across digits
        for n = (digit-char-p d) then (+ (* n 10) (digit-char-p d))
        finally (return n)))

(defun json-array ()
  (sequential (_ (char1 #\[))
              (is (sep-by (sequential (v (json)) (_ (ignore-whitespace)) v) (char1 #\,)))
              (_ (char1 #\]))
              is))

(defun fractional-part ()
  (sequential (_ (char1 #\.))
              (ds (many1 #'digit-char-p))
              (/ (digits-to-int ds) (expt 10.0 (length ds)))))

(defun exponent-part ()
  (sequential (_ (orp (char1 #\e)
                      (char1 #\E)))
              (op (orp (sequential (_ (char1 #\-)) #'/)
                       (sequential (_ (char1 #\+)) #'*)))
              (ex *non-negative-int*)
              (lambda (n) (funcall op n (expt 10.0 ex)))))

(defun integral-part ()
  (sequential 
   (i (orp (sequential (_ (char1 #\0)) 0)
           *positive-int*))
          i))

(defun json-number ()
  (sequential (negate (optional (char1 #\-)))
              (int (integral-part))
              (f (optional (fractional-part)))
              (e (optional (exponent-part)))
              (let* ((n (if f (+ (car f) int) int))
                     (with-exp (if e (funcall (car e) n) n)))
                (if negate (- with-exp) with-exp))))

(defun run-benchmark ()
  (let ((directory (asdf:system-source-directory :pears))) 
    (time (with-open-file (file (format nil "~a/tests/large_file.json" directory))
       (parse-stream (json) file)))))
