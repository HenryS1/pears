(defpackage :pears
  (:use :cl :monad))

(in-package :pears)

(defmacro defer (a) `(lambda () ,a))

(defmacro lazy-cons (a tail)
  `(cons ,a (defer ,tail)))

(defstruct deferred-stream thunk)

(defun head (lazy-list) 
  (and lazy-list (car lazy-list)))

(defun tail (lazy-list) 
  (and lazy-list
    (let ((evaluated (cond ((null (cdr lazy-list)) nil)
                           ((functionp (cdr lazy-list)) (funcall (cdr lazy-list)))
                           (t (cdr lazy-list)))))
      (setf (cdr lazy-list) evaluated))))

(defun lazy-elems (&rest elems) elems)

(defstruct stream-end)
(defparameter *stream-end* (make-stream-end))

(defun lazy-stream (stream) 
  (labels ((rec (next-stream)
             (let ((next (read-char next-stream nil *stream-end*)))
               (etypecase next
                 (stream-end nil)
                 (t (lazy-cons next (rec next-stream)))))))
    (rec stream)))

(defun take-while (p lazy-stream)
  (labels ((rec (acc s)
             (if (null s)
                 (values (reverse acc) s)
                 (if (funcall p (car s))
                     (rec (cons (car s) acc) (tail s))
                     (values (reverse acc) s)))))
    (rec nil lazy-stream)))

(defun drop-while (p lazy-stream)
  (if (null lazy-stream) 
      lazy-stream
      (if (funcall p (car lazy-stream))
          (drop-while p (tail lazy-stream))
          lazy-stream)))

(defstruct parser f)

(defstruct failure)

(defparameter *failure* (make-failure))

(defun new-parser (f)
  (make-parser :f f))

(defun apply-parser (p strm)
  (funcall (parser-f p) strm))

(defun transform-sep-stream (parser sep-parser stream)
  (and stream
       (multiple-value-bind (result next-stream) (apply-parser parser stream)
         (if (eq result *failure*)
             (multiple-value-bind (sep-result sep-stream) (apply-parser sep-parser stream)
               (if (eq sep-result *failure*)
                   nil
                   (transform-sep-stream parser sep-parser sep-stream)))
             (lazy-cons result (transform-sep-stream parser sep-parser next-stream))))))

(defun transform-stream (parser stream)
  (and stream 
       (multiple-value-bind (result next-stream) (apply-parser parser stream)
         (if (eq result *failure*)
             *failure*
             (transform-stream parser next-stream)))))

(defun fold-stream (f stream val)
  (if (null stream)
      val
      (fold-stream f (tail stream) (funcall f val (head stream)))))

(defmethod fmap (f (p parser))
  (new-parser (lambda (strm) 
                (multiple-value-bind (result new-stream) (apply-parser p strm)
                  (if (eq result *failure*) 
                      *failure* 
                      (values (funcall f result) new-stream))))))

(defmethod flatmap (f (p parser))
  (new-parser (lambda (strm)
     (multiple-value-bind (result new-stream) (apply-parser p strm)
       (if (eq result *failure*)
           *failure*
           (apply-parser (funcall f result) new-stream))))))

(defmacro orp (&rest parsers)
  (if (null parsers)
      (error "orp must be given at least one parser")
      (let* ((reversed-parsers (reverse parsers))
             (stream (gensym))
             (parse-attempts (reduce (lambda (acc p)
                                      (let ((result (gensym))
                                            (next-stream (gensym)))
                                        `(multiple-value-bind (,result ,next-stream) 
                                             (apply-parser ,p ,stream)
                                           (if (eq ,result *failure*)
                                               ,acc
                                               (values ,result ,next-stream)))))
                                     (cdr reversed-parsers)
                                     :initial-value 
                                     `(apply-parser ,(car reversed-parsers) ,stream))))
        `(new-parser (lambda (,stream) ,parse-attempts)))))

(defun one (pred) 
  (new-parser (lambda (stream)
                (if (null stream)
                    *failure*
                    (let ((c (head stream)))
                      (if (funcall pred c)
                          (values (head stream) (tail stream))
                          *failure*))))))

(defun many (pred)
  (new-parser (lambda (stream) 
                (take-while pred stream))))

(defun many1 (pred)
  (mdo (fst (one pred))
       (rest (many pred))
       (yield (cons fst rest))))

(defun repeated (parser)
  (new-parser (lambda (stream)
                (labels ((rec (acc new-stream)
                           (multiple-value-bind (result next-stream) 
                               (apply-parser parser new-stream)
                             (if (eq result *failure*)
                                 (values (reverse acc) new-stream)
                                 (rec (cons result acc) next-stream)))))
                  (rec nil stream)))))

(defun repeated1 (parser)
  (new-parser (lambda (stream)
                (multiple-value-bind (result next-stream) 
                    (apply-parser (repeated parser) stream)
                  (if (or (eq result *failure*) (null result))
                      *failure*
                      (values result next-stream))))))

(defun sep-by (value-parser sep-parser)
  (let ((sep-parser (mdo (_ sep-parser)
                         (v value-parser)
                         (yield v))))
    (mdo (fst value-parser)
         (rest (repeated sep-parser))
         (yield (cons fst rest)))))

(defun discard (pred)
  (new-parser (lambda (stream)
                (values nil (drop-while pred stream)))))

(defun whitespacep (c)
  (or (char= c #\space) (char= c #\newline)
      (char= c #\return) (char= c #\tab)))

(defun ignore-whitespace () (discard #'whitespacep))

(defun char1 (c)
  (one (lambda (ch) (char= c ch))))

(defun newlinep (c) (or (char= c #\newline) (char= c #\return)))

(defun seq (s &key (test #'equal))
  (labels ((rec (i cur-stream)
             (if (= i (length s))
                 (values s cur-stream)
                 (if (null cur-stream)
                     *failure*
                     (if (funcall test (aref s i) (head cur-stream))
                         (rec (+ i 1) (tail cur-stream))
                         *failure*)))))
    (new-parser (lambda (stream) (rec 0 stream)))))

(defun optional (parser)
  (new-parser (lambda (stream)
                (multiple-value-bind (result next-stream) (apply-parser parser stream)
                  (if (eq result *failure*)
                      (values nil stream)
                      (values (list result) next-stream))))))

(defun repeat-n (predicate n)
  (new-parser (lambda (stream)
                (labels ((rec (acc i cur-stream)
                           (if (= i 0)
                               (values (reverse acc) cur-stream)
                               (if (null cur-stream)
                                   *failure*
                                   (if (funcall predicate (head cur-stream))
                                       (rec (cons (head cur-stream) acc) (- i 1) (tail cur-stream))
                                       *failure*)))))
                  (rec nil n stream)))))

(defun digits-to-int (digits)
  (loop for d in digits
     for n = (digit-char-p d) then (+ (* n 10) (digit-char-p d))
     finally (return n)))

(defun non-zero-digit ()
  (one (lambda (c) (and (digit-char-p c) (char/= c #\0)))))

(defparameter *positive-int* (fmap #'digits-to-int 
                                   (mdo (fst (non-zero-digit))
                                        (rest (many #'digit-char-p))
                                        (yield (cons fst rest)))))

(defparameter *non-negative-int* (orp (mdo (_ (char1 #\0)) (yield 0)) *positive-int*))

(defun row () (sep-by *positive-int* (char1 #\,)))

(defun csv ()
  (sep-by (row) (one (lambda (c) (char= c #\newline)))))

(defun json ()
  (mdo (_ (ignore-whitespace))
       (v (orp (json-string)
               (json-number)
               (json-object)
               (json-array)
               (json-boolean)
               (json-null)))
       (yield v)))

(defun json-boolean ()
  (orp (mdo (_ (seq "true")) (yield t)) 
       (mdo (_ (seq "false")) (yield nil))))

(defun json-null ()
  (mdo (_ (seq "null")) (yield nil)))

(defun hexp (c)
  (let ((code (char-code c)))
    (or (<= 48 code 57)
        (<= 65 code 70)
        (<= 97 code 102))))

(defun unicode-char ()
  (mdo (_ (char1 #\\))
       (_ (char1 #\u))
       (cs (repeat-n #'hexp 4))
       (yield (cons #\\ (cons #\u cs)))))

(defun escaped-character ()
  (orp 
   (unicode-char)
   (mdo (_ (char1 #\\))
        (c (orp (char1 #\")
                (char1 #\\)
                (char1 #\/)
                (char1 #\b)
                (char1 #\f)
                (char1 #\n)
                (char1 #\r)
                (char1 #\t)))
        (yield (list #\\ c)))))

(defun json-string ()
  (mdo (_ (char1 #\"))
       (cs (fmap (lambda (ls) (apply #'append ls))
                 (repeated (orp (many1 (lambda (c) 
                                        (and (not (char= c #\\))
                                             (not (char= c #\")))))
                                (escaped-character)))))
       (_ (char1 #\"))
       (yield (coerce cs 'string))))

(defun json-key-value ()
  (mdo (_ (ignore-whitespace))
       (key (json-string))
       (_ (ignore-whitespace))
       (_ (char1 #\:))
       (value (json))
       (_ (ignore-whitespace))
       (yield (cons key value))))

(defun alist-to-hash-table (alist)
  (let ((table (make-hash-table :test 'equal)))
    (mapc (lambda (e) (setf (gethash (car e) table) (cdr e))) alist)
    table))

(defun json-object ()
  (mdo (_ (ignore-whitespace))
       (_ (char1 #\{))
       (_ (ignore-whitespace))
       (es (sep-by (json-key-value) (char1 #\,)))
       (_ (ignore-whitespace))
       (_ (char1 #\}))
       (yield (alist-to-hash-table es))))

(defun json-array ()
  (mdo (_ (char1 #\[))
       (is (sep-by (mdo (v (json)) (_ (ignore-whitespace)) (yield v)) (char1 #\,)))
       (_ (char1 #\]))
       (yield is)))

(defun fractional-part ()
  (mdo (_ (char1 #\.))
       (ds (many1 #'digit-char-p))
       (yield (/ (digits-to-int ds) (expt 10.0 (length ds))))))

(defun exponent-part ()
  (mdo (_ (orp (char1 #\e)
               (char1 #\E)))
       (op (orp (mdo (_ (char1 #\-)) (yield #'/))
                (mdo (_ (char1 #\+)) (yield #'*))))
       (ex *non-negative-int*)
       (yield (lambda (n) (funcall op n (expt 10.0 ex))))))

(defun json-number ()
  (mdo (negate (optional (char1 #\-)))
       (integral-part (orp (mdo (_ (char1 #\0)) (yield 0))
                           *positive-int*))
       (f (optional (fractional-part)))
       (e (optional (exponent-part)))
       (let (n (if f (+ (car f) integral-part) integral-part)))
       (let (with-exp (if e (funcall (car e) n) n)))
       (yield (if negate (- with-exp) with-exp))))
