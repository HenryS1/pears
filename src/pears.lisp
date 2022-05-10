(defpackage :pears
  (:use :cl :monad))

(in-package :pears)

(defmacro defer (a) `(lambda () ,a))

(defstruct stream-end)
(defparameter *stream-end* (make-stream-end))

(defstruct indexed-stream
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32))
  (buffer (vector) :type (vector))
  (stream nil :type t)
  (next nil :type t))

(defun stream-subseq (indexed-stream start end)
  (declare (optimize (speed 3)) ((unsigned-byte 32) start end))
  (cond ((null indexed-stream) nil)
        ((and (>= start (indexed-stream-start indexed-stream))
              (<= end (+ (indexed-stream-start indexed-stream)
                         (length (indexed-stream-buffer indexed-stream)))))
         (subseq (indexed-stream-buffer indexed-stream) 
                 (- start (indexed-stream-start indexed-stream))
                 (- end (indexed-stream-start indexed-stream))))
        ((>= start (indexed-stream-end indexed-stream))
         (stream-subseq (indexed-stream-next indexed-stream) start end))
        (t (apply #'concatenate 'string
                  (loop for stream = indexed-stream then (indexed-stream-next stream)
                        until (< end (+ (indexed-stream-end stream) 1))
                        collect (subseq (indexed-stream-buffer stream)
                                        (max 0 (- start (indexed-stream-start stream)))
                                        (min (length (indexed-stream-buffer stream))
                                             (- end (indexed-stream-start stream)))))))))

(defparameter *buffer-size* 100000)

(defun read-stream-chunk (start stream)
  (let ((buffer (make-array *buffer-size* :element-type 'character :adjustable nil)))
    (make-indexed-stream 
     :start start 
     :buffer buffer 
     :end (+ start (read-sequence buffer stream))
     :stream stream
     :next nil)))

(defun indexed-string-stream (str)
  (make-indexed-stream
   :start 0
   :buffer str
   :end (length str)
   :stream nil
   :next nil))

(defun get-entry (indexed-stream i)
  (declare ((unsigned-byte 32) i) (optimize (speed 3)))
  (etypecase indexed-stream
    (stream-end (values i *stream-end*))
    (t (cond ((< i (indexed-stream-end indexed-stream))
              (values (aref (indexed-stream-buffer indexed-stream) 
                            (- i (indexed-stream-start indexed-stream)))
                      indexed-stream))
             ((< (indexed-stream-end indexed-stream)
                 (+ (indexed-stream-start indexed-stream)
                    (length (indexed-stream-buffer indexed-stream)))) (values nil *stream-end*))
             ((null (indexed-stream-stream indexed-stream)) (values nil *stream-end*))
             (t (when (null (indexed-stream-next indexed-stream))
                  (setf (indexed-stream-next indexed-stream)
                        (read-stream-chunk (indexed-stream-end indexed-stream) 
                                           (indexed-stream-stream indexed-stream))))
                (get-entry (indexed-stream-next indexed-stream) i))))))

(defun indexed-file-stream (file-stream)
  (read-stream-chunk 0 file-stream))

(defun take-while (p stream i)
  (labels ((rec (stream end)
             (declare (fixnum end i))
             (multiple-value-bind (entry next-stream) (get-entry stream end)
               (etypecase next-stream
                 (stream-end (values next-stream end))
                 (t (if (funcall (the function p) entry)
                        (rec next-stream (+ end 1))
                        (values next-stream end)))))))
    (rec stream i)))

(defun drop-while (p lazy-stream i)
  (declare (fixnum i))
  (multiple-value-bind (entry next-stream) (get-entry lazy-stream i)
    (etypecase entry
      (stream-end (values next-stream i))
      (t (if (funcall (the function p) entry)
             (drop-while p next-stream (+ i 1))
             (values next-stream i))))))

(defstruct parser f)

(defstruct failure)

(defparameter *failure* (make-failure))

(defun new-parser (f)
  (make-parser :f f))

(defun apply-parser (p strm i)
  (funcall (the function (parser-f p)) strm i))

;; (defun transform-sep-stream (parser sep-parser stream)
;;   (labels ((rec (i)
;;              (and stream
;;                   (multiple-value-bind (result next-i) (apply-parser parser stream 0)
;;                     (if (eq result *failure*)
;;                         (multiple-value-bind (sep-result sep-i) (apply-parser sep-parser stream next-i)
;;                           (if (eq sep-result *failure*)
;;                               nil
;;                               (transform-sep-stream parser sep-parser stream sep-i)))
;;                         (lazy-cons result (transform-sep-stream parser sep-parser next-i))))))) ))

;; (defun transform-stream (parser stream)
;;   (and stream 
;;        (multiple-value-bind (result next-stream) (apply-parser parser stream)
;;          (if (eq result *failure*)
;;              *failure*
;;              (transform-stream parser next-stream)))))

;; (defun fold-stream (f stream val)
;;   (if (null stream)
;;       val
;;       (fold-stream f (tail stream) (funcall f val ( stream)))))

(defmethod fmap (f (p parser))
  (new-parser (lambda (strm i) 
                (multiple-value-bind (result new-strm new-i) (apply-parser p strm i)
                  (etypecase result 
                    (failure result)
                    (t (values (funcall (the function f) result) new-strm new-i)))))))

(defmethod flatmap (f (p parser))
  (new-parser (lambda (strm i)
     (multiple-value-bind (result new-stream new-i) (apply-parser p strm i)
       (etypecase result
         (failure result)
         (t (apply-parser (funcall (the function f) result) new-stream new-i)))))))

(defmacro sequential (&rest body)
  (let ((init-stream (gensym)) (init-i (gensym)))
    (labels ((nest (parser-bindings cur-stream cur-i value-form)
               (if (null parser-bindings)
                   `(values ,value-form ,cur-stream ,cur-i)
                   (let* ((bindings (car parser-bindings))
                          (result-binding (car bindings))
                          (cur-parser (cadr bindings))
                          (next-result (gensym))
                          (next-i (gensym))
                          (next-stream (gensym)))
                     `(multiple-value-bind (,next-result ,next-stream ,next-i)
                          (apply-parser ,cur-parser ,cur-stream ,cur-i)
                        (etypecase ,next-result 
                          (failure ,next-result)
                          (t ,(if (string= (symbol-name result-binding) "_")
                                  (nest (cdr parser-bindings) next-stream next-i value-form)
                                  `(let ((,result-binding ,next-result))
                                     ,(nest (cdr parser-bindings) 
                                            next-stream next-i value-form))))))))))
      (let* ((parser-bindings (butlast body))
             (evaluated-parsers (loop for (b p) in parser-bindings collect (list (gensym) nil)))
             (bindings (loop for (b p) in parser-bindings for (sym nl) in evaluated-parsers
                          collect `(,b (or ,sym (setf ,sym ,p)))))
             (value-form (car (last body))))
        `(new-parser (let ,evaluated-parsers 
                       (lambda (,init-stream ,init-i)
                         ,(nest bindings init-stream init-i value-form))))))))

(defmacro orp (&rest parsers)
  (if (null parsers)
      (error "orp must be given at least one parser")
      (let* ((reversed-parsers (reverse parsers))
             (evaluated-parsers (loop for p in reversed-parsers collect `(,(gensym) nil)))
             (parsers (mapcar (lambda (sym p) `(or ,(car sym) (setf ,(car sym) ,p))) 
                              evaluated-parsers reversed-parsers))
             (init-stream (gensym))
             (init-i (gensym))
             (parse-attempts (reduce (lambda (acc p)
                                      (let ((result (gensym))
                                            (next-i (gensym))
                                            (next-stream (gensym)))
                                        `(multiple-value-bind (,result ,next-stream ,next-i) 
                                             (apply-parser ,p ,init-stream ,init-i)
                                           (etypecase ,result
                                             (failure ,acc) 
                                             (t (values ,result ,next-stream ,next-i))))))
                                     (cdr parsers)
                                     :initial-value 
                                     `(apply-parser ,(car parsers) ,init-stream ,init-i))))
        `(new-parser (let ,evaluated-parsers 
                       (lambda (,init-stream ,init-i) ,parse-attempts))))))

(defun one (pred) 
  (new-parser (lambda (stream i)
                (declare (fixnum i))
                (multiple-value-bind (entry next-stream) (get-entry stream i)
                  (etypecase next-stream
                    (stream-end *failure*)
                    (t (if (funcall (the function pred) entry)
                           (values entry next-stream (+ i 1))
                           *failure*)))))))

(defun many (pred)
  (new-parser (lambda (stream i) 
                (multiple-value-bind (next-stream end) (take-while pred stream i)
                  (values (stream-subseq stream i end) next-stream end)))))

(defun many1 (pred)
  (new-parser (lambda (stream i)
                (declare (fixnum i))
                (multiple-value-bind (next-stream end) (take-while pred stream i)
                  (declare (fixnum end))
                  (if (= i end)
                      *failure*
                      (values (stream-subseq stream i end) next-stream end))))))

(defun repeated (parser)
  (new-parser (lambda (stream i)
                (loop for end-prev = i then end
                   for stream-prev = stream then next-stream
                   for (result next-stream end) = (multiple-value-list
                                                  (apply-parser parser stream i))
                   then (multiple-value-list (apply-parser parser next-stream end))
                   while (etypecase result (failure nil) (t t))
                   collect result into results
                   finally (return (values results stream-prev end-prev))))))

(defun repeated1 (parser)
  (new-parser (lambda (stream i)
                (loop for end-prev = i then end
                   for stream-prev = stream then next-stream
                   for (result next-stream end) = (multiple-value-list
                                                   (apply-parser parser stream i))
                   then (multiple-value-list (apply-parser parser next-stream end))
                   while (etypecase result (failure nil) (t t))
                   collect result into results
                   finally (return (if (null results)
                                       *failure*
                                       (values results stream-prev end-prev)))))))

(defun sep-by (value-parser sep-parser)
  (let ((sep-parser (sequential (_ sep-parser)
                                (v value-parser)
                                v)))
    (sequential (fst value-parser)
                (rest (repeated sep-parser))
                (cons fst rest))))

(defun discard (pred)
  (new-parser (lambda (stream i)
                (multiple-value-bind (next-stream next-i) (drop-while pred stream i)
                  (values nil next-stream next-i)))))

(defun whitespacep (c)
  (or (char= c #\space) (char= c #\newline)
      (char= c #\return) (char= c #\tab)))

(defun ignore-whitespace () (discard #'whitespacep))

(defun char1 (c)
  (one (lambda (ch) (char= c ch))))

(defun newlinep (c) (or (char= c #\newline) (char= c #\return)))

(defun seq (s &key (test #'equal))
  (declare (vector s))
  (labels ((rec (stream cnt i)
             (declare (fixnum cnt i))
             (if (= cnt (length s))
                 (values s stream i)
                 (multiple-value-bind (entry next-stream) (get-entry stream i)
                   (etypecase next-stream
                     (stream-end *failure*)
                     (t (if (funcall (the function test) (aref s cnt) entry)
                            (rec next-stream (+ cnt 1) (+ i 1))
                            *failure*)))))))
    (new-parser (lambda (stream i) (rec stream 0 i)))))

(defun optional (parser)
  (new-parser (lambda (stream i)
                (multiple-value-bind (result next-stream next-i) (apply-parser parser stream i)
                  (etypecase result
                    (failure (values nil stream i))
                    (t (values (list result) next-stream next-i)))))))

(defun manyn (predicate n)
  (new-parser (lambda (stream i)
                (declare (fixnum i n))
                (loop for cur-i = i then (+ cur-i 1)
                   for cnt = 0 then (+ cnt 1)
                   for (entry next-stream) = (multiple-value-list (get-entry stream cur-i))
                   while (and (not (eq next-stream *stream-end*))
                              (funcall (the function predicate) entry) (< cnt n))
                   collect entry into results
                   finally (return (if (= cnt n)
                                       (values next-stream results i)
                                       *failure*))))))

(defun digits-to-int (digits)
  (loop for d across digits
     for n = (digit-char-p d) then (+ (* n 10) (digit-char-p d))
     finally (return n)))

(defun non-zero-digit ()
  (one (lambda (c) (and (digit-char-p c) (char/= c #\0)))))

(defparameter *positive-int* (sequential (fst (non-zero-digit))
                                         (rest (many #'digit-char-p))
                                         (+ (* (expt 10 (length (the vector rest))) 
                                               (digit-char-p fst))
                                            (digits-to-int rest))))

(defparameter *non-negative-int* (orp (sequential (_ (char1 #\0)) 0) *positive-int*))

(defun row () (sep-by *positive-int* (char1 #\,)))

(defun csv ()
  (sep-by (row) (one (lambda (c) (char= c #\newline)))))

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
              (cons #\\ (cons #\u cs))))

(defun escaped-character ()
  (orp 
   (unicode-char)
   (sequential (_ (char1 #\\))
               (c (orp (char1 #\")
                       (char1 #\\)
                       (char1 #\/)
                       (char1 #\b)
                       (char1 #\f)
                       (char1 #\n)
                       (char1 #\r)
                       (char1 #\t)))
               (list #\\ c))))

(defun json-string ()
  (sequential (_ (char1 #\"))
              (cs (fmap (lambda (ls) (apply #'append ls))
                        (repeated (orp (many1 (lambda (c) 
                                                (and (not (char= c #\\))
                                                     (not (char= c #\")))))
                                       (escaped-character)))))
              (_ (char1 #\"))
              (coerce cs 'string)))

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
              (es (sep-by (json-key-value) (char1 #\,)))
              (_ (ignore-whitespace))
              (_ (char1 #\}))
              (alist-to-hash-table es)))

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
