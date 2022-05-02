(defpackage :pears
  (:use :cl :monad))

(in-package :pears)

(defmacro defer (a) `(lambda () ,a))

(defmacro lazy-cons (a tail)
  `(cons ,a (defer ,tail)))

(defstruct deferred-stream thunk)

(defstruct bs
  (buffer (make-array 1000000 :element-type 'character
                      :adjustable nil) :type (simple-array character))
  strm
  (pointer 0 :type (unsigned-byte 32))
  (remaining 0 :type (unsigned-byte 32)))

(defun ensure-available (buff-stream)
  (declare (optimize (speed 3)))
  (when (= (bs-remaining buff-stream) 0)
    (setf (bs-remaining buff-stream) (read-sequence (bs-buffer buff-stream) (bs-strm buff-stream)))
    (setf (bs-pointer buff-stream) 0)))

(defun next-el (buff-stream)
  (declare (optimize (speed 3)))
  (ensure-available buff-stream)
  (if (= (bs-remaining buff-stream) 0)
      nil
      (let ((value (aref (bs-buffer buff-stream) (bs-pointer buff-stream))))
        (incf (bs-pointer buff-stream))
        (decf (bs-remaining buff-stream))
        value)))

(defstruct stream-end)
(defparameter *stream-end* (make-stream-end))

(defstruct indexed-stream buffer)

(defun stream-subseq (indexed-stream start end)
  (subseq (indexed-stream-buffer indexed-stream) start end))

(defun get-entry (indexed-stream i)
  (let ((buffer (indexed-stream-buffer indexed-stream)))
    (if (< i (length buffer))
        (aref buffer i)
        *stream-end*)))

(defun indexed-file-stream (file-stream)
  (let ((buffer (make-array (file-length file-stream) :element-type 'character :adjustable nil)))
    (declare ((simple-array character) buffer))
    (read-sequence buffer file-stream)
    (make-indexed-stream :buffer buffer)))

(defun indexed-string-stream (str)
  (make-indexed-stream :buffer str))

(defun buffer-stream (strm)
  (declare (optimize (speed 3)))
  (let ((buffered (make-bs :strm strm)))
    (labels ((rec (next-stream)
               (let ((next (next-el buffered)))
                 (if (eq next *stream-end*)
                     nil
                     (lazy-cons next (rec next-stream))))))
      (rec strm))))

(defun head (lazy-list) 
  (and lazy-list (car lazy-list)))

(defun tail (lazy-list) 
  (and lazy-list
    (let ((evaluated (cond ((null (cdr lazy-list)) nil)
                           ((functionp (cdr lazy-list)) (funcall (cdr lazy-list)))
                           (t (cdr lazy-list)))))
      (setf (cdr lazy-list) evaluated))))

(defun lazy-elems (&rest elems) elems)

(defun lazy-stream (stream) 
  (labels ((rec (next-stream)
             (let ((next (read-char next-stream nil *stream-end*)))
               (etypecase next
                 (stream-end nil)
                 (t (lazy-cons next (rec next-stream)))))))
    (rec stream)))

(defun take-while (p stream i)
  (labels ((rec (end)
             (let ((entry (get-entry stream end)))
               (etypecase entry
                 (stream-end end)
                 (t (if (funcall p entry)
                        (rec (+ end 1))
                        end))))))
    (rec i)))

(defun drop-while (p lazy-stream i)
  (let ((entry (get-entry lazy-stream i)))
    (etypecase entry
      (stream-end i)
      (t (if (funcall p entry)
             (drop-while p lazy-stream (+ i 1))
             i)))))

(defstruct parser f)

(defstruct failure)

(defparameter *failure* (make-failure))

(defun new-parser (f)
  (make-parser :f f))

(defun apply-parser (p strm i)
  (funcall (parser-f p) strm i))

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
                (multiple-value-bind (result new-i) (apply-parser p strm i)
                  (if (eq result *failure*) 
                      *failure* 
                      (values (funcall f result) new-i))))))

(defmethod flatmap (f (p parser))
  (new-parser (lambda (strm i)
     (multiple-value-bind (result new-i) (apply-parser p strm i)
       (if (eq result *failure*)
           *failure*
           (apply-parser (funcall f result) strm new-i))))))

(defmacro sequential (&rest body)
  (let ((stream (gensym)) (init-i (gensym)))
    (labels ((nest (parser-bindings cur-i value-form)
               (if (null parser-bindings)
                   `(values ,value-form ,cur-i)
                   (let* ((bindings (car parser-bindings))
                          (result-binding (car bindings))
                          (cur-parser (cadr bindings))
                          (next-result (gensym))
                          (next-i (gensym)))
                     `(multiple-value-bind (,next-result ,next-i)
                          (apply-parser ,cur-parser ,stream ,cur-i)
                        (if (eq ,next-result *failure*)
                            *failure*
                            ,(if (string= (symbol-name result-binding) "_")
                                 (nest (cdr parser-bindings) next-i value-form)
                                 `(let ((,result-binding ,next-result))
                                    ,(nest (cdr parser-bindings) next-i value-form)))))))))
      (let* ((parser-bindings (butlast body))
             (evaluated-parsers (loop for (b p) in parser-bindings collect (list (gensym) nil)))
             (bindings (loop for (b p) in parser-bindings for (sym nl) in evaluated-parsers
                          collect `(,b (or ,sym (setf ,sym ,p)))))
             (value-form (car (last body))))
        `(new-parser (let ,evaluated-parsers 
                       (lambda (,stream ,init-i)
                         ,(nest bindings init-i value-form))))))))

(defmacro orp (&rest parsers)
  (if (null parsers)
      (error "orp must be given at least one parser")
      (let* ((reversed-parsers (reverse parsers))
             (evaluated-parsers (loop for p in reversed-parsers collect `(,(gensym) nil)))
             (parsers (mapcar (lambda (sym p) `(or ,(car sym) (setf ,(car sym) ,p))) 
                              evaluated-parsers reversed-parsers))
             (stream (gensym))
             (init-i (gensym))
             (parse-attempts (reduce (lambda (acc p)
                                      (let ((result (gensym))
                                            (next-i (gensym)))
                                        `(multiple-value-bind (,result ,next-i) 
                                             (apply-parser ,p ,stream ,init-i)
                                           (if (eq ,result *failure*)
                                               ,acc
                                               (values ,result ,next-i)))))
                                     (cdr parsers)
                                     :initial-value 
                                     `(apply-parser ,(car parsers) ,stream ,init-i))))
        `(new-parser (let ,evaluated-parsers 
                       (lambda (,stream ,init-i) ,parse-attempts))))))

(defun one (pred) 
  (new-parser (lambda (stream i)
                (let ((entry (get-entry stream i)))
                  (etypecase entry
                    (stream-end *failure*)
                    (t (if (funcall pred entry)
                           (values entry (+ i 1))
                           *failure*)))))))

(defun many (pred)
  (new-parser (lambda (stream i) 
                (let ((end (take-while pred stream i)))
                  (values (stream-subseq stream i end) end)))))

(defun many1 (pred)
  (new-parser (lambda (stream i)
                (let ((end (take-while pred stream i)))
                  (if (= i end)
                      *failure*
                      (values (stream-subseq stream i end) end))))))

(defun repeated (parser)
  (new-parser (lambda (stream i)
                (loop for end-prev = i then end
                   for (result end) = (multiple-value-list (apply-parser parser stream i))
                   then (multiple-value-list (apply-parser parser stream end))
                   while (not (eq result *failure*))
                   collect result into results
                   finally (return (values results end-prev))))))

(defun repeated1 (parser)
  (new-parser (lambda (stream i)
                (loop for end-prev = i then end
                   for (result end) = (multiple-value-list (apply-parser parser stream i))
                   then (multiple-value-list (apply-parser parser stream end))
                   while (not (eq result *failure*))
                   collect result into results
                   finally (return (if (null results)
                                       *failure*
                                       (values results end-prev)))))))

(defun sep-by (value-parser sep-parser)
  (let ((sep-parser (sequential (_ sep-parser)
                                (v value-parser)
                                v)))
    (sequential (fst value-parser)
                (rest (repeated sep-parser))
                (cons fst rest))))

(defun discard (pred)
  (new-parser (lambda (stream i)
                (values nil (drop-while pred stream i)))))

(defun whitespacep (c)
  (or (char= c #\space) (char= c #\newline)
      (char= c #\return) (char= c #\tab)))

(defun ignore-whitespace () (discard #'whitespacep))

(defun char1 (c)
  (one (lambda (ch) (char= c ch))))

(defun newlinep (c) (or (char= c #\newline) (char= c #\return)))

(defun seq (s &key (test #'equal))
  (labels ((rec (stream cnt i)
             (if (= cnt (length s))
                 (values s i)
                 (let ((entry (get-entry stream i)))
                   (etypecase entry
                     (stream-end *failure*)
                     (t (if (funcall test (aref s cnt) entry)
                          (rec stream (+ cnt 1) (+ i 1))
                          *failure*)))))))
    (new-parser (lambda (stream i) (rec stream 0 i)))))

(defun optional (parser)
  (new-parser (lambda (stream i)
                (multiple-value-bind (result next-i) (apply-parser parser stream i)
                  (if (eq result *failure*)
                      (values nil i)
                      (values (list result) next-i))))))

(defun manyn (predicate n)
  (new-parser (lambda (stream i)
                (loop for cur-i = i then (+ cur-i 1)
                   for cnt = 0 then (+ cnt 1)
                   for entry = (get-entry stream cur-i)
                   while (and (funcall predicate entry) (< cnt n))
                   collect entry into results
                   finally (return (if (= cnt n)
                                       (values results i)
                                       *failure*))))))

(defun digits-to-int (digits)
  (loop for d across digits
     for n = (digit-char-p d) then (+ (* n 10) (digit-char-p d))
     finally (return n)))

(defun non-zero-digit ()
  (one (lambda (c) (and (digit-char-p c) (char/= c #\0)))))

(defparameter *positive-int* (sequential (fst (non-zero-digit))
                                         (rest (many #'digit-char-p))
                                         (+ (* (expt 10 (length rest)) 
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
  (let ((table (make-hash-table :test 'equal)))
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

(defun json-number ()
  (sequential (negate (optional (char1 #\-)))
              (integral-part (orp (sequential (_ (char1 #\0)) 0)
                                  *positive-int*))
              (f (optional (fractional-part)))
              (e (optional (exponent-part)))
              (let* ((n (if f (+ (car f) integral-part) integral-part))
                     (with-exp (if e (funcall (car e) n) n)))
                (if negate (- with-exp) with-exp))))
