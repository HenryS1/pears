(defpackage :pears
  (:use :cl)
  (:export
   :make-indexed-stream 
   :stream-subseq
   :sequential
   :orp
   :read-stream-chunk
   :indexed-stream-buffer
   :indexed-stream-start
   :indexed-stream-end
   :indexed-stream-next
   :set-indexed-stream-next
   :new-indexed-stream
   :indexed-string-stream
   :get-entry
   :take-while
   *stream-end*
   *failure*
   :apply-parser
   :one
   :many
   :many1
   :repeated
   :repeated1
   :sep-by
   :discard
   :whitespacep
   :ignore-whitespace
   :char1
   :newlinep
   :seq
   :optional
   :manyn
   :*positive-int*
   :*non-negative-int*
   :parse-string
   :parse-stream
   :buffer-input))

(in-package :pears)

(defstruct stream-end)
(defparameter *stream-end* (make-stream-end))

(defstruct indexed-stream
  (start 0 :type (unsigned-byte 32))
  (end 0 :type (unsigned-byte 32))
  (buffer (vector) :type (vector))
  (stream nil :type t)
  (next nil :type t))

(defun read-stream-chunk (start stream buffer-size)
  (let ((buffer (make-array buffer-size :element-type 'character :adjustable nil)))
    (make-indexed-stream 
     :start start 
     :buffer buffer 
     :end (+ start (read-sequence buffer stream))
     :stream stream
     :next nil)))

(defun new-indexed-stream (stream buffer-size)
  (read-stream-chunk 0 stream buffer-size))

(defun indexed-stream-size (indexed-stream)
  (- (indexed-stream-end indexed-stream) (indexed-stream-start indexed-stream)))

(defun set-indexed-stream-next (indexed-stream next-str)
  (setf (indexed-stream-next indexed-stream) next-str))

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
                  (loop for stream = indexed-stream then (get-next-chunk stream )
                        for current-start = start then (indexed-stream-start stream)
                        collect (subseq (indexed-stream-buffer stream)
                                        (max 0 (- current-start (indexed-stream-start stream)))
                                        (min (length (indexed-stream-buffer stream))
                                             (- end (indexed-stream-start stream))))
                        until (< end (+ (indexed-stream-end stream) 1)))))))

(defun indexed-string-stream (str)
  (make-indexed-stream
   :start 0
   :buffer str
   :end (length str)
   :stream nil
   :next nil))

(defun get-next-chunk (indexed-stream)
  (let ((stream (indexed-stream-stream indexed-stream))) 
    (cond ((null stream) *stream-end*)
          ((indexed-stream-next indexed-stream) 
           (indexed-stream-next indexed-stream))
          (t (let ((next (read-stream-chunk
                          (indexed-stream-end indexed-stream) 
                          stream
                          (- 
                           (indexed-stream-end indexed-stream)
                           (indexed-stream-start indexed-stream)))))
               (setf (indexed-stream-next indexed-stream) next)
               next)))))

(defun get-entry (indexed-stream i)
  (declare ((unsigned-byte 32) i) (optimize (speed 3)))
  (etypecase indexed-stream
    (stream-end (values i *stream-end*))
    (t (cond ((null indexed-stream) (values nil *stream-end*))
             ((< i (indexed-stream-end indexed-stream))
              (values (aref (indexed-stream-buffer indexed-stream) 
                            (- i (indexed-stream-start indexed-stream)))
                      indexed-stream))
             ((< (indexed-stream-end indexed-stream)
                 (+ (indexed-stream-start indexed-stream)
                    (length (indexed-stream-buffer indexed-stream)))) (values nil *stream-end*))
             (t (get-entry (or (indexed-stream-next indexed-stream)
                               (get-next-chunk indexed-stream)) i))))))

(defun indexed-file-stream (file-stream &key (buffer-size 100000))
  (read-stream-chunk 0 file-stream buffer-size))

(defparameter *empty-chunk* 
  (make-indexed-stream
   :start 0
   :buffer (vector)
   :end 0
   :stream nil
   :next nil))

(defun take-while (pred stream j)
  (declare ((unsigned-byte 32) j) (function pred) (optimize (speed 3) (debug 0)))
  (etypecase stream
    (stream-end (values *stream-end* j))
    (t (loop with stream-start = (indexed-stream-start stream)
             with stream-end = (indexed-stream-end stream)
             with buffer = (indexed-stream-buffer stream)
             for i fixnum from (- j stream-start) to (- (min (length buffer)
                                                         (- stream-end stream-start)) 1)
             for el = (aref buffer i)
             while (funcall pred el)
             finally (return (cond ((= stream-start stream-end) 
                                    (values *stream-end* j))
                                   ((= j stream-end) 
                                    (take-while pred (get-next-chunk stream) j))
                                   ((not (funcall pred el))
                                    (values stream (+ i stream-start)))
                                   ((= i (length buffer))
                                    (take-while pred (get-next-chunk stream)
                                         (+ stream-start i)))
                                   (t (values stream (+ i stream-start)))))))))

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
  (let ((p-f (parser-f p)))
   (new-parser (lambda (strm i) 
                 (multiple-value-bind (result new-strm new-i) (funcall p-f strm i)
                   (etypecase result 
                     (failure (values result i))
                     (t (values (funcall (the function f) result) new-strm new-i))))))))

(defmethod flatmap (f (p parser))
  (let ((p-f (parser-f p)))
    (declare (function p-f))
    (new-parser (lambda (strm i)
                  (multiple-value-bind (result new-stream new-i) (funcall p-f strm i)
                    (etypecase result
                      (failure result)
                      (t (apply-parser (funcall (the function f) result) new-stream new-i))))))))

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
                          (funcall ,cur-parser ,cur-stream ,cur-i)
                        (etypecase ,next-result 
                          (failure (values ,next-result ,next-stream))
                          (t ,(if (string= (symbol-name result-binding) "_")
                                  (nest (cdr parser-bindings) next-stream next-i value-form)
                                  `(let ((,result-binding ,next-result))
                                     ,(nest (cdr parser-bindings) 
                                            next-stream next-i value-form))))))))))
      (let* ((parser-bindings (butlast body))
             (evaluated-parsers (loop for (b p) in parser-bindings collect (list (gensym) nil)))
             (bindings (loop for (b p) in parser-bindings for (sym nl) in evaluated-parsers
                          collect `(,b (or ,sym (setf ,sym (if (parser-p ,p) (parser-f ,p) ,p))))))
             (value-form (car (last body))))
        `(new-parser (let ,evaluated-parsers 
                       (lambda (,init-stream ,init-i)
                         ,(nest bindings init-stream init-i value-form))))))))

(defmacro orp (&rest parsers)
  (if (null parsers)
      (error "orp must be given at least one parser")
      (let* ((reversed-parsers (reverse parsers))
             (evaluated-parsers (loop for p in reversed-parsers collect `(,(gensym) nil)))
             (parsers (mapcar (lambda (sym p) `(or ,(car sym) 
                                                   (setf ,(car sym)
                                                         (if (parser-p ,p) (parser-f ,p) ,p)))) 
                              evaluated-parsers reversed-parsers))
             (init-stream (gensym))
             (init-i (gensym))
             (parse-attempts (reduce (lambda (acc p)
                                      (let ((result (gensym))
                                            (next-i (gensym))
                                            (next-stream (gensym)))
                                        `(multiple-value-bind (,result ,next-stream ,next-i) 
                                             (funcall ,p ,init-stream ,init-i)
                                           (etypecase ,result
                                             (failure ,acc) 
                                             (t (values ,result ,next-stream ,next-i))))))
                                     (cdr parsers)
                                     :initial-value 
                                     `(funcall ,(car parsers) ,init-stream ,init-i))))
        `(new-parser (let ,evaluated-parsers 
                       (lambda (,init-stream ,init-i) ,parse-attempts))))))

(defun one (pred) 
  "Match one stream element satisfying the supplied predicate"
  (new-parser (lambda (stream i)
                (declare (fixnum i))
                (multiple-value-bind (entry next-stream) (get-entry stream i)
                  (etypecase next-stream
                    (stream-end (values *failure* i))
                    (t (if (funcall (the function pred) entry)
                           (values entry next-stream (+ i 1))
                           (values *failure* i))))))))

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
                      (values *failure* i)
                      (values (stream-subseq stream i end) next-stream end))))))

(defun repeated (parser)
  (let ((p-f (parser-f parser)))
    (declare (function p-f))
    (new-parser (lambda (stream i)
                  (loop for end-prev = i then end
                        for stream-prev = stream then next-stream
                        for (result next-stream end) = (multiple-value-list
                                                        (funcall p-f stream i))
                        then (multiple-value-list (funcall p-f next-stream end))
                        while (etypecase result (failure nil) (t t))
                        collect result into results
                        finally (return (values results stream-prev end-prev)))))))

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
                                       (values *failure* i)
                                       (values results stream-prev end-prev)))))))

(defun sep-by (value-parser sep-parser)
  (let ((sep-parser (sequential (_ sep-parser)
                                (v value-parser)
                                v)))
    (fmap #'car (optional (sequential (fst value-parser)
                                      (rest (repeated sep-parser))
                                      (cons fst rest))))))

(defun discard (pred)
  (new-parser (lambda (stream i)
                (multiple-value-bind (next-stream next-i) (take-while pred stream i)
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
                     (stream-end (values *failure* i))
                     (t (if (funcall (the function test) (aref s cnt) entry)
                            (rec next-stream (+ cnt 1) (+ i 1))
                            (values *failure* i))))))))
    (new-parser (lambda (stream i) (rec stream 0 i)))))

(defun optional (parser)
  (let ((p-f (parser-f parser)))
    (declare (function p-f))
    (new-parser (lambda (stream i)
                  (multiple-value-bind (result next-stream next-i) (funcall p-f stream i)
                    (etypecase result
                      (failure (values nil stream i))
                      (t (values (list result) next-stream next-i))))))))

(defun manyn (predicate n)
  (new-parser (lambda (stream i)
                (declare (fixnum i n))
                (loop for cur-i = i then (+ cur-i 1)
                   for cnt = 0 then (+ cnt 1)
                   for (entry next-stream) = (multiple-value-list (get-entry stream cur-i))
                   while (and (not (eq next-stream *stream-end*))
                              (funcall (the function predicate) entry) (< cnt n))
                   finally (return (if (= cnt n)
                                       (values (stream-subseq stream i cur-i) next-stream cur-i)
                                       (values *failure* i)))))))

(defun non-zero-digit ()
  (one (lambda (c) (and (digit-char-p c) (char/= c #\0)))))

(defparameter *positive-int* (sequential (fst (non-zero-digit))
                                         (rest (many #'digit-char-p))
                                         (if (> (length rest) 0)
                                             (+ (* (expt 10 (length (the vector rest))) 
                                                   (digit-char-p fst))
                                                (digits-to-int rest))
                                             (digit-char-p fst))))

(defparameter *non-negative-int* (orp (sequential (_ (char1 #\0)) 0) *positive-int*))

(defun parse-string (parser string)
  (let ((indexed-stream (indexed-string-stream string)))
    (multiple-value-bind (result next-stream next-index)
        (apply-parser parser indexed-stream 0)
      (declare (ignore next-stream next-index))
      result)))

(defun parse-stream (parser stream)
  (let ((indexed-stream (new-indexed-stream stream 100000)))
    (multiple-value-bind (result next-stream next-index)
        (apply-parser parser indexed-stream 0)
      (declare (ignore next-stream next-index))
      result)))

(defun buffer-input (input)
  (etypecase input
    (string (indexed-string-stream input))
    (stream (new-indexed-stream input 100000))))
