(defpackage :pears
  (:use :cl :monad))

(in-package :pears)

(defstruct lazy-list head tail)

(defmacro defer (a) `(lambda () ,a))

(defmacro lazy-cons (a tail)
  `(make-lazy-list :head ,a :tail (defer ,tail)))

(defun head (lazy-list) (lazy-list-head lazy-list))
(defun tail (lazy-list) 
  (let ((evaluated (if (eq (type-of (lazy-list-tail lazy-list)) 'lazy-list)
                       (lazy-list-tail lazy-list)
                       (funcall (lazy-list-tail lazy-list)))))
    (setf (lazy-list-tail lazy-list)
          (make-lazy-list :head (lazy-list-head evaluated) :tail (lazy-list-tail evaluated)))))

(defun lazy-stream (stream) (lazy-cons (read-char stream nil nil) (lazy-stream stream)))

(defun take-while (p lazy-stream)
  (labels ((rec (acc s)
             (if (funcall p (head s))
                 (rec (cons (head s) acc) (tail s))
                 (values (reverse acc) s))))
    (rec nil lazy-stream)))

(defun drop-while (p lazy-stream)
  (if (funcall p (head lazy-stream))
      (drop-while p (tail lazy-stream))
      lazy-stream))

(defstruct parser f)

(defstruct failure)

(defparameter *failure* (make-failure))

(defun new-parser (f)
  (make-parser :f f))

(defun apply-parser (p strm)
  (funcall (parser-f p) strm))

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
                (let ((c (head stream)))
                  (if (funcall pred c)
                      (values (head stream) (tail stream))
                      *failure*)))))

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

(defun sep-by (value-parser sep-parser)
  (let ((sep-parser (mdo (_ sep-parser)
                         (v value-parser)
                         (yield v))))
    (mdo (fst value-parser)
         (rest (repeated sep-parser))
         (yield (cons fst rest)))))

(defun digits-to-int (digits)
  (loop for d in digits
     for n = (digit-char-p d) then (+ (* n 10) (digit-char-p d))
     finally (return n)))

(defparameter *positive-int* (fmap #'digits-to-int (many1 #'digit-char-p)))

(defun row ()
  (mdo (vs (sep-by *positive-int* (one (lambda (c) (char= c #\,)))))
       (_ (one (lambda (c) (char= c #\newline))))
       (yield vs)))

(defun csv ()
  (repeated (row)))

