(defpackage parser-tests
  (:use :cl :rove :pears))

(in-package :parser-tests)

(deftest read-stream-chunk-test
  (testing "read-stream-chunk creates a new indexed stream reading from the underlying stream into the indexed stream buffer"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (read-stream-chunk 0 s 4)))
        (ok (equalp (indexed-stream-buffer ind-str) "test")))))
  (testing "read-stream-chunk sets the start index from the provided value"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (read-stream-chunk 1 s 4)))
        (ok (equalp (indexed-stream-start ind-str) 1)))))
  (testing "read-stream-chunk reads until the end of the stream when the buffer is larger than the ramining input"
    (with-input-from-string (s "test")
      (let ((ind-str (read-stream-chunk 1 s 8)))
        (ok (equalp "test"
                    (subseq (indexed-stream-buffer ind-str)
                            0
                            (- (indexed-stream-end ind-str)
                               (indexed-stream-start ind-str)))))
        (ok (= (indexed-stream-start ind-str) 1))
        (ok (= (indexed-stream-end ind-str)) 5)))))

(deftest stream-subseq-test
  (testing "stream-subseq subseqs contiguous portion of the underlying buffer"
    (with-input-from-string (s "test-stream") 
      (let ((ind-str (new-indexed-stream s 5)))
        (ok (equalp (stream-subseq ind-str 0 3) "tes")))))
  (testing "stream-subseq subseqs starting at the provided offset in the indexed-stream"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (new-indexed-stream s 5)))
        (ok (equalp (stream-subseq ind-str 2 4) "st")))))
  (testing "stream-subseq reads parts of the subsequence from  sequence of indexed streams and appends them"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (new-indexed-stream s 3)))
        (ok (equalp (stream-subseq ind-str 0 5) "test-")))))
  (testing "stream-subseq concatenates complete chunks that span part of the required subsequence"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (new-indexed-stream s 2)))
        (ok (equalp (stream-subseq ind-str 1 5) "est-"))))))

(deftest get-entry-test
  (testing "get-entry gets a stream entry in the first stream buffer"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (new-indexed-stream s 2)))
        (ok (equalp (pears:get-entry ind-str 1) #\e)))))
  (testing "get-entry gets a stream entry beyond the first buffer"
    (with-input-from-string (s "test-stream")
      (let ((ind-str (new-indexed-stream s 2)))
        (ok (equalp (pears:get-entry ind-str 4) #\-))))))

(deftest take-while-test
  (testing "take-while returns the one more than the last index of the stream where the predicate applies"
    (with-input-from-string (s "abcdefghij")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (str i) (pears:take-while (lambda (c) (char< c #\d)) ind-str 0)
          (ok (equalp (indexed-stream-start str) 2))
          (ok (equalp (indexed-stream-end str) 4))
          (ok (equalp i 3))))))
  (testing "take-while returns stream-end and the length of the stream if it reaches the end of input"
    (with-input-from-string (s "abcd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (str i) (pears:take-while (lambda (c) (char< c #\e)) ind-str 0)
          (ok (equalp str pears:*stream-end*))
          (ok (equalp i 4)))))))

(deftest one-test
  (testing "one matches only one stream element satisfying a predicate"
    (with-input-from-string (s "cc")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (e next-s next-i)
            (pears:apply-parser (pears:one (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp e #\c))
          (ok (equalp next-i 1))
          (ok (equalp (indexed-stream-start next-s) 0))
          (ok (equalp (indexed-stream-end next-s) 2))))))
  (testing "one fails if the next stream element doesn't satisfy its predicate"
    (with-input-from-string (s "dd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (f i)
            (pears:apply-parser (pears:one (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp f pears:*failure*))
          (ok (equalp i 0)))))))

(deftest apply-parser-test
  (testing "apply-parser parses successfully when the parser matches"
    (with-input-from-string (s "c")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (e next-s next-i)
            (pears:apply-parser (pears:one (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp e #\c))
          (ok (equalp next-s ind-str))
          (ok (equalp next-i 1))))))
  (testing "apply parser parses using the provided parser at the specified index"
    (with-input-from-string (s "dc")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (e next-s next-i)
            (pears:apply-parser (pears:one (lambda (c) (char= c #\c))) ind-str 1)
          (ok (equalp e #\c))
          (ok (equalp next-s ind-str))
          (ok (equalp next-i 2)))))))

(deftest many-test
  (testing "many returns an empty sequence when zero stream elements satisfy a predicate"
    (with-input-from-string (s "dd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:many (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp result ""))
          (ok (equalp next-s ind-str))
          (ok (equalp next-i 0))))))
  (testing "many returns a sequence of elements satisfying a predicate"
    (with-input-from-string (s "ccccd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:many (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp result "cccc"))
          (ok (equalp (indexed-stream-start next-s) 4))
          (ok (equalp (indexed-stream-end next-s) 5))
          (ok (equalp next-i 4)))))))

(deftest many1-test
  (testing "many1 returns failure when zero stream elements satisfy a predicate"
    (with-input-from-string (s "dd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (f i)
            (pears:apply-parser (pears:many1 (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp f pears:*failure*))
          (ok (equalp i 0))))))
  (testing "many1 returns a sequence of elements satisfying its predicate"
    (with-input-from-string (s "ccccd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:many1 (lambda (c) (char= c #\c))) ind-str 0)
          (ok (equalp result "cccc"))
          (ok (equalp (indexed-stream-start next-s) 4))
          (ok (equalp (indexed-stream-end next-s) 5))
          (ok (equalp next-i 4)))))))

(deftest repeated-test
  (testing "repeated returns a list of all elements matching a parser"
    (with-input-from-string (s "ccccd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:repeated (pears:one (lambda (c) (char= c #\c)))) ind-str 0)
          (ok (equalp result (list #\c #\c #\c #\c)))
          (ok (equalp (indexed-stream-start next-s) 2))
          (ok (equalp (indexed-stream-end next-s) 4))
          (ok (equalp next-i 4))))))
  (testing "repeated returns an empty list when the supplied parser doesn't match"
    (with-input-from-string (s "dd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:repeated (pears:one (lambda (c) (char= c #\c)))) ind-str 0)
          (ok (equalp result nil))
          (ok (equalp next-s ind-str))
          (ok (equalp next-i 0)))))))

(deftest repeated1-test
  (testing "repeated1 returns a list of all elements matching a parser"
    (with-input-from-string (s "ccccd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:repeated1 (pears:one (lambda (c) (char= c #\c)))) ind-str 0)
          (ok (equalp result (list #\c #\c #\c #\c)))
          (ok (equalp (indexed-stream-start next-s) 2))
          (ok (equalp (indexed-stream-end next-s) 4))
          (ok (equalp next-i 4))))))
  (testing "repeated1 fails when the supplied parser doesn't match any input"
    (with-input-from-string (s "dd")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (f next-i)
            (pears:apply-parser (pears:repeated1 (pears:one (lambda (c) (char= c #\c)))) ind-str 0)
          (ok (equalp f pears:*failure*))
          (ok (equalp next-i 0)))))))

(deftest sep-by-test 
  (testing "sep-by returns a list of elements separated by a separator"
    (with-input-from-string (s "aa,bb,cc")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:sep-by (pears:many1 #'alpha-char-p) 
                                              (pears:one (lambda (c) (char= c #\,))))
                                ind-str 0)
          (ok (equalp result (list "aa" "bb" "cc")))
          (ok (equalp next-i 8))))))
  (testing "sep-by fails when the value parser doesn't match any input"
    (with-input-from-string (s ",aa,bb,cc")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (f next-i)
            (pears:apply-parser (pears:sep-by (pears:many1 #'alpha-char-p)
                                              (pears:one (lambda (c) (char= c #\,))))
                                ind-str 0)
          (ok (equalp f pears:*failure*))
          (ok (equalp next-i 0))))))
  (testing "return matching value input when only the separator parser doesn't match"
    (with-input-from-string (s "aa bb cc")
      (let ((ind-str (new-indexed-stream s 2)))
        (multiple-value-bind (result next-s next-i)
            (pears:apply-parser (pears:sep-by (pears:many1 #'alpha-char-p)
                                              (pears:one (lambda (c) (char= c #\,))))
                                ind-str 0)
          (ok (equalp result (list "aa")))
          (ok (equalp next-i 2)))))))

