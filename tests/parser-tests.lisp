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

