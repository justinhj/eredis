;;; eredis-tests.el --- eredis, a test suite for eredis
;; Copyright 2012-2018 Justin Heyes-Jones
 
;; Author: Justin Heyes-Jones
;; URL: http://github.com/justinhj/eredis/
;; Version: 1.0.00

(require 'eredis)

;;;; Parsing single bulk reponses

(ert-deftest parse-single-bulk ()
  "Parse a normal single bulk"
  (let* ((response "$5\r\nhello\r\n")
	 (parsed (eredis-parse-bulk-response response)))
    (should (equal parsed '("hello" . 11)))))

(ert-deftest parse-single-bulk-mb ()
  "Parse a single bulk with multibyte characters"
  (let* ((response "$6\r\n⣇⣀\r\n")
	 (parsed (eredis-parse-bulk-response response)))
    (should (equal parsed '("⣇⣀" . 8)))))

(ert-deftest parse-not-set-single-bulk()
  "Parsing single bulk that is not set"
  (let* ((response "$-1\r\n")
	 (parsed (eredis-parse-bulk-response response)))
    (should (equal parsed '(nil . 5)))))

(ert-deftest parse-empty-single-bulk()
  "Parsing empty single bulk"
  (let* ((response "$0\r\n\r\n")
	 (parsed (eredis-parse-bulk-response response)))
    (should (equal parsed '("" . 6)))))

;;;; Parse error repsonses

(ert-deftest parse-error-response()
  "Parsing error"
  (let* ((response "-ERR Some error\r\nextrastuff")
	 (parsed (eredis-parse-error-response response)))
    (should (equal parsed '("ERR Some error" . 17)))))

;;;; parse status responses

(ert-deftest parse-status-response()
  "Parsing status"
  (let* ((response "+OK\r\nextrastuff")
	 (parsed (eredis-parse-status-response response)))
    (should (equal parsed '("OK" . 5)))))

;;;; parse integer

(ert-deftest parse-int-response()
  (should
   (equal (eredis-parse-integer-response ":1\r\nextrastuff")
	  '(1 . 4))))

;;;; multi bulk (array) responses

(ert-deftest parse-zero-length-array-response()
  (should (equal
	   (eredis-parse-array-response "*0\r\n")
	   '(nil . 4))))

(ert-deftest parse-null-array-response()
  (should (equal
	   (eredis-parse-array-response "*-1\r\n")
	   '(nil . 5))))
	   
;;nested array

(ert-deftest parse-array-response()
  (should (equal
	   (eredis-parse-array-response "*2\r\n$5\r\nwinky\r\n$8\r\ngarfield\r\n")
	   '(("garfield" "winky") . 29))))
		 

;; empty list

;; "*0"

;; missing list
;; "$-1"

;; nested array

(eredis-parse-array-response "*2\r\n*3\r\n:1\r\n:2\r\n:3\r\n*2\r\n+Foo\r\n-Bar\r\n")

(trace-function-background #'substring "*trace-output*")

(substring "ass" 0 1)

(eredis-parse-response ":1\r\n")










