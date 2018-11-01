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
  (let* ((response "-ERR Some error\r\n")
	 (parsed (eredis-parse-error-response response)))
    (should (equal parsed '("ERR Some error" . 17)))))

;;;; parse status responses

(ert-deftest parse-status-response()
  "Parsing status"
  (let* ((response "+OK\r\n")
	 (parsed (eredis-parse-status-response response)))
    (should (equal parsed '("OK" . 5)))))






