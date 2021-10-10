;; nrql-tests.el -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Mackenzie Bligb

;; Author: Mackenzie Bligh <mackenziebligh@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Tests for nrql
;;; Code:
(ert-deftest test-nrql-process-hash-table-value ()
  ;; The instant newrelic sends should be converted to a org-mode timestamp
  (should (string= "2021-10-03 Sun 20:45:52.383" (nrql-process-hash-table-value "timestamp" 1633319152383)))
  ;; newlines are replaced with spaces
  (should (string= "hello there" (nrql-process-hash-table-value "samplekey" "hello\nthere")))
  ;; pipe is replaced with space because it messes with table styled output
  (should (string= "hello there" (nrql-process-hash-table-value "samplekey" "hello|there")))
  ;; Should not process :null from upstream
  (should (eq :null (nrql-process-hash-table-value "hello" :null)))
  (should (eq 123 (nrql-process-hash-table-value "some-key" 123)))
  (should (equal '(123) (nrql-process-hash-table-value "listp-key" '(123))))
  (should (equal 'false (nrql-process-hash-table-value "bool-key" 'false)))
  (should (equal nil (nrql-process-hash-table-value "bool-key" '()))))

;; nrql-tests.el ends here
