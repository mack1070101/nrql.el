;; nrql-tests.el -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Mackenzie Bligh

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
  ;; Things that are timestamps should be parsed
  (should (string= "2021-10-03 Sun 20:45:52.383" (nrql-process-hash-table-value "additionaltime" 1633319152383)))
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

(ert-deftest test-org-babel-execute:nrql_single-parameter-query ()
  ;; Test that a single param query can be parsed for org-mode correctly
  (cl-letf (((symbol-function 'make-nrql-query-and-parse)
             (lambda (query)
               [#s(hash-table size 2 test equal rehash-size 1.5 rehash-threshold 0.8125
                              data ("message" "Logging is working"))])))
    (should (equal '(("message")
                     hline
                     ("Logging is working"))
                   (org-babel-execute:nrql "select message from Log" nil)))))

(ert-deftest test-org-babel-execute:nrql_multi-parameter-query ()
  (cl-letf (((symbol-function 'make-nrql-query-and-parse)
             (lambda (query)
                [#s(hash-table size 2
                               test equal
                               rehash-size 1.5
                               rehash-threshold 0.8125
                               data ("message" "here is a log"
                                     "timestamp" 1633352126371))
                   #s(hash-table size 2
                                 test equal
                                 rehash-size 1.5
                                 rehash-threshold
                                 0.8125
                                 data ("message" "here is another log"
                                       "timestamp" 1633352126371))])))

    (should (equal '(("timestamp" "message")
                     hline
                     ("2021-10-04 Mon 05:55:26.371" "here is a log")
                     ("2021-10-04 Mon 05:55:26.371" "here is another log"))
                   (org-babel-execute:nrql "select timestamp, message from Log" nil))))
  (cl-letf (((symbol-function 'make-nrql-query-and-parse)
             (lambda (query)
                [#s(hash-table size 2
                               test equal
                               rehash-size 1.5
                               rehash-threshold 0.8125
                               data ("message" "here is a log"
                                     "timestamp" 1633352126371))
                   #s(hash-table size 2
                                 test equal
                                 rehash-size 1.5
                                 rehash-threshold
                                 0.8125
                                 data ("message" "here is another log"
                                       "timestamp" 1633352126371))])))

    (should (equal '(("timestamp" "message")
                     hline
                     ("2021-10-04 Mon 05:55:26.371" "here is a log")
                     ("2021-10-04 Mon 05:55:26.371" "here is another log"))
                   (org-babel-execute:nrql "from Log select timestamp, message" nil)))))

(ert-deftest test-org-babel-execute:nrql_star-parameter-query ()
  (cl-letf (((symbol-function 'make-nrql-query-and-parse)
             (lambda (query)
                [#s(hash-table size 80
                               test equal
                               rehash-size 1.5
                               rehash-threshold 0.8125
                               data ("master_url" "https://100.64.0.1:443/api"
                                     "labels.app" "taco_shop"
                                     "message" "complete: GET /api/reservation/imagesV2 200"
                                     "timestamp" 1633352684278
                                     "newrelic.source" "api.logs"
                                     "user_agent_client" "turo_ios"
                                     "container_name" "turo-taco"
                                     "actor_email" "mackenziebligh@gmail.com"
                                     "entity.name" "turo-com-taco-api"))])))
    (should (equal '(("master_url"
                      "labels.app"
                      "message"
                      "timestamp"
                      "git_hash"
                      "newrelic.source"
                      "user_agent_client"
                      "container_name"
                      "actor_email"
                      "entity.name")
                     hline
                     ("https://100.64.0.1:443/api"
                      "taco"
                      "complete: GET /api/reservation/imagesV2 200"
                      "2021-10-04 Mon 06:04:44.278"
                      "c4cded5df2a967462003e8f32ddd595a1fefbd6b"
                      "api.logs"
                      "turo_ios"
                      "turo-taco"
                      "mackenziebligh@gmail.com"
                      "turo-com-taco-api"))
                   (org-babel-execute:nrql "select * from Log" nil)))))

(ert-deftest test-org-babel-execute:nrql_transaction ()
  (cl-letf (((symbol-function 'make-nrql-query-and-parse)
             (lambda (query)
                [#s(hash-table size 2
                               test equal
                               rehash-size 1.5
                               rehash-threshold 0.8125
                               data ("timestamp" 1633443177549
                                     "user_agent" :null))
                   #s(hash-table size 2
                                 test equal
                                 rehash-size 1.5
                                 rehash-threshold 0.8125
                                 data ("timestamp" 1633443177488
                                       "user_agent" :null))
                   #s(hash-table size 2
                                 test equal
                                 rehash-size 1.5
                                 rehash-threshold 0.8125
                                 data ("timestamp" 1633443177359
                                       "user_agent" "Mozilla/5.0"))])))

    (should (equal '(("timestamp" "user_agent")
                     hline
                     ("2021-10-05 Tue 07:12:57.549" :null)
                     ("2021-10-05 Tue 07:12:57.488" :null)
                     ("2021-10-05 Tue 07:12:57.359" "Mozilla/5.0"))
                   (org-babel-execute:nrql "select * from Log" nil)))))
;; nrql-tests.el ends here
