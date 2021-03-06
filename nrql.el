;;; nrql.el --- An Elisp library for running NRQL queries -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Mackenzie Bligh <mackenziebligh@gmail.com>

;; Keywords: tools
;; URL: https://github.com/mack1070101/nrql.el
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (request))

;; nrql is free software; you can redistribute it and/or modify it
;; under the terms of the gnu general public license as published by
;; the free software foundation; either version 3, or (at your option)
;; any later version.
;;
;; nrql is distributed in the hope that it will be useful, but without
;; any warranty; without even the implied warranty of merchantability
;; or fitness for a particular purpose. See the gnu general public
;; license for more details.
;;
;; You should have received a copy of the gnu general public license
;; along with nrql.  if not, see http://www.gnu.org/licenses.

;;; Commentary:

;;; Code:
(require 'request)
(require 'json)
(require 'dash)

(defcustom nrql.el-dir
  (concat user-emacs-directory "nrql.el/")
  "File in which to save token."
  :group 'nrql
  :type 'string)

(defcustom nrql-api-keys-file
  (expand-file-name ".nrql-el-api-keys.el" nrql.el-dir)
  "File in which to save token."
  :group 'nrql
  :type 'string)

(defcustom nrql-timestamp-format-string
  "%Y-%m-%d %a %H:%M:%S.%3N"
  "Format string for displaying timestamps. Must comply with format-time-string"
  :group 'nrql
  :type 'string)

(defun nrql-replace-in-string (replace with in)
  "Utility function to replace a value in a string"
  (replace-regexp-in-string (regexp-quote replace) with in nil 'literal))

(defun nrql-get-api-keys (filename)
  "Get locally stored api key and account id. If it doesn't exist, store it"
  (if (file-exists-p filename)
      (with-temp-buffer
              (insert-file-contents filename)
              (read (current-buffer)))
      (progn
        (if (not (file-directory-p nrql.el-dir)) (make-directory nrql.el-dir))
        (with-temp-file
           filename
           (prin1 (let* ((account-id (read-number "Please enter your account number: "))
                         (api-key (read-string "Please enter your api key: ")))
                      (list account-id api-key))
                  (current-buffer))))))

;; TODO timeseries isn't supported
;; TODO embedded charts aren't supported
;; TODO test that all functions can be parsed proeprly
(defun nrql-make-query-and-parse (query)
  "Run a NRQL query on your new-relic account, and parse the successful result"
  (let* ((api-keys (nrql-get-api-keys nrql-api-keys-file))
         (cleaned-query (nrql-replace-in-string "\n" " " query))
         (response (request "https://api.newrelic.com/graphql"
                    :type "POST"
                    :sync t
                    :headers (list '("Content-Type" . "application/json")
                                   (cons "API-Key" (nth 1 api-keys)))
                    :data (format "{\"query\":  \"{actor {account(id: %d) {nrql(query: \\\"%s\\\") {results}}}}\"}"
                                  (car api-keys)
                                  cleaned-query)))
         (json-data (json-parse-string (request-response-data response))))

    (if (gethash "errors" json-data)
        ;; We have to map over errors because it's an array
        (gethash "message" (car (-map (lambda (x) x) (gethash "errors" json-data))))
        (->> json-data
             (gethash "data")
             (gethash "actor")
             (gethash "account")
             (gethash "nrql")
             (gethash "results")))))

(defun nrql-process-hash-table-value (key value)
  "Converts the value from a hash-table created from a NewRelic API response
into something that can be easily read in a result. For example:
- Converts epoch millis into a human readable timestamp if nrql-timestamp-format-string is
  properly configured
- Removes newlines and pipe characters from output as they tend to mess up org-mode tables
- Pretty format nested hash-tables to be easier to read, and so they don't break the table
  structure
"
  (cond ((eq :null value) value)
        ;; TODO this isn't parsing "seconds" fields (e.g. beginTimeSeconds) like from metrics
        ((and (string-match "times" key)
              (> value 0)
              (= (ceiling (log10 value)) 13) (format-time-string nrql-timestamp-format-string
                                                       (time-convert (cons value 1000)))))
        ((number-or-marker-p value) value)
        ((listp value) value)
        ;; Pretty format a hash table to improve how things look in result tables
        ((hash-table-p value) (nrql-pp-hash value))
        ((string= (type-of value) "string") (->> value
                                              (nrql-replace-in-string "\n" " ")
                                              ;; Pipe characters tend to mess up org-mode tables
                                              (nrql-replace-in-string "\|" " ")))
        (value (if value value 'false))
        (t value)))

;; TODO Can more than one query per block be supported?
(defun org-babel-execute:nrql (body params)
  "Execute a block of nrql code with org-babel. Currently can only handle one query per block."
  (let* ((string body)
         ;; Look at the end of the word "select"
         (select-position (+ (string-match "select" string) 7))
         (from-position  (string-match "from" string))
         (where-position (string-match "where" string))
         (since-position (string-match "since" string))
         (limit-position (string-match "limit" string))

         ;; Handle both "Select * From Blah" and "From Blah Select *" syntaxes
         (select-end-position (if (< from-position select-position)
                                  ;; Take the position of the keyword that is after SELECT
                                  (seq-min (-filter (lambda (position)
                                                      (and (not (eq nil position))
                                                           (> position select-position)))
                                                    (-non-nil (list from-position
                                                                    where-position
                                                                    since-position
                                                                    limit-position
                                                                    (length string)))))
                                  from-position))

         (query-variables (-map (lambda (str)
                                  (string-trim str))
                                (split-string (substring string select-position
                                                         select-end-position)
                                              ",")))
         (nrql-result (nrql-make-query-and-parse body)))

    ;; Return a table for org-mode or a string if there are an error
    (if (string= "string" (type-of nrql-result))
        nrql-result
      (let* ((hash-table-variables (delq nil (delete-dups
                                              (apply #'append
                                                     (-map (lambda (x) (hash-table-keys x))
                                                           nrql-result)))))
             ;; Take the ordering of variables from query-variables, but the names/values from hash-table-variables
             ;;  if they exist. This gives the most correct feeling ordering for result tables by using the value
             ;;  from NewRelic but the ordering provided by the user. It makes queries with functions work better
             (variables-to-process (append (-filter (lambda (x) (member x hash-table-variables)) query-variables)
                                           (-filter (lambda (x) (not (member x query-variables))) hash-table-variables))))
        (append (list variables-to-process 'hline)
                (-map (lambda (hashtable)
                        (-map (lambda (var)
                                (nrql-process-hash-table-value var
                                                               (gethash var hashtable)))
                              variables-to-process))
                      nrql-result))))))

;; Major mode and font faces
;; TODO improve function name faces: highlighted in substrings are highligted
;; https://docs.newrelic.com/docs/query-your-data/nrql-new-relic-query-language/get-started/nrql-syntax-clauses-functions/
(setq nrql-highlights
      (list
       (cons (string-join '("select"
                            "from"
                            "as"
                            "compare with"
                            "exrapolate"
                            "facet"
                            "limit"
                            "offset"
                            "order by"
                            "show event types"
                            "since"
                            "slide by"
                            "timeseries"
                            "until"
                            "where"
                            "with metric format"
                            "with timezone"
                            "and"
                            "or"
                            "is"
                            "not"
                            "like"
                            "ago"
                            "with")
                          "\\|")
             font-lock-keyword-face)
       (cons (string-join '(;; Simple numerical functions
                            "average"
                            "max"
                            ;; Aggregator functions
                            "apdex"
                            "aggregationendtime"
                            "buckets"
                            "bucketPercentile"
                            "cardinality"
                            "count"
                            "derivative"
                            "dimensions"
                            "latestrate"
                            "max"
                            "median"
                            "min"
                            "minuteOf"
                            "mod"
                            "percentage"
                            "percentile"
                            "predictLinear"
                            "rate"
                            "round"
                            "stddev"
                            "stdvar"
                            "sum"
                            "uniquesCount"
                            "uniques"
                            "capture"
                            ;; Non Aggregator Functions)
                            "earliest"
                            "eventType"
                            "filter"
                            "funnel"
                            "getField"
                            "histogram"
                            "keyset"
                            "latest"
                            ;; Type conversion
                            "numeric"
                            "boolean")
                          "\\|")
             font-lock-function-name-face)
       (cons "second\\|seconds\\|minutes\\|minute\\|hours\\|hour\\|days\\|day\\|max\\|null"
             font-lock-constant-face)
       ;; TODO support "`" better
       (cons "\'\\(\\(?:[^\'\\]+\\|\\\\\\(?:.\\|\\)\\)*\\)\'"
             font-lock-string-face)
       (cons "[0-9]+\\([eE][+-]?[0-9]*\\)?"
             font-lock-variable-name-face)))

(define-derived-mode nrql-mode prog-mode "nrql"
  "major mode for editing nrql queries."
  (set (make-local-variable 'font-lock-defaults) '(nrql-highlights nil t)))

(defun nrql-pp-hash (table)
  "Pretty format hash tables to make them easier to read."
  (let ((data (nthcdr 2 (nbutlast
                         (split-string (pp-to-string table) "[()]")
                         2))))
    (princ (concat "(" (car data) ")"))))

(defun nrql-eval-region (start end)
  "Evaluate the region between start and end."
  (interactive "r")
  (pp-hash (nrql-make-query-and-parse (apply #'buffer-substring-no-properties (list start end)))))

(provide 'nrql)
;;; nrql.el ends here
