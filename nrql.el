;;; nrql.el --- An Elisp library for running NRQL queries -*- lexical-binding: t; coding: utf-8 -*-

;; Author: Mackenzie Bligh <mackenziebligh@gmail.com>

;; Keywords: tools
;; Homepage:
;; Version: 0.0.1

;; nrql requires at least GNU Emacs 25.1

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
;; TODO what happens with more than one query per body
;; TODO not all functions don't work
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
        ;; TODO this hashmap behaviour is weird but works
        (gethash "message" (car (-map (lambda (x) x) (gethash "errors" json-data))))
        (->> json-data
             (gethash "data")
             (gethash "actor")
             (gethash "account")
             (gethash "nrql")
             (gethash "results")))))

(defun nrql-process-hash-table-value (key value)
  (cond ((eq :null value) value)
        ((string= key "timestamp") (format-time-string nrql-timestamp-format-string
                                                       (time-convert (cons value 1000))))
        ((number-or-marker-p value) value)
        ((listp value) value)
        ((string= (type-of value) "string") (->> value
                                              (nrql-replace-in-string "\n" " ")
                                              ;; Pipe characters tend to mess up org-mode tables
                                              (nrql-replace-in-string "\|" " ")))
        (value (if value value 'false))
        t value))

(defun org-babel-execute:nrql (body params)
  "Execute a block of nrql code with org-babel."
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
         (nrql-result (nrql-make-query-and-parse body))
         ;; TODO reorder the delq portion to make functions work
         (variables-to-process (if (string= "*" (car query-variables))
                                   (delq nil (delete-dups
                                              (apply #'append
                                                     (-map (lambda (x) (hash-table-keys x)) nrql-result))))
                                   query-variables)))

    ;; Return a table for org-mode or a string if there are an error
    (if (string= "string" (type-of nrql-result))
        nrql-result
        (append (list variables-to-process 'hline)
                (mapcar (lambda (hashtable)
                          (-map (lambda (var)
                                  (nrql-process-hash-table-value var
                                                                 (gethash var hashtable)))
                                variables-to-process))
                        nrql-result)))))

;; Major mode and font faces
;; TODO need to add additional function names
(setq nrql-highlights
      '(("select\\|from\\|where\\|and\\|or\\|not\\|like\\|since\\|ago\\|limit\\|facet\\|timeseries\\|with\\|timezone\\|count" . font-lock-function-name-face)
        ("second\\|seconds\\|minutes\\|minute\\|hours\\|hour\\|days\\|day\\|max". font-lock-constant-face)
        ("\'\\(\\(?:[^\'\\]+\\|\\\\\\(?:.\\|\\)\\)*\\)\'" . font-lock-string-face)
        ("[0-9]+\\([eE][+-]?[0-9]*\\)?" . font-lock-variable-name-face)))

(define-derived-mode nrql-mode prog-mode "nrql"
  "major mode for editing nrql queries."
  (set (make-local-variable 'font-lock-defaults) '(nrql-highlights nil t)))
;;; nrql.el ends here
