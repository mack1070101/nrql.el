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

(defun make-nrql-query-and-parse (query)
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
  "Run a NRQL query on your new-relic account, and parse the successful result"
  (gethash "results"
    (gethash "nrql"
      (gethash "account"
        (gethash "actor"
          (gethash "data"
            (json-parse-string
              (request-response-data
                (request "https://api.newrelic.com/graphql"
                  :type "POST"
                  :sync t
                  :headers '(("Content-Type" . "application/json") ("API-Key" . ""))
                  :data (format "{\"query\":  \"{actor {account(id: 721478) {nrql(query: \\\"%s\\\") {results}}}}\" }" query))))))))))

(defun nrql-process-hash-table-value (key value)
  (if (string= key "timestamp")
      (format-time-string "[%Y-%m-%d %a %H:%M:%S]" (seconds-to-time (/ value 1000)))
    value))

(defun org-babel-execute:nrql (body params)
  "Execute a block of nrql code with org-babel."
  (let* ((string body)
         (select-position (+ 7 (string-match "select" string)))
         (select-end-position (- (seq-min (-non-nil
                                           (list (string-match "from" string)
                                                 (string-match "where" string)))) 1))
         (query-variables (-map (lambda (str) (string-trim str))
                                (split-string (substring string select-position
                                                         select-end-position)
                                              ",")))
         (nrql-result (make-nrql-query-and-parse body))
         (variables-to-process (if (string= "*" (car query-variables))
                                   (delq nil (delete-dups
                                              (apply #'append
                                                     (-map (lambda (x) (hash-table-keys x)) nrql-result))))
                                   query-variables)))

    ;; TODO Need to handle the "Add table headers case"
    (mapcar (lambda (hashtable)
             (-map (lambda (var)
                     (nrql-process-hash-table-value var (gethash var hashtable)))
                   variables-to-process))
           nrql-result)))

;; Major mode and font faces
;; TODO work on face highlights more, make them also work with uppercase
(setq nrql-highlights
      '(("select\\|from\\|where\\|since\\|ago\\|limit" . font-lock-function-name-face)
        ("second\\|minute\\|hour\\|day\\|days\\|max". font-lock-constant-face)
        ("[0-9]+\\([eE][+-]?[0-9]*\\)?" . font-lock-variable-name-face)))

(define-derived-mode nrql-mode fundamental-mode "nrql"
  "major mode for editing nrql queries."
  (setq font-lock-defaults '(nrql-highlights)))
;;; nrql.el ends here
