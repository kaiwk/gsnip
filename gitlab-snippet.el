;;; gitlab-snippet.el --- A gitlab snippet client.          -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2020  Wang Kai

;; Author: Wang Kai <kaiwkx@gmail.com>
;; Keywords: extensions, tools
;; URL: https://github.com/kaiwk/gitlab-snippet
;; Package-Requires: ((emacs "26") (dash "2.16.0") (aio "1.0") (log4e "0.3.3"))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; gitlab-snippet.el is an unofficial Gitlab Snippet client.
;;
;;; Code:

(require 'cl)
(require 'seq)

(require 'aio)
(require 'dash)
(require 'log4e)
(log4e:deflogger "gsnippet" "%t [%l] %m" "%H:%M:%S" '((fatal . "fatal")
                                                      (error . "error")
                                                      (warn  . "warn")
                                                      (info  . "info")
                                                      (debug . "debug")
                                                      (trace . "trace")))
(setq log4e--log-buffer-gsnippet "*gsnippet-log*")

;;;###autoload
(defun gsnippet-toggle-debug ()
  "Toggle debug."
  (interactive)
  (if (gsnippet--log-debugging-p)
      (progn
        (gsnippet--log-set-level 'info)
        (gsnippet--log-disable-debugging)
        (message "gsnippet disable debug"))
    (progn
      (gsnippet--log-set-level 'debug)
      (gsnippet--log-enable-debugging)
      (message "gsnippet enable debug"))))


;;; Gitlab Snippet API

(defvar gsnippet-private-token nil)
(defvar gsnippet-url nil)
(defvar gsnippet--user-snippets nil)
(defvar gsnippet--public-snippets nil) ; currently public snippets API not working

(defmacro gsnippet--api-base (suffix-path)
  `(concat ,gsnippet-url "api/v4/snippets/" ,suffix-path))

(defconst gsnippet--api-user-snippets (gsnippet--api-base ""))
(defconst gsnippet--api-public-snippets (gsnippet--api-base "public?per_page=20&page=50"))
(defconst gsnippet--buffer-name "*gitlab-snippets*")

(aio-defun gsnippet--fetch-snippets (url)
  "Fetch user snippets."
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gsnippet-private-token)))
         (res (aio-await (aio-url-retrieve url))))
    (with-current-buffer (cdr res)
      (set-buffer-multibyte t)
      (json-read-from-string
       (decode-coding-string
        (string-trim (buffer-substring-no-properties
                      url-http-end-of-headers
                      (point-max)))
        'utf-8)))))

(aio-defun gsnippet--fetch-raw (snippet-id)
  "Fetch snippet raw content by SNIPPET-ID"
  (gsnippet--debug "fetch raw snippet-id: %s" snippet-id)
  (let* ((url-request-method "GET")
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gsnippet-private-token)))
         (snippet (seq-find (lambda (s)
                              (let-alist s (string= (number-to-string .id) snippet-id)))
                            gsnippet--user-snippets))
         (res (let-alist snippet
                (aio-await (aio-url-retrieve .raw_url)))))
    (with-current-buffer (cdr res)
      (set-buffer-multibyte t)
      (decode-coding-string
       (string-trim (buffer-substring-no-properties
                     url-http-end-of-headers
                     (point-max)))
       'utf-8))))

(aio-defun gsnippet--put-snippet (snippet-id snippet)
  "Update snippet by SNIPPET-ID with SNIPPET, a SNIPPET is an
  alist which will be `json-encode' like below:

{
    \"title\":\"a snippet for files backup\",
    \"file_name\":\"run_backup.py\",
    \"description\":\"blah blah blah\",
    \"content\":\"some python code here\",
    \"visibility\":\"private|internal|public\"
}"
  (gsnippet--debug "put raw snippet-id: %s" snippet-id)
  (message "Updating %s..." snippet-id)
  (let* ((url-request-method "PUT")
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gsnippet-private-token)
            ("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string (json-encode snippet) 'utf-8))
         (res (aio-await (aio-url-retrieve (gsnippet--api-base snippet-id)))))
    (with-current-buffer (cdr res)
      (let ((status-code (url-http-parse-response)))
        (if (= status-code 200)
            (message "Update %s success!" snippet-id)
          (message "Update %s failed, status code: %s" snippet-id status-code))))))

(aio-defun gsnippet--post-snippet (snippet)
  "Post SNIPPET, a SNIPPET is an alist which will be `json-encode' like below:

{
    \"title\":\"a snippet for files backup\",
    \"file_name\":\"run_backup.py\",
    \"description\":\"blah blah blah\",
    \"content\":\"some python code here\",
    \"visibility\":\"private|internal|public\"
}"
  (gsnippet--debug "post snippet: %s" snippet)
  (message "Posting...")
  (let* ((fname (let-alist snippet .file_name))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gsnippet-private-token)
            ("Content-Type" . "application/json")))
         (url-request-data
          (encode-coding-string (json-encode snippet) 'utf-8))
         (res (aio-await (aio-url-retrieve (gsnippet--api-base "")))))
    (with-current-buffer (cdr res)
      (switch-to-buffer (current-buffer))
      (set-buffer-multibyte t)
      (let* ((status-code (url-http-parse-response))
             (svr-snippet (json-read-from-string
                           (decode-coding-string
                            (string-trim (buffer-substring-no-properties
                                          url-http-end-of-headers
                                          (point-max)))
                            'utf-8)))
             (snippet-id (let-alist svr-snippet .id)))
        (if (= status-code 201)
            (message "Post %s(id=%s) success!" fname snippet-id)
          (message "Post %s failed, status code: %s" fname status-code))))))

(aio-defun gsnippet--delete-snippet (snippet-id)
  "Delete snippet by SNIPPET-ID."
  (gsnippet--debug "delete snippet: %s" snippet-id)
  (message "Deleting...")
  (let* ((url-request-method "DELETE")
         (url-request-extra-headers
          `(("PRIVATE-TOKEN" . ,gsnippet-private-token)
            ("Content-Type" . "application/json")))
         (res (aio-await (aio-url-retrieve (gsnippet--api-base snippet-id)))))
    (with-current-buffer (cdr res)
      (let ((status-code (url-http-parse-response)))
        (if (= status-code 204)
            (message "Delete %s success!" snippet-id)
          (message "Delete %s failed, status code: %s" snippet-id status-code))))))


(aio-defun gsnippet--init ()
  (interactive)
  (setq gsnippet--user-snippets (aio-await (gsnippet--fetch-snippets gsnippet--api-user-snippets)))
  (setq gsnippet--public-snippets (aio-await (gsnippet--fetch-snippets gsnippet--api-public-snippets))))

(defun gsnippet--make-tabulated-headers (header-names rows)
  "Calculate headers width.
Column width calculated by picking the max width of every cell
under that column and the HEADER-NAMES. HEADER-NAMES are a list
of header name, ROWS are a list of vector, each vector is one
row."
  (let ((widths
         (seq-reduce
          (lambda (acc row)
            (-zip-with
             (lambda (a col) (+ (max a (length col)) 1))
             acc
             (append row '())))
          rows
          (seq-map #'length header-names))))
    (vconcat
     (-zip-with
      (lambda (col size) (list col size nil))
      header-names widths))))

(defun gsnippet--make-tabulated-rows ()
  "Generate tabulated list rows from `gsnippet--user-snippets'.
Return a list of rows, each row is a vector:
\([<Id> <Created> <Visibility> <Filename> <Title> <Description>] ...)"
  (cl-loop for snippet across gsnippet--user-snippets
           collect
           (let-alist snippet
             (vector (number-to-string .id)
                     (format-time-string
                      "%m/%d/%Y %H:%M"
                      (parse-iso8601-time-string .created_at))
                     .visibility .file_name .title (or .description "")))))

(aio-defun gsnippet-refresh ()
  (interactive)
  (aio-await (gsnippet--init))
  (let* ((header-names `("Id" "Created" "Visibility" "Filename" "Title" "Description"))
         (rows (gsnippet--make-tabulated-rows))
         (headers (gsnippet--make-tabulated-headers header-names rows)))
    (with-current-buffer (get-buffer-create gsnippet--buffer-name)
      (gsnippet--snippets-mode)
      (setq tabulated-list-format headers)
      (setq tabulated-list-entries
            (-zip-with
             (lambda (i x) (list i x))
             (-iterate '1+ 0 (length rows))
             rows))
      (tabulated-list-init-header)
      (tabulated-list-print t))))

;;;###autoload(autoload 'gsnippet-list "gitlab-snippet" "" t nil)
(aio-defun gsnippet-list ()
  (interactive)
  (aio-await (gsnippet-refresh))
  (switch-to-buffer gsnippet--buffer-name))

(aio-defun gsnippet--show-current-snippet ()
  "Show current entry snippet. Get current entry by using `tabulated-list-get-entry'."
  (interactive)
  (let* ((entry (tabulated-list-get-entry))
         (snippet-id (aref entry 0))
         (buf-name (format "*gsnippet:%s*/%s" snippet-id (gsnippet--snippet-filename snippet-id)))
         (raw (aio-await (gsnippet--fetch-raw snippet-id))))
    (with-current-buffer (get-buffer-create buf-name)
      (erase-buffer)
      (insert raw)
      (gsnippet--debug "insert raw: \n%s" raw)
      (set-buffer-modified-p nil)
      (let ((fmode (assoc-default (file-name-extension buf-name t) auto-mode-alist #'string-match-p)))
        (if fmode (funcall fmode)))
      (gsnippet-mode t)
      (setq gsnippet-id snippet-id))
    (switch-to-buffer-other-window buf-name)))

(defun gsnippet--snippet-filename (snippet-id)
  "Get snippet filename with SNIPPET-ID."
  (let* ((snippet (seq-find (lambda (s) (let-alist s (string= (number-to-string .id) snippet-id)))
                            gsnippet--user-snippets))
         (filename (let-alist snippet .file_name)))
    filename))


;;; gsnippet--snippets-mode

(aio-defun gsnippet-yank ()
  (interactive)
  (let* ((snippet-id (aref (tabulated-list-get-entry) 0))
         (raw (aio-await (gsnippet--fetch-raw snippet-id))))
    (kill-new raw)
    (message "Snippet saved!")))

(aio-defun gsnippet-edit-meta ()
  (interactive)
  (let* ((snippet-id (aref (tabulated-list-get-entry) 0))
         (title (aref (tabulated-list-get-entry) 4))
         (description (aref (tabulated-list-get-entry) 5))
         (new-title (read-from-minibuffer "Title: " title))
         (new-description (read-from-minibuffer "Description: " description)))
    (aio-await (gsnippet--put-snippet snippet-id `((title . ,new-title) (description . ,new-description))))
    (aio-await (gsnippet-refresh))
    (message "Save title and description success!")))

(aio-defun gsnippet-yank-link ()
  (interactive)
  (let ((snippet-id (aref (tabulated-list-get-entry) 0)))
    (kill-new (let-alist (seq-find
                          (lambda (s) (let-alist s (string= (number-to-string .id) snippet-id)))
                          gsnippet--user-snippets)
                .web_url))
    (message "Snippet link saved!")))

(aio-defun gsnippet-region (begin end &optional visibility)
  (interactive "r")
  (let* ((file (or (buffer-file-name) (buffer-name)))
         (fname (file-name-nondirectory file)))
    (aio-await (gsnippet--post-snippet
                `((title . ,fname)
                  (file_name . ,fname)
                  (content . ,(buffer-substring-no-properties begin end))
                  (visibility . "private"))))
    (aio-await (gsnippet-refresh))))

(aio-defun gsnippet-region-private (begin end)
  (interactive "r")
  (aio-await (gsnippet-region begin end "private")))

(aio-defun gsnippet-region-public (begin end)
  (interactive "r")
  (aio-await (gsnippet-region begin end "public")))

(aio-defun gsnippet-delete ()
  (interactive)
  (let* ((snippet-id (aref (tabulated-list-get-entry) 0))
         (confirm-delete (yes-or-no-p (format "Delete snippet %s?" snippet-id))))
    (when confirm-delete
      (aio-await (gsnippet--delete-snippet snippet-id))
      (aio-await (gsnippet-refresh)))))

(defvar gsnippet--snippets-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map (kbd "RET") #'gsnippet--show-current-snippet)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "q" #'quit-window)
      (define-key map "G" #'gsnippet-refresh)
      (define-key map "Y" #'gsnippet-yank)
      (define-key map "y" #'gsnippet-yank-link)
      (define-key map "d" #'gsnippet-delete)
      (define-key map "e" #'gsnippet-edit-meta)))
  "Keymap for `gsnippet--snippets-mode'.")

(define-derived-mode gsnippet--snippets-mode
  tabulated-list-mode "Gitlab Snippet"
  "Major mode for browsing a list of problems."
  (setq-local tabulated-list-padding 1)
  :group 'gsnippet)
(add-hook 'gsnippet--snippets-mode-hook #'hl-line-mode)


;;; gsnippet-mode

(defvar gsnippet-id nil
  "A buffer-local value to identify current buffer snippet-id.")
(make-variable-buffer-local 'gsnippet-id)

(defvar gsnippet-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (define-key map [remap save-buffer] 'gsnippet-mode-save-buffer)
      (define-key map [remap write-file] 'gsnippet-mode-write-file)))
  "Keymap for `gsnippet-mode'.")

(aio-defun gsnippet-mode-save-buffer ()
  (interactive)
  (let ((content (with-current-buffer (buffer-name)
                   (save-restriction
                     (widen)
                     (buffer-substring-no-properties (point-min) (point-max))))))
    (aio-await (gsnippet--put-snippet gsnippet-id `((content . ,content)))))
  (aio-await (gsnippet-refresh)))

(aio-defun gsnippet-mode-write-file ()
  (interactive)
  (let ((new-name (read-from-minibuffer "File name: " (gsnippet--snippet-filename gsnippet-id))))
    (aio-await (gsnippet--put-snippet gsnippet-id `((file_name . ,new-name)))))
  (aio-await (gsnippet-refresh)))

(define-minor-mode gsnippet-mode
  "Minor mode for buffers containing gitlab snippets"
  :lighter " gsnippet"
  :map 'gsnippet-mode-map)

(provide 'gitlab-snippet)
