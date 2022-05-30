;;; macports-describe.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-describe.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; macports-describe.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-describe.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: major mode for viewing details about a port

;;; Code:

(require 'macports-core)
(require 'thingatpt)

(defface macports-describe-heading
  '((t (:weight bold)))
  "Face used for headings in MacPorts Describe buffers."
  :group 'macports)

(defface macports-describe-loading
  '((t (:slant italic)))
  "Face used for loading text in MacPorts Describe buffers."
  :group 'macports)

(defun macports-describe-port (port)
  "Display detailed information about PORT."
  (interactive (list (macports-core--prompt-port)))
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (macports-dispatch-mode)
      (shell-command (concat macports-command " -q info " port) standard-output)
      (macports-describe--linkify-urls)
      (macports-describe--linkify-emails)
      (macports-describe--linkify-github)
      (macports-describe--style-headings)
      (goto-char (point-max))
      (macports-describe--heading "Dependents")
      (macports-describe--async-insert (concat macports-command " -q rdependents " port) "None\n")
      (macports-describe--heading "Deps")
      (macports-describe--async-insert (concat macports-command " -q rdeps " port) "None\n"))))

(defun macports-describe--heading (text)
  "Insert a heading with content TEXT."
  (insert (format "\n%s\n" (propertize text 'face 'macports-describe-heading))))

(defun macports-describe--async-insert (command empty-msg)
  "Insert output of COMMAND with temporary loading message.

If result is blank, show EMPTY-MSG instead."
  (let ((s-marker (point-marker))
        e-marker
        (buf (current-buffer)))
    (insert (propertize "Loading..." 'face 'macports-describe-loading))
    (setq e-marker (point-marker))
    (macports-core--async-shell-command-to-string
     command
     (lambda (result)
       (with-current-buffer buf
         (let* ((inhibit-read-only t)
                (cleaned (replace-regexp-in-string "\r" "" result))
                (to-insert (if (string-blank-p cleaned)
                               empty-msg
                             cleaned)))
           (delete-region (marker-position s-marker) (marker-position e-marker))
           (goto-char (marker-position s-marker))
           (insert to-insert)
           (set-marker s-marker nil)
           (set-marker e-marker nil)))))))

(defun macports-describe--linkify-urls ()
  "Linkify URLs in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "https?://[^ \n]+" nil t)
      (let ((url (thing-at-point-url-at-point))
            (bounds (bounds-of-thing-at-point 'url)))
        (delete-region (car bounds) (cdr bounds))
        (help-insert-xref-button url 'help-url url)))))

(defun macports-describe--linkify-emails ()
  "Linkify email addresses in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^[:blank:]]+@[^[:blank:]]" nil t)
      (let ((email (thing-at-point 'email))
            (bounds (bounds-of-thing-at-point 'email)))
        (delete-region (car bounds) (cdr bounds))
        (help-insert-xref-button email 'help-url (concat "mailto:" email))))))

(defun macports-describe--linkify-github ()
  "Linkify GitHub usernames in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "GitHub:[[:blank:]]*[^[:blank:]]" nil t)
      (let ((username (thing-at-point 'symbol))
            (bounds (bounds-of-thing-at-point 'symbol)))
        (delete-region (car bounds) (cdr bounds))
        (help-insert-xref-button username 'help-url (concat "https://github.com/" username))))))

(defun macports-describe--style-headings ()
  "Apply heading style to current buffer content."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^[:blank:]][^:\n]+:" nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(face macports-describe-heading)))))

(defun macports-describe-port-contents (port)
  "Display contents of PORT in a new buffer."
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (macports-dispatch-mode)
      (shell-command (concat macports-command " -q contents " port) standard-output))))

(provide 'macports-describe)
;;; macports-describe.el ends here
