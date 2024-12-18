;;; macports-describe.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
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

(defface macports-describe-port-button-hover
  '((t (:underline t)))
  "Face used when mouse-hovering over link buttons to other ports."
  :group 'macports)

(defface macports-describe-build-test-only-rdeps
  '((t (:foreground "grey60")))
  "Face used for build- or test-only rdeps text in MacPorts Describe buffers."
  :group 'macports)

(defvar-local macports-describe--status nil
  "A variable indicating the buffer's loading status.")

(defun macports-describe-port (port)
  "Display detailed information about PORT."
  (interactive (list (macports-core--prompt-port)))
  (let* ((buf-name (format "*Port: %s*" port))
         (buf (get-buffer buf-name)))
    (if buf
        (pop-to-buffer buf)
      (macports-describe--init-buffer port (get-buffer-create buf-name)))))

(defun macports-describe--init-buffer (port buf)
  "Init help BUF describing PORT."
  (with-help-window buf
    (with-current-buffer standard-output
      (setq-local revert-buffer-function
                  (lambda (&rest _)
                    (macports-describe--init-buffer port buf)))
      (setq-local macports-describe--status (list :rdependents nil :rdeps nil :rdeps-build-test-only nil))
      (macports-dispatch-mode)
      (shell-command (concat macports-command " -q info " port) standard-output)
      (macports-describe--linkify-urls)
      (macports-describe--linkify-emails)
      (macports-describe--linkify-github)
      (macports-describe--linkify-ports-in-headers
       "Replaced by:" "Sub-ports:" "Fetch Dependencies:" "Extract Dependencies:" "Build Dependencies:"
       "Library Dependencies:" "Runtime Dependencies:" "Test Dependencies:" "Conflicts with:")
      (macports-describe--style-headings)
      (goto-char (point-max))
      (macports-describe--heading "Dependents:")
      (macports-describe--async-insert
       (concat macports-command " -q rdependents " port) "None\n"
       (lambda (s-marker e-marker had-output succeeded)
         (if (and had-output succeeded)
             (macports-describe--linkify-ports (marker-position s-marker) (marker-position e-marker)))
         (macports-describe--update-status :rdependents t)
         (macports-describe--dispose-markers s-marker e-marker)))
      (macports-describe--heading "Deps:")
      (macports-describe--async-insert
       (concat macports-command " -q rdeps " port) "None\n"
       (lambda (s-marker e-marker had-output succeeded)
         (macports-describe--update-status :rdeps t)
         (if (and had-output succeeded)
             (progn
               (macports-describe--linkify-ports (marker-position s-marker) (marker-position e-marker))
               (macports-describe--async-mark-build-test-rdeps port s-marker e-marker))
           (macports-describe--update-status :rdeps-build-test-only t))))
      ;; Return the created buffer, which is the current buffer
      (current-buffer))))

(defun macports-describe--heading (text)
  "Insert a heading with content TEXT."
  (insert (format "\n%s\n" (propertize text 'face 'macports-describe-heading))))

(defun macports-describe--async-insert (command empty-msg callback)
  "Insert output of COMMAND with temporary loading message.

If result is blank, show EMPTY-MSG instead.

CALLBACK is a function that accepts:
- The start marker of the inserted text
- The end marker of the inserted text
- A boolean that is nil if EMPTY-MSG was used
- A boolean that is nil if the command exited with an error

CALLBACK is responsible for setting the markers to nil when finished."
  (let ((s-marker (point-marker))
        e-marker
        (buf (current-buffer)))
    (insert (propertize "Loading..." 'face 'macports-describe-loading))
    (setq e-marker (point-marker))
    (macports-core--async-shell-command-to-string
     command
     (lambda (result exit-status)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (save-excursion
             (let* ((inhibit-read-only t)
                    (cleaned (replace-regexp-in-string "\r" "" result))
                    (had-output (not (string-blank-p cleaned))))
               (delete-region (marker-position s-marker) (marker-position e-marker))
               (goto-char (marker-position s-marker))
               (insert (if had-output cleaned empty-msg))
               (set-marker e-marker (point))
               (funcall callback s-marker e-marker had-output (= exit-status 0))))))))))

(defun macports-describe--linkify-urls ()
  "Linkify URLs in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "https?://[^ \n]+" nil t)
      (pcase-let ((url (thing-at-point-url-at-point))
                  (`(,beg . ,end) (bounds-of-thing-at-point 'url)))
        (delete-region beg end)
        (help-insert-xref-button url 'help-url url)))))

(defun macports-describe--linkify-emails ()
  "Linkify email addresses in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[^[:blank:]]+@[^[:blank:]]" nil t)
      ;; Using 'email fails in cases where the email contains a number, so we
      ;; use 'symbol which happens to give good results.
      ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=61519
      (pcase-let ((email (thing-at-point 'symbol))
                  (`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
        (delete-region beg end)
        (help-insert-xref-button email 'help-url (concat "mailto:" email))))))

(defun macports-describe--linkify-github ()
  "Linkify GitHub usernames in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "GitHub:[[:blank:]\n]*[^[:blank:]]" nil t)
      (pcase-let ((username (thing-at-point 'symbol))
                  (`(,beg . ,end) (bounds-of-thing-at-point 'symbol)))
        (delete-region beg end)
        (help-insert-xref-button username 'help-url (concat "https://github.com/" username))))))

(defun macports-describe--style-headings ()
  "Apply heading style to current buffer content."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[^[:blank:]][^:\n]+:" nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(face macports-describe-heading)))))

(defun macports-describe--linkify-ports-in-headers (&rest headers)
  "Linkify ports listed under the specified HEADERS."
  (save-excursion
    (dolist (header headers)
      (goto-char (point-min))
      (when (re-search-forward (concat "^" header) nil t)
        (let ((start (match-end 0))
              (end (save-excursion (and (re-search-forward "^[^[:blank:]]" nil t) (match-beginning 0)))))
          (macports-describe--linkify-ports start end))))))

(defun macports-describe--linkify-ports (start end)
  "Linkify ports between START and END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward "[^[:blank:]\n,]+" end t)
      (let ((start (match-beginning 0))
            (end (match-end 0))
            (port (match-string-no-properties 0)))
        (make-text-button
         start end
         'action (lambda (_) (macports-describe-port port))
         'face 'default
         'mouse-face 'macports-describe-port-button-hover)))))

(defun macports-describe--async-mark-build-test-rdeps (port s-marker e-marker)
  "Mark build/test-only rdeps for PORT within the bounded region.

Will null-out S-MARKER and E-MARKER markers upon completion."
  (let ((buf (current-buffer)))
    (macports-core--async-shell-command-to-string
     (concat macports-command " -q rdeps --no-build --no-test " port)
     (lambda (result _exit-status)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (save-excursion
             (goto-char (marker-position s-marker))
             (let (had-build-test-only
                   (inhibit-read-only t)
                   (no-build (split-string (string-trim result))))
               (while (re-search-forward "[^[:blank:]\n]+" (marker-position e-marker) t)
                 (let ((dep (match-string-no-properties 0)))
                   (unless (member dep no-build)
                     (add-text-properties (match-beginning 0) (match-end 0) '(face macports-describe-build-test-only-rdeps))
                     (insert (propertize "*" 'face 'macports-describe-build-test-only-rdeps))
                     (setq had-build-test-only t))))
               (when had-build-test-only
                 (goto-char (marker-position e-marker))
                 (insert "\n *Build- or test-only dependency"))))
           (macports-describe--dispose-markers s-marker e-marker)
           (macports-describe--update-status :rdeps-build-test-only t)))))))

(defun macports-describe--update-status (key value)
  "Update the load status with KEY and VALUE."
  (setq macports-describe--status (plist-put macports-describe--status key value)))

(defun macports-describe--dispose-markers (&rest markers)
  "Dispose of all MARKERS."
  (mapc (lambda (m) (set-marker m nil)) markers))

(defun macports-describe-port-contents (port)
  "Display contents of PORT in a new buffer."
  (with-help-window (get-buffer-create (format "*Port contents: %s*" port))
    (with-current-buffer standard-output
      (macports-dispatch-mode)
      (shell-command (concat macports-command " -q contents " port) standard-output))
    (macports-describe--linkify-files)))

(defun macports-describe--linkify-files ()
  "Linkify files in buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\(https?:\\)?\\(/[^[:blank:]\n,]+\\)" nil t)
      (let ((scheme (match-string-no-properties 1))
            (start (match-beginning 2))
            (end (match-end 2))
            (file (match-string-no-properties 2)))
        (unless scheme
          (make-text-button
           start end
           'action (lambda (_) (find-file file))
           'face 'default
           'mouse-face 'macports-describe-port-button-hover))))))

(defun macports-describe-port-distfiles (port)
  "Display distfiles of PORT in a new buffer."
  (with-help-window (get-buffer-create (format "*Port distfiles: %s*" port))
    (with-current-buffer standard-output
      (macports-dispatch-mode)
      (shell-command (concat macports-command " -q distfiles " port) standard-output))
    (macports-describe--linkify-urls)
    (macports-describe--linkify-files)))

(provide 'macports-describe)
;;; macports-describe.el ends here
