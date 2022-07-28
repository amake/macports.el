;;; macports-select.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-select.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; macports-select.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-select.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: major mode for managing selected ports

;;; Code:

(require 'macports-core)
(require 'subr-x)

(defvar-local macports-select--init-flag nil
  "Flag for avoiding multiple init.

See `macports-installed--init-flag' for details.")

;;;###autoload
(defun macports-select ()
  "List select ports."
  (interactive)
  (let ((buf (pop-to-buffer "*macports-select*")))
    (with-current-buffer buf
      (let (macports-select--init-flag)
        ;; See `macports-outdated' for why this conditional
        (unless (derived-mode-p #'macports-select-mode)
          (macports-select-mode))
        (unless macports-select--init-flag
          (revert-buffer))))))

(defvar macports-select-columns
  [("Group" 24 t)
   ("Selected" 24 t)
   ("Options" 48 t)]
  "Columns to be shown in `macports-select-mode'.")

(defvar macports-select-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'macports-select-port)
    map)
  "Keymap for `macports-select-mode'.")

(defun macports-select-port ()
  "Interactively set selection for the selected group."
  (interactive)
  (macports-select--ensure-macports-select-mode)
  (let ((group (tabulated-list-get-id)))
    (if group
        (let* ((options (split-string (elt (tabulated-list-get-entry) 2)))
               (selection (completing-read "Select option: " options nil t)))
          (macports-core--exec
           (macports-core--privileged-command `("-N" "select" "--set" ,group ,selection))
           (macports-core--revert-buffer-func)))
      (user-error "No group selected"))))

(defun macports-select--ensure-macports-select-mode ()
  "Signal a user-error if major mode is not `macports-select-mode'."
  (unless (derived-mode-p #'macports-select-mode)
    (user-error "The current buffer is not a MacPorts Select list")))

(define-derived-mode macports-select-mode tabulated-list-mode "MacPorts select"
  "Major mode for managing selected MacPorts ports."
  (setq tabulated-list-format macports-select-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Group" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-select-refresh nil t)
  (tabulated-list-init-header)
  (macports-dispatch-mode))

(defun macports-select-refresh ()
  "Refresh the list of select ports."
  (macports-select--ensure-macports-select-mode)
  (setq tabulated-list-entries
        (mapcar #'macports-select--parse-select (macports-select--select-lines))
        macports-select--init-flag
        t))

(defun macports-select--select-lines ()
  "Return linewise output of `port select'."
  (let* ((cmd (concat macports-command " -q select --summary"))
         (output (string-trim (shell-command-to-string cmd))))
    (unless (string-empty-p output)
      (split-string output "\n"))))

(defun macports-select--parse-select (line)
  "Parse a LINE output by `macports--select-lines'."
  (let ((fields (split-string line)))
    (list (nth 0 fields) (vector (nth 0 fields) (nth 1 fields) (string-join (cddr fields) " ")))))

(provide 'macports-select)
;;; macports-select.el ends here
