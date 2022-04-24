;;; macports-outdated.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-outdated.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-gen.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-outdated.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: major mode for managing outdated ports

;;; Code:

(require 'macports-core)
(require 'macports-describe)
(require 'subr-x)
(require 'keymap)

;;;###autoload
(defun macports-outdated ()
  "List outdated ports."
  (interactive)
  (pop-to-buffer "*macports-outdated*")
  (macports-outdated-mode))

(defvar macports-outdated-columns
  [("Port" 32 t)
   ("Current" 16 t)
   ("Latest" 16 t)]
  "Columns to be shown in `macports-outdated-mode'.")

(defvar macports-outdated-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'macports-outdated-describe-port)
    (keymap-set map "e" #'macports-outdated-edit-port)
    (keymap-set map "u" #'macports-outdated-mark-upgrade)
    (keymap-set map "U" #'macports-outdated-mark-upgrades)
    (keymap-set map "x" #'macports-outdated-upgrade)
    (keymap-set map "DEL" #'macports-outdated-backup-unmark)
    (keymap-set map "?" #'macports)
    map)
  "Keymap for `macports-outdated-mode'.")

(defun macports-outdated-describe-port ()
  "Show details about the current port."
  (interactive nil macports-outdated-mode)
  (macports-describe-port (tabulated-list-get-id)))

(defun macports-outdated-edit-port ()
  "Open portfile for the current port."
  (interactive nil macports-outdated-mode)
  (macports-edit-portfile (tabulated-list-get-id)))

(defun macports-outdated-mark-upgrade (&optional _num)
  "Mark a port for upgrade and move to the next line."
  (interactive "p" macports-outdated-mode)
  (tabulated-list-put-tag "U" t))

(defun macports-outdated-mark-upgrades ()
  "Mark all ports for upgrade."
  (interactive nil macports-outdated-mode)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (macports-outdated-mark-upgrade))))

(defun macports-outdated-upgrade ()
  "Perform marked upgrades."
  (interactive nil macports-outdated-mode)
  (let (ports)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((eq (char-after) ?U)
               (push (tabulated-list-get-id) ports)))
        (forward-line)))
    (if ports
        (when (y-or-n-p
               (format "Ports to upgrade: %d (%s).  Proceed? "
                       (length ports)
                       (string-join ports " ")))
          (macports-core--exec
           (macports-privileged-command `("-q" "upgrade" ,@ports))
           (macports-core--revert-buffer-func)))
      (user-error "No ports specified"))))

(defun macports-outdated-backup-unmark ()
  "Back up one line and clear any marks on that port."
  (interactive nil macports-outdated-mode)
  (forward-line -1)
  (tabulated-list-put-tag " "))

(define-derived-mode macports-outdated-mode tabulated-list-mode "MacPorts outdated"
  "Major mode for handling a list of outdated MacPorts ports."
  (setq tabulated-list-format macports-outdated-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Port" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-outdated-refresh nil t)
  (tabulated-list-init-header))

(defun macports-outdated-refresh ()
  "Refresh the list of outdated ports."
  (setq tabulated-list-entries
        (mapcar #'macports-outdated--parse-outdated (macports-outdated--outdated-lines))))

(defun macports-outdated--outdated-lines ()
  "Return linewise output of `port outdated'."
  (let ((output (string-trim (shell-command-to-string "port -q outdated"))))
    (unless (string-empty-p output)
      (split-string output "\n"))))

(defun macports-outdated--parse-outdated (line)
  "Parse a LINE output by `macports--outdated-lines'."
  (let ((fields (split-string line)))
    (list (nth 0 fields) (vector (nth 0 fields) (nth 1 fields) (nth 3 fields)))))

(provide 'macports-outdated)
;;; macports-outdated.el ends here
