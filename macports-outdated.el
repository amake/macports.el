;;; macports-outdated.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-outdated.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; macports-outdated.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-outdated.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: major mode for managing outdated ports

;;; Code:

(require 'macports-core)
(require 'macports-describe)
(require 'transient)
(require 'subr-x)

(defvar-local macports-outdated--init-flag nil
  "Flag for avoiding multiple init.

See `macports-installed--init-flag' for details.")

;;;###autoload
(defun macports-outdated ()
  "List outdated ports."
  (interactive)
  (pop-to-buffer "*macports-outdated*")
  (let (macports-outdated--init-flag)
    (macports-outdated-mode)
    (unless macports-outdated--init-flag
      (revert-buffer))))

(defun macports-outdated--update-status-async ()
  "Generate the label for Outdated in `macports'."
  (plist-put macports-status-strings :outdated "Outdated")
  (when macports-show-status
    (macports-core--async-shell-command-to-string
     (concat macports-command " -q outdated")
     (lambda (output)
       (let* ((trimmed (string-trim output))
              (count (if (string-empty-p trimmed)
                         0
                       (length (split-string trimmed "\n")))))
         (plist-put
          macports-status-strings
          :outdated
          (format "Outdated (%d)" count)))
       (transient--redisplay)))))

(add-hook 'macports-open-hook #'macports-outdated--update-status-async)

(defvar macports-outdated-columns
  [("Port" 32 t)
   ("Current" 16 t)
   ("Latest" 16 t)]
  "Columns to be shown in `macports-outdated-mode'.")

(defvar macports-outdated-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'macports-outdated-describe-port)
    (define-key map (kbd "c") #'macports-outdated-port-contents)
    (define-key map (kbd "e") #'macports-outdated-edit-port)
    (define-key map (kbd "u") #'macports-outdated-mark-upgrade)
    (define-key map (kbd "U") #'macports-outdated-mark-upgrades)
    (define-key map (kbd "x") #'macports-outdated-upgrade)
    (define-key map (kbd "DEL") #'macports-outdated-backup-unmark)
    map)
  "Keymap for `macports-outdated-mode'.")

(defun macports-outdated-describe-port ()
  "Show details about the current port."
  (interactive)
  (macports-outdated--ensure-macports-outdated-mode)
  (macports-describe-port (tabulated-list-get-id)))

(defun macports-outdated-port-contents ()
  "Show contents of the current port."
  (interactive)
  (macports-outdated--ensure-macports-outdated-mode)
  (macports-describe-port-contents (tabulated-list-get-id)))

(defun macports-outdated-edit-port ()
  "Open portfile for the current port."
  (interactive)
  (macports-outdated--ensure-macports-outdated-mode)
  (macports-edit-portfile (tabulated-list-get-id)))

(defun macports-outdated-mark-upgrade (&optional _num)
  "Mark a port for upgrade and move to the next line."
  (interactive "p")
  (macports-outdated--ensure-macports-outdated-mode)
  (tabulated-list-put-tag "U" t))

(defun macports-outdated-mark-upgrades ()
  "Mark all ports for upgrade."
  (interactive)
  (macports-outdated--ensure-macports-outdated-mode)
  (save-excursion
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (macports-outdated-mark-upgrade)
        (setq count (1+ count)))
      (message "Outdated ports marked for upgrade: %d" count))))

(defun macports-outdated-upgrade ()
  "Perform marked upgrades."
  (interactive)
  (macports-outdated--ensure-macports-outdated-mode)
  (let (ports)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((eq (char-after) ?U)
               (push (tabulated-list-get-id) ports)))
        (forward-line)))
    (if ports
        (let ((target (if (eq (length ports) (length tabulated-list-entries))
                          nil
                        ports)))
          (with-no-warnings
            ;; Transients are interactive-only but we really want to call this
            ;; interactively here
            (macports-upgrade target)))
      (user-error "No ports specified"))))

(defun macports-outdated-backup-unmark ()
  "Back up one line and clear any marks on that port."
  (interactive)
  (macports-outdated--ensure-macports-outdated-mode)
  (forward-line -1)
  (tabulated-list-put-tag " "))

(defun macports-outdated--ensure-macports-outdated-mode ()
  "Signal a user-error if major mode is not `macports-outdated-mode'."
  (unless (derived-mode-p 'macports-outdated-mode)
    (user-error "The current buffer is not a MacPorts Outdated list")))

(define-derived-mode macports-outdated-mode tabulated-list-mode "MacPorts outdated"
  "Major mode for handling a list of outdated MacPorts ports."
  (setq tabulated-list-format macports-outdated-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Port" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-outdated-refresh nil t)
  (tabulated-list-init-header)
  (macports-dispatch-mode))

(defun macports-outdated-refresh ()
  "Refresh the list of outdated ports."
  (macports-outdated--ensure-macports-outdated-mode)
  (setq tabulated-list-entries
        (mapcar
         (lambda (e)
           (let ((name (nth 0 e))
                 (curr-version (nth 1 e))
                 (new-version (nth 3 e)))
             (list
              name
              (vector
               name
               curr-version
               new-version))))
         (macports-outdated--outdated-items))
        macports-outdated--init-flag
        t))

(defun macports-outdated--outdated-items ()
  "Return linewise output of `port outdated'."
  (let* ((cmd (concat macports-command " -q outdated"))
         (output (string-trim (shell-command-to-string cmd))))
    (unless (string-empty-p output)
      (mapcar
       #'split-string
       (split-string output "\n")))))

(provide 'macports-outdated)
;;; macports-outdated.el ends here
