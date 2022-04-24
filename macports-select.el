;;; macports-select.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A porcelain for MacPorts: major mode for managing selected ports

;;; Code:

(require 'macports-core)

;;;###autoload
(defun macports-select ()
  "List select ports."
  (interactive)
  (pop-to-buffer "*macports-select*")
  (macports-select-mode))

(defvar macports-select-columns
  [("Group" 24 t)
   ("Selected" 24 t)
   ("Options" 48 t)]
  "Columns to be shown in `macports-select-mode'.")

(defvar macports-select-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map "RET" #'macports-select-port)
    (keymap-set map "?" #'macports)
    map)
  "Keymap for `macports-select-mode'.")

(defun macports-select-port ()
  "Interactively set selection for the selected group."
  (interactive)
  (let* ((group (tabulated-list-get-id))
         (options (split-string (elt (tabulated-list-get-entry) 2)))
         (current (elt (tabulated-list-get-entry) 1))
         (selection (completing-read "Select option: " options nil t)))
    (macports-core--exec
     (macports-privileged-command `("-q" "select" "--set" ,group ,selection))
     (macports-core--revert-buffer-func))))

(define-derived-mode macports-select-mode tabulated-list-mode "MacPorts select"
  "Major mode for managing selected MacPorts ports."
  (setq tabulated-list-format macports-select-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Group" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-select-refresh nil t)
  (tabulated-list-init-header))

(defun macports-select-refresh ()
  "Refresh the list of select ports."
  (setq tabulated-list-entries
        (mapcar #'macports-select--parse-select (macports-select--select-lines))))

(defun macports-select--select-lines ()
  "Return linewise output of `port select'."
  (let ((output (string-trim (shell-command-to-string "port -q select --summary"))))
    (unless (string-empty-p output)
      (split-string output "\n"))))

(defun macports-select--parse-select (line)
  "Parse a LINE output by `macports--select-lines'."
  (let ((fields (split-string line)))
    (list (nth 0 fields) (vector (nth 0 fields) (nth 1 fields) (string-join (cddr fields) " ")))))

(provide 'macports-select)
;;; macports-select.el ends here
