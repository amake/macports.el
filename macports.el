;;; macports.el --- A porcelain for MacPorts

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A porcelain for MacPorts

;;; Code:

(require 'transient)
(require 'tablist)

;;;###autoload (autoload 'macports "macports" nil t)
(transient-define-prefix macports ()
  "Transient for MacPorts."
  ["Arguments"
   ("d" "Debug" "-d")]
  ["MacPorts"
   ("s" "Selfupdate" macports-selfupdate)
   ("o" "Outdated" macports-outdated)])

(defun macports-selfupdate (args)
  "Run MacPorts selfupdate with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (compilation-start (string-join `("sudo port" ,@args "selfupdate") " ") t))

;;;###autoload
(defun macports-outdated ()
  "List outdated ports."
  (interactive)
  (pop-to-buffer "*macports-outdated*")
  (macports-outdated-mode)
  (tablist-revert))

(defvar macports-outdated-columns
  [("Port" 32 t)
   ("Current" 16 t)
   ("Latest" 16 t)]
  "Columns to be shown in `macports-outdated-mode'.")

(define-derived-mode macports-outdated-mode tabulated-list-mode "MacPorts outdated"
  "Major mode for handling a list of MacPorts ports."
  (setq tabulated-list-format macports-outdated-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Port" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-outdated-refresh nil t)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun macports-outdated-refresh ()
  "Refresh the list of outdated ports."
  (setq tabulated-list-entries
        (mapcar #'macports--parse-outdated (macports--outdated-lines))))

(defun macports--outdated-lines ()
  "Return linewise output of `port outdated'."
  (let ((output (string-trim (shell-command-to-string "port outdated"))))
    (cond ((string-prefix-p "No installed ports are outdated." output) nil)
          ((string-prefix-p "The following installed ports are outdated:" output)
           (cdr (split-string output "\n"))))))

(defun macports--parse-outdated (line)
  "Parse a LINE output by `macports--outdated-lines'."
  (let ((fields (split-string line)))
    (list nil (vector (nth 0 fields) (nth 1 fields) (nth 3 fields)))))

(provide 'macports)
;;; macports.el ends here
