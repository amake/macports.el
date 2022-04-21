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
   ("d" "Debug" "-d")
   ("n" "Non-interactive" "-N")]
  ["Commands"
   ("s" "Selfupdate" macports-selfupdate)
   ("r" "Reclaim" macports-reclaim)]
  ["Lists"
   ("o" "Outdated" macports-outdated)
   ("i" "Installed" macports-installed)])

(defun macports-selfupdate (args)
  "Run MacPorts selfupdate with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (compilation-start (string-join `("sudo port" ,@args "selfupdate") " ") t))

(defun macports-reclaim (args)
  "Run MacPorts reclaim with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (compilation-start (string-join `("sudo port" ,@args "reclaim") " ") t))

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

(defvar macports-outdated-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "u" #'macports-outdated-mark-upgrade)
    (define-key map "U" #'macports-outdated-mark-upgrades)
    (define-key map "x" #'macports-outdated-upgrade)
    map)
  "Keymap for `macports-outdated-mode'.")

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
               (format "Ports to upgrade: %s.  Proceed? " (string-join ports " ")))
          (compilation-start (string-join `("sudo port upgrade" ,@ports) " ") t))
      (user-error "No ports specified"))))

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
    (list (nth 0 fields) (vector (nth 0 fields) (nth 1 fields) (nth 3 fields)))))

;;;###autoload
(defun macports-installed ()
  "List installed ports."
  (interactive)
  (pop-to-buffer "*macports-installed*")
  (macports-installed-mode)
  (tablist-revert))

(defvar macports-installed-columns
  [("Port" 32 t)
   ("Version" 48 t)
   ("Active" 8 t)]
  "Columns to be shown in `macports-installed-mode'.")

(define-derived-mode macports-installed-mode tabulated-list-mode "MacPorts installed"
  "Major mode for handling a list of installed MacPorts ports."
  (setq tabulated-list-format macports-installed-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Port" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-installed-refresh nil t)
  (tabulated-list-init-header))

(defun macports-installed-refresh ()
  "Refresh the list of installed ports."
  (setq tabulated-list-entries
        (mapcar #'macports--parse-installed (macports--installed-lines))))

(defun macports--installed-lines ()
  "Return linewise output of `port installed'."
  (let ((output (string-trim (shell-command-to-string "port installed"))))
    (cond ((string-prefix-p "The following ports are currently installed:" output)
           (cdr (mapcar #'string-trim (split-string output "\n")))))))

(defun macports--parse-installed (line)
  "Parse a LINE output by `macports--installed-lines'."
  (let ((fields (split-string line)))
    (list (nth 0 fields) (vector (nth 0 fields) (nth 1 fields) (if (nth 2 fields) "Yes" "")))))

(provide 'macports)
;;; macports.el ends here
