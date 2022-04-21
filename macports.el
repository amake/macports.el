;;; macports.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A porcelain for MacPorts

;;; Code:

(require 'transient)
(require 'macports-outdated)
(require 'macports-installed)

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

(provide 'macports)
;;; macports.el ends here
