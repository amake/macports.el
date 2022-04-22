;;; macports-core.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A porcelain for MacPorts: core operations

;;; Code:

(require 'transient)

;;;###autoload (autoload 'macports "macports" nil t)
(transient-define-prefix macports ()
  "Transient for MacPorts."
  ["Commands"
   ("s" "Selfupdate" macports-selfupdate)
   ("r" "Reclaim" macports-reclaim)]
  ["Lists"
   ("o" "Outdated" macports-outdated)
   ("i" "Installed" macports-installed)
   ("S" "Select" macports-select)])

;;;###autoload (autoload 'macports "macports-selfupdate" nil t)
(transient-define-prefix macports-selfupdate ()
  "Transient for MacPorts."
  ["Arguments"
   ("d" "Debug" "-d")
   ("n" "Non-interactive" "-N")]
  ["Commands"
   ("s" "Selfupdate" macports-core--selfupdate-exec)])

(defun macports-core--selfupdate-exec (args)
  "Run MacPorts selfupdate with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (compilation-start (string-join `("sudo port -q" ,@args "selfupdate") " ") t))

;;;###autoload (autoload 'macports "macports-reclaim" nil t)
(transient-define-prefix macports-reclaim ()
  "Transient for MacPorts."
  ["Arguments"
   ("d" "Debug" "-d")
   ("n" "Non-interactive" "-N")]
  ["Commands"
   ("r" "Reclaim" macports-core--reclaim-exec)])

(defun macports-core--reclaim-exec (args)
  "Run MacPorts reclaim with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (compilation-start (string-join `("sudo port -q" ,@args "reclaim") " ") t))

(provide 'macports-core)
;;; macports-core.el ends here
