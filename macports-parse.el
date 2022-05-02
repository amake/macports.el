;;; macports-parse.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-installed.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-gen.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-installed.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: utilities for parsing output from MacPorts

;;; Code:

(defun macports-parse--installed-items ()
  "Return linewise output of `port installed'."
  (let ((output (string-trim (shell-command-to-string "port -q installed"))))
    (unless (string-empty-p output)
      (mapcar
       (lambda (line) (split-string (string-trim line)))
       (split-string output "\n")))))

(defun macports-parse--leaf-items ()
  "Return linewise output of `port echo leaves'."
  (let ((output (string-trim (shell-command-to-string "port -q echo leaves"))))
    (unless (string-empty-p output)
      (split-string output))))

(defun macports-parse--requested-items ()
  "Return linewise output of `port echo requested'."
  (let ((output (string-trim (shell-command-to-string "port -q echo requested"))))
    (unless (string-empty-p output)
      (split-string output))))

(defun macports-parse--outdated-items ()
  "Return linewise output of `port outdated'."
  (let ((output (string-trim (shell-command-to-string "port -q outdated"))))
    (unless (string-empty-p output)
      (mapcar
       #'split-string
       (split-string output "\n")))))

(provide 'macports-parse)
;;; macports-parse.el ends here
