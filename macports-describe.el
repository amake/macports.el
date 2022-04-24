;;; macports-describe.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/.emacs.d
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-describe.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-gen.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-describe.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: major mode for viewing details about a port

;;; Code:

(require 'thingatpt)

(defun macports-describe-port (port)
  "Display detailed information about PORT."
  (with-help-window (help-buffer)
    (with-current-buffer standard-output
      (shell-command (concat "port -q info " port) standard-output)
      (macports-describe--linkify))))

(defun macports-describe--linkify ()
  "Linkify URLs in current buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "https?://[^ \n]+" nil t)
      (let ((url (thing-at-point-url-at-point))
            (bounds (bounds-of-thing-at-point 'url)))
        (delete-region (car bounds) (cdr bounds))
        (help-insert-xref-button url 'help-url url)))))

(provide 'macports-describe)
;;; macports-describe.el ends here
