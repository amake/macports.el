;;; macports-core.el --- A porcelain for MacPorts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-core.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; flutter-gen.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-core.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: core operations

;;; Code:

(require 'transient)
(require 'compile)
(require 'subr-x)

(defgroup macports nil
  "MacPorts"
  :group 'convenience)

(defcustom macports-use-sudo t
  "If non-nil, use sudo for MacPorts operations as necessary."
  :type 'boolean)


;;;###autoload (autoload 'macports "macports" nil t)
(transient-define-prefix macports ()
  "Transient for MacPorts."
  [["Commands"
    ("s" "Selfupdate" macports-selfupdate)
    ("r" "Reclaim" macports-reclaim)
    ("I" "Install" macports-install)]
   ["Ports"
    ("o" "Outdated" macports-outdated)
    ("i" "Installed" macports-installed)
    ("S" "Select" macports-select)]])

(defvar macports-dispatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'macports)
    map)
  "Keymap for `macports-installed-mode'.")

(define-minor-mode macports-dispatch-mode
  "A minor mode allowing easy access to MacPorts commands via `macports'."
  :keymap macports-dispatch-mode-map)

(eval-and-compile
  (defconst macports-core--global-flags-infix
    ["Arguments"
     ("v" "Verbose" "-v")
     ("d" "Debug" "-d")
     ("q" "Quiet" "-q")
     ("n" "Non-interactive" "-N")]
    "Global flags for the `port` command."))

;;;###autoload (autoload 'macports "macports-selfupdate" nil t)
(transient-define-prefix macports-selfupdate ()
  "Transient for MacPorts selfupdate."
  macports-core--global-flags-infix
  ["Commands"
   ("s" "Selfupdate" macports-core--selfupdate-exec)])

(defun macports-core--selfupdate-exec (args)
  "Run MacPorts selfupdate with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (macports-core--exec (macports-core--privileged-command `(,@args "selfupdate"))))

;;;###autoload (autoload 'macports "macports-reclaim" nil t)
(transient-define-prefix macports-reclaim ()
  "Transient for MacPorts reclaim."
  macports-core--global-flags-infix
  ["Commands"
   ("r" "Reclaim" macports-core--reclaim-exec)])

(defun macports-core--reclaim-exec (args)
  "Run MacPorts reclaim with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (macports-core--exec (macports-core--privileged-command `(,@args "reclaim"))))

;; TODO: Support choosing variants
(defun macports-install ()
  "Interactively choose a port to install."
  (interactive)
  (let ((port (completing-read
               "Search: "
               (split-string (shell-command-to-string "port -q echo name:")))))
    (macports-core--exec (macports-core--privileged-command `("-N" "install" ,port)))))

(defun macports-core--exec (command &optional after)
  "Execute COMMAND, and then AFTER if supplied."
  (when after
    (macports-core--post-compilation-setup after))
  (let ((buf (compilation-start command t)))
    (with-current-buffer buf
      (macports-dispatch-mode))))

(defun macports-core--post-compilation-setup (func)
  "Arrange to execute FUNC when the compilation process exits."
  (let (advice)
    (setq advice (lambda (old-func &rest args)
                   (apply old-func args)
                   (funcall func)
                   (advice-remove #'compilation-handle-exit advice)))
    (advice-add #'compilation-handle-exit :around advice)))

(defun macports-core--revert-buffer-func ()
  "Return a function that reverts the current buffer at the time of execution."
  (let ((buf (current-buffer)))
    (lambda ()
      (with-current-buffer buf
        (revert-buffer)))))

(defun macports-edit-portfile (port)
  "Open the portfile for PORT in a new buffer."
  (let ((portfile (string-trim (shell-command-to-string (concat "port -q file " port)))))
    (find-file portfile)))

(defun macports-core--privileged-command (args)
  "Build a MacPorts invocation with ARGS list."
  (concat
   (if macports-use-sudo "sudo " "")
   "port "
   (string-join args " ")))

(provide 'macports-core)
;;; macports-core.el ends here
