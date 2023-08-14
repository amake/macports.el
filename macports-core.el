;;; macports-core.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-core.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; macports-core.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
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

(defcustom macports-command "port"
  "The MacPorts port binary."
  :group 'macports
  :type 'string)

(defcustom macports-use-sudo t
  "If non-nil, use sudo for MacPorts operations as necessary."
  :group 'macports
  :type 'boolean)

(defvar macports-open-hook nil
  "Called when `macports' transient is opened.")

(defvar macports-status-strings '(:outdated "" :installed "")
  "Plist of statuses for `macports' transient.")

(defcustom macports-show-status t
  "Whether to display port counts in the main transient buffer."
  :group 'macports
  :type 'boolean)

(defvar macports-core--refresh-major-modes nil
  "Major modes identifying buffers to refresh after operations.")

;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports ()
  "Transient for MacPorts."
  [["Commands"
    ("s" "Selfupdate" macports-selfupdate)
    ("u" "Upgrade outdated" macports-upgrade)
    ("r" "Reclaim" macports-reclaim)
    ("I" "Install" macports-install)]
   ["Ports"
    ("o" (lambda () (plist-get macports-status-strings :outdated)) macports-outdated)
    ("i" (lambda () (plist-get macports-status-strings :installed)) macports-installed)
    ("S" "Select" macports-select)]]
  (interactive)
  (run-hooks 'macports-open-hook)
  (transient-setup 'macports))

(defvar macports-dispatch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "?") #'macports)
    map)
  "Keymap for `macports-installed-mode'.")

(define-minor-mode macports-dispatch-mode
  "A minor mode allowing easy access to MacPorts commands via `macports'."
  :keymap macports-dispatch-mode-map)

(eval-and-compile
  (defconst macports-core--output-flags-infix
    ["Output"
     ("v" "Verbose" "-v")
     ("d" "Debug" "-d")
     ("q" "Quiet" "-q")
     ("n" "Non-interactive" "-N")]
    "Global flags for the `port` command."))

(eval-and-compile
  (defconst macports-core--sources-flags-infix
    ["Sources"
     ("s" "Source-only mode" "-s")
     ("b" "Binary-only mode" "-b")]
    "Source-related flags for the `port` command."))

(eval-and-compile
  (defconst macports-core--exit-status-flags-infix
    ["Exit status"
     ("p" "Proceed on error" "-p")]
    "Exit status flags for the `port` command."))

;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports-selfupdate ()
  "Transient for MacPorts selfupdate."
  macports-core--output-flags-infix
  ["Commands"
   ("s" "Selfupdate" macports-core--selfupdate-exec)])

(defun macports-core--selfupdate-exec (args)
  "Run MacPorts selfupdate with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (macports-core--exec
   (macports-core--privileged-command `(,@args "selfupdate"))
   "*macports-selfupdate*"))

;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports-reclaim ()
  "Transient for MacPorts reclaim."
  macports-core--output-flags-infix
  ["Commands"
   ("r" "Reclaim" macports-core--reclaim-exec)])

(defun macports-core--reclaim-exec (args)
  "Run MacPorts reclaim with ARGS."
  (interactive (list (transient-args transient-current-command)))
  (macports-core--exec
   (macports-core--privileged-command `(,@args "reclaim"))
   "*macports-reclaim*"))

;; TODO: Support choosing variants
;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports-install (ports)
  "Transient for MacPorts install.

PORTS is a list of port names; if not supplied, choose interactively."
  macports-core--output-flags-infix
  macports-core--sources-flags-infix
  ["Commands"
   ("i"
    (lambda () (concat "Install " (string-join (oref transient--prefix scope) ", ")))
    macports-core--install-exec)]
  (interactive (list (macports-core--prompt-ports)))
  (transient-setup 'macports-install nil nil :scope ports))

(defun macports-core--install-exec (ports args)
  "Run MacPorts install with PORTS and ARGS."
  (interactive (list
                (oref transient-current-prefix scope)
                (transient-args transient-current-command)))
  (macports-core--exec
   (macports-core--privileged-command `(,@args "install" ,@ports))
   "*macports-install*"))

(eval-and-compile
  (defconst macports-core--clean-flags-infix
    ["Clean"
     ("A" "All" "--all")
     ("w" "Work" "--work")
     ("D" "Dist" "--dist")
     ("a" "Archive" "--archive")
     ("l" "Logs" "--logs")]
    "Flags for the `port clean` subcommand."))

;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports-clean (port)
  "Transient for MacPorts clean.

If PORT not supplied, choose interactively."
  :incompatible '(("--all" "--work")
                  ("--all" "--dist")
                  ("--all" "--archive")
                  ("--all" "--logs"))
  macports-core--output-flags-infix
  macports-core--clean-flags-infix
  ["Commands"
   ("c"
    (lambda () (concat "Clean " (car (oref transient--prefix scope))))
    macports-core--clean-exec)]
  (interactive (list (macports-core--prompt-port)))
  (transient-setup 'macports-clean nil nil :scope `(,port)))

(defun macports-core--clean-exec (ports args)
  "Run MacPorts clean with PORTS and ARGS."
  (interactive (list
                (oref transient-current-prefix scope)
                (transient-args transient-current-command)))
  (let* ((all-clean-args (mapcar #'cl-caddr (substring macports-core--clean-flags-infix 1)))
         (clean-args (seq-filter (lambda (e) (member e args)) all-clean-args))
         (other-args (seq-filter (lambda (e) (not (member e clean-args))) args)))
    (macports-core--exec
     (macports-core--privileged-command `(,@other-args "clean" ,@clean-args ,@ports))
     "*macports-clean*")))

(defun macports-core--prompt-port ()
  "Prompt user to choose a port from a list of all available ports.

This is quite slow!"
  (let* ((cmd (concat macports-command " -q echo name:")))
    (completing-read
     "Search: "
     (split-string (shell-command-to-string cmd)))))

(defun macports-core--prompt-ports ()
  "Prompt user to choose multiple ports from a list of all available ports.

This is quite slow!"
  (let* ((cmd (concat macports-command " -q echo name:")))
    (completing-read-multiple
     "Ports (separated by comma): "
     (split-string (shell-command-to-string cmd)))))

;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports-upgrade (&optional ports)
  "Transient for MacPorts upgrade."
  macports-core--output-flags-infix
  macports-core--sources-flags-infix
  macports-core--exit-status-flags-infix
  ["Commands"
   ("u"
    (lambda ()
      (concat "Upgrade "
              (let ((ports (oref transient--prefix scope)))
                (if ports (string-join ports ", ") "outdated"))))
    macports-core--upgrade-exec)]
  (interactive)
  (transient-setup 'macports-upgrade nil nil :scope ports))

(defun macports-core--upgrade-exec (ports args)
  "Run MacPorts upgrade with PORTS and ARGS."
  (interactive (list
                (or (oref transient-current-prefix scope) '("outdated"))
                (transient-args transient-current-command)))
  (macports-core--exec
   (macports-core--privileged-command `(,@args "upgrade" ,@ports))
   "*macports-upgrade*"))

(eval-and-compile
  (defconst macports-core--fetch-flags-infix
    ["Fetch"
     ("N" "No mirrors" "--no-mirrors")]
    "Flags for the `port fetch` subcommand."))

;;;###autoload (autoload 'macports "macports-core" nil t)
(transient-define-prefix macports-fetch (port)
  "Transient for MacPorts fetch.

If PORT not supplied, choose interactively."
  macports-core--output-flags-infix
  macports-core--fetch-flags-infix
  ["Commands"
   ("f"
    (lambda () (concat "Fetch " (car (oref transient--prefix scope))))
    macports-core--fetch-exec)]
  (interactive (list (macports-core--prompt-port)))
  (transient-setup 'macports-fetch nil nil :scope `(,port)))

(defun macports-core--fetch-exec (ports args)
  "Run MacPorts fetch with PORTS and ARGS."
  (interactive (list
                (oref transient-current-prefix scope)
                (transient-args transient-current-command)))
  (let* ((all-fetch-args (mapcar #'cl-caddr (substring macports-core--fetch-flags-infix 1)))
         (fetch-args (seq-filter (lambda (e) (member e args)) all-fetch-args))
         (other-args (seq-filter (lambda (e) (not (member e fetch-args))) args)))
    (macports-core--exec
     (macports-core--privileged-command `(,@other-args "fetch" ,@fetch-args ,@ports))
     "*macports-fetch*")))

(defun macports-core--exec (command buf-name)
  "Execute COMMAND in buffer named BUF-NAME."
  (macports-core--post-compilation-setup #'macports-core--refresh-macports-buffers)
  (let ((buf (compilation-start command t (lambda (_) buf-name))))
    (with-current-buffer buf
      (macports-dispatch-mode))))

(defun macports-core--macports-buffers ()
  "List all macports.el buffers."
  (seq-filter
   (lambda (buf)
     (memq
      (buffer-local-value 'major-mode buf)
      macports-core--refresh-major-modes))
   (buffer-list)))

(defun macports-core--refresh-macports-buffers ()
  "Revert all macports.el buffers."
  (dolist (buf (macports-core--macports-buffers))
    (with-current-buffer buf
      (revert-buffer))))

(defun macports-core--post-compilation-setup (func)
  "Arrange to execute FUNC when the compilation process exits."
  (let (advice)
    (setq advice (lambda (old-func &rest args)
                   (unwind-protect
                       (progn
                         (apply old-func args)
                         (funcall func))
                     (advice-remove #'compilation-handle-exit advice))))
    (advice-add #'compilation-handle-exit :around advice)))

(defun macports-edit-portfile (port)
  "Open the portfile for PORT in a new buffer."
  (let* ((cmd (concat macports-command " -q file " port))
         (portfile (string-trim (shell-command-to-string cmd))))
    (find-file portfile)))

(defun macports-port-log (port)
  "Open the log for PORT in a new buffer."
  (let* ((cmd (concat macports-command " -q logfile " port))
         (logfile (string-trim (shell-command-to-string cmd))))
    (find-file-read-only logfile)))

(defun macports-core--privileged-command (args)
  "Build a MacPorts invocation with ARGS list."
  (concat
   (if macports-use-sudo "sudo " "")
   macports-command
   " "
   (string-join args " ")))

(defun macports-core--async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the background.

Return the temporary output buffer which command is writing to during execution.

When the command is finished, call CALLBACK with the result. The provided
function should accept two parameters:
- The command output as a string
- The process's exit code

Inspired by https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00032.html"
  (let ((buf (macports-core--gen-temp-buffer)))
    (set-process-sentinel
     (start-process "Shell" buf shell-file-name shell-command-switch command)
     (lambda (process _signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer buf
           (let ((output
                  (buffer-substring-no-properties (point-min) (point-max)))
                 (exit-status (process-exit-status process)))
             (funcall callback output exit-status)))
         (kill-buffer buf))))
    buf))

(defun macports-core--gen-temp-buffer ()
  "Get a temp buffer. Adapter for pre-Emacs 28 compatibility."
  (let ((args '(" *temp*")))
    (when (and (fboundp 'func-arity)
               (> (cdr (func-arity #'generate-new-buffer)) 1))
      (setq args `(,@args t)))
    (apply #'generate-new-buffer args)))

(provide 'macports-core)
;;; macports-core.el ends here
