;;; macports-installed.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-installed.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; macports-installed.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-installed.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: major mode for managing installed ports

;;; Code:

(require 'macports-core)
(require 'macports-describe)
(require 'subr-x)
(require 'transient)

(defvar-local macports-installed--init-flag nil
  "Flag for avoiding multiple init.

`tabulated-list-mode' adds `tabulated-list-revert' to
`display-line-numbers-mode-hook'. The result is that when
`global-display-line-numbers-mode' is enabled, merely enabling
`macports-installed-mode' causes `macports-installed-refresh' to be executed.
However when `global-display-line-numbers-mode' is not enabled, we must manually
init by calling `revert-buffer'.

We don't want to call `macports-installed-refresh' twice because it causes
noticeable lag. So this flag is used in `macports-installed' and
`macports-installed-refresh' to ensure that the latter is only called once per
invocation of the former.")

;;;###autoload
(defun macports-installed ()
  "List installed ports."
  (interactive)
  (let ((buf (pop-to-buffer "*macports-installed*")))
    (with-current-buffer buf
      (let (macports-installed--init-flag)
        ;; See `macports-outdated' for why this conditional
        (unless (derived-mode-p #'macports-installed-mode)
          (macports-installed-mode))
        (unless macports-installed--init-flag
          (revert-buffer))))))

(defun macports-installed--update-status-async ()
  "Generate the label for Installed in `macports'."
  (plist-put macports-status-strings :installed "Installed")
  (when macports-show-status
    (let* ((installed "-")
           (leaves "-")
           (inactive "-")
           (count-fn
            (lambda (output)
              (let ((trimmed (string-trim  output)))
                (if (string-empty-p trimmed)
                    0
                  (length (split-string trimmed "\n"))))))
           (update-fn
            (lambda ()
              (plist-put
               macports-status-strings
               :installed
               (format
                "Installed (%s total, %s %s, %s inactive)"
                installed
                leaves
                (if (eq leaves 1) "leaf" "leaves")
                inactive))
              (when transient--showp
                (transient--redisplay)))))
      (macports-core--async-shell-command-to-string
       (concat macports-command " -q installed")
       (lambda (output _exit-status)
         (setq installed (funcall count-fn output))
         (funcall update-fn)))
      (macports-core--async-shell-command-to-string
       (concat macports-command " -q echo leaves")
       (lambda (output _exit-status)
         (setq leaves (funcall count-fn output))
         (funcall update-fn)))
      (macports-core--async-shell-command-to-string
       (concat macports-command " -q echo inactive")
       (lambda (output _exit-status)
         (setq inactive (funcall count-fn output))
         (funcall update-fn))))))

(add-hook 'macports-open-hook #'macports-installed--update-status-async)

(defvar macports-installed-columns
  [("Port" 32 t)
   ("Version" 48 t)
   ("Active" 8 t)
   ("Requested" 10 t)
   ("Leaf" 8 t)]
  "Columns to be shown in `macports-installed-mode'.")

(defvar macports-installed-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'macports-installed-describe-port)
    (define-key map (kbd "c") #'macports-installed-port-contents)
    (define-key map (kbd "e") #'macports-installed-edit-port)
    (define-key map (kbd "f") #'macports-installed-edit-port)
    (define-key map (kbd "L") #'macports-installed-port-log)
    (define-key map (kbd "C") #'macports-installed-port-clean)
    (define-key map (kbd "u") #'macports-installed-mark-uninstall)
    (define-key map (kbd "U") #'macports-installed-mark-inactive)
    (define-key map (kbd "l") #'macports-installed-mark-leaves)
    (define-key map (kbd "a") #'macports-installed-mark-toggle-activate)
    (define-key map (kbd "r") #'macports-installed-mark-toggle-requested)
    (define-key map (kbd "x") #'macports-installed-exec)
    (define-key map (kbd "DEL") #'macports-installed-backup-unmark)
    map)
  "Keymap for `macports-installed-mode'.")

(defun macports-installed-describe-port ()
  "Show details about the current port."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (macports-describe-port (macports-installed--get-port)))

(defun macports-installed-port-contents ()
  "Show contents of the current port."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (macports-describe-port-contents (macports-installed--get-port)))

(defun macports-installed-edit-port ()
  "Open portfile for the current port."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (macports-edit-portfile (macports-installed--get-port)))

(defun macports-installed-port-log ()
  "Open log for the current port."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (macports-port-log (macports-installed--get-port)))

(defun macports-installed-port-clean ()
  "Clean the current port."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (funcall-interactively #'macports-clean (macports-installed--get-port)))

(defun macports-installed-mark-uninstall (&optional _num)
  "Mark a port for uninstall and move to the next line."
  (interactive "p")
  (macports-installed--ensure-macports-installed-mode)
  (tabulated-list-put-tag "U" t))

(defun macports-installed-mark-toggle-activate (&optional _num)
  "Mark a port for activate/deactivate and move to the next line."
  (interactive "p")
  (macports-installed--ensure-macports-installed-mode)
  (let ((active (macports-installed-item-active-p)))
    (cond ((and active (eq (char-after) ?D))
           (tabulated-list-put-tag " " t))
          ((and (not active) (eq (char-after) ?A))
           (tabulated-list-put-tag " " t))
          (active (tabulated-list-put-tag "D" t))
          ((not active) (tabulated-list-put-tag "A" t)))))

(defun macports-installed-item-active-p ()
  "Return non-nil if the current item is activated."
  (macports-installed--ensure-macports-installed-mode)
  (not (string-blank-p (elt (tabulated-list-get-entry) 2))))

(defun macports-installed-mark-inactive (&optional start end)
  "Mark all inactive ports from START to END for uninstall.

Acts within the region when active, otherwise on entire buffer."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  ;; `use-region-beginning', `use-region-end' not available in Emacs 25
  (setq start (or start (and (use-region-p) (region-beginning)) (point-min)))
  (setq end (or end (and (use-region-p) (region-end)) (point-max)))
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (and (< (point) end) (not (eobp)))
        (if (macports-installed-item-active-p)
            (forward-line)
          (macports-installed-mark-uninstall)
          (setq count (1+ count))))
      (message "Inactive ports marked for uninstall: %d" count))))

(defun macports-installed-item-leaf-p ()
  "Return non-nil if the current item is a leaf."
  (macports-installed--ensure-macports-installed-mode)
  (not (string-blank-p (elt (tabulated-list-get-entry) 4))))

(defun macports-installed-mark-leaves (&optional start end)
  "Mark all leaf ports from START to END for uninstall.

Acts within the region when active, otherwise on entire buffer."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  ;; `use-region-beginning', `use-region-end' not available in Emacs 25
  (setq start (or start (and (use-region-p) (region-beginning)) (point-min)))
  (setq end (or end (and (use-region-p) (region-end)) (point-max)))
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (and (< (point) end) (not (eobp)))
        (if (not (macports-installed-item-leaf-p))
            (forward-line)
          (macports-installed-mark-uninstall)
          (setq count (1+ count))))
      (message "Leaf ports marked for uninstall: %d" count))))

(defun macports-installed-item-requested-p ()
  "Return non-nil if the current item is requested."
  (macports-installed--ensure-macports-installed-mode)
  (not (string-blank-p (elt (tabulated-list-get-entry) 3))))

(defun macports-installed-mark-toggle-requested (&optional _num)
  "Mark a port as requested/unrequested and move to the next line."
  (interactive "p")
  (macports-installed--ensure-macports-installed-mode)
  (let ((requested (macports-installed-item-requested-p)))
    (cond ((and requested (eq (char-after) ?r))
           (tabulated-list-put-tag " " t))
          ((and (not requested) (eq (char-after) ?R))
           (tabulated-list-put-tag " " t))
          (requested (tabulated-list-put-tag "r" t))
          ((not requested) (tabulated-list-put-tag "R" t)))))

(defun macports-installed-backup-unmark ()
  "Back up one line and clear any marks on that port."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (forward-line -1)
  (tabulated-list-put-tag " "))

(defun macports-installed--ensure-macports-installed-mode ()
  "Signal a user-error if major mode is not `macports-installed-mode'."
  (unless (derived-mode-p #'macports-installed-mode)
    (user-error "The current buffer is not a MacPorts Installed list")))

(defun macports-installed--get-port ()
  "Get currently selected port."
  (or (elt (tabulated-list-get-entry) 0)
      (user-error "No port selected")))

(defun macports-installed-exec ()
  "Perform marked actions."
  (interactive)
  (macports-installed--ensure-macports-installed-mode)
  (let (uninstall deactivate activate requested unrequested)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((eq (char-after) ?U)
               (push (tabulated-list-get-entry) uninstall))
              ((eq (char-after) ?D)
               (push (tabulated-list-get-entry) deactivate))
              ((eq (char-after) ?A)
               (push (tabulated-list-get-entry) activate))
              ((eq (char-after) ?R)
               (push (tabulated-list-get-entry) requested))
              ((eq (char-after) ?r)
               (push (tabulated-list-get-entry) unrequested)))
        (forward-line)))
    (if (or uninstall deactivate activate requested unrequested)
        (when (macports-installed--prompt-transaction-p uninstall deactivate activate requested unrequested)
          (let ((uninstall-cmd (when uninstall
                                 (macports-core--privileged-command
                                  `("-N" "uninstall" ,@(macports-installed--list-to-args uninstall)))))
                (deactivate-cmd (when deactivate
                                  (macports-core--privileged-command
                                   `("-N" "deactivate" ,@(macports-installed--list-to-args deactivate)))))
                (activate-cmd (when activate
                                (macports-core--privileged-command
                                 `("-N" "activate" ,@(macports-installed--list-to-args activate)))))
                (requested-cmd (when requested
                                 (macports-core--privileged-command
                                  `("-N" "setrequested" ,@(macports-installed--list-to-args requested)))))
                (unrequested-cmd (when unrequested
                                   (macports-core--privileged-command
                                    `("-N" "unsetrequested" ,@(macports-installed--list-to-args unrequested))))))
            (macports-core--exec
             (string-join
              (remq nil (list uninstall-cmd deactivate-cmd activate-cmd requested-cmd unrequested-cmd))
              " && ")
             "*macports-installed-operation*")))
      (user-error "No ports specified"))))

(defun macports-installed--prompt-transaction-p (uninstall deactivate activate requested unrequested)
  "Prompt the user about UNINSTALL, DEACTIVATE, ACTIVATE, REQUESTED, UNREQUESTED."
  (y-or-n-p
   (concat
    (when uninstall
      (format
       "Ports to uninstall: %s.  "
       (macports-installed--list-to-prompt uninstall)))
    (when deactivate
      (format
       "Ports to deactivate: %s.  "
       (macports-installed--list-to-prompt deactivate)))
    (when activate
      (format
       "Ports to activate: %s.  "
       (macports-installed--list-to-prompt activate)))
    (when requested
      (format
       "Ports to set as requested: %s.  "
       (macports-installed--list-to-prompt requested)))
    (when unrequested
      (format
       "Ports to set as unrequested: %s.  "
       (macports-installed--list-to-prompt unrequested)))
    "Proceed? ")))

(defun macports-installed--list-to-prompt (entries)
  "Format ENTRIES for prompting."
  (format "%d (%s)"
          (length entries)
          (mapconcat
           (lambda (entry) (concat (elt entry 0) (elt entry 1)))
           entries
           " ")))

(defun macports-installed--list-to-args (entries)
  "Format ENTRIES as command arguments."
  (apply #'nconc (mapcar
                  (lambda (entry) `(,(elt entry 0) ,(elt entry 1)))
                  entries)))

(define-derived-mode macports-installed-mode tabulated-list-mode "MacPorts installed"
  "Major mode for handling a list of installed MacPorts ports."
  (setq tabulated-list-format macports-installed-columns)
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key `("Port" . nil))
  (add-hook 'tabulated-list-revert-hook #'macports-installed-refresh nil t)
  (tabulated-list-init-header)
  (macports-dispatch-mode))

(add-to-list 'macports-core--refresh-major-modes 'macports-installed-mode)

(defun macports-installed-refresh ()
  "Refresh the list of installed ports."
  (macports-installed--ensure-macports-installed-mode)
  (let ((installed (macports-installed--installed-items))
        (leaves (make-hash-table :test #'equal))
        (requested (make-hash-table :test #'equal)))
    (mapc (lambda (e) (puthash e t leaves))
          (macports-installed--leaf-items))
    (mapc (lambda (e) (puthash e t requested))
          (macports-installed--requested-items))
    (setq tabulated-list-entries
          (mapcar
           (lambda (e)
             (let ((name (nth 0 e))
                   (version (nth 1 e))
                   (active (nth 2 e)))
               (list
                (concat name version)
                (vector
                 name
                 version
                 (if active "Yes" "")
                 (if (gethash name requested) "Yes" "")
                 (if (gethash name leaves) "Yes" "")))))
           installed)
          macports-installed--init-flag
          t)))

(defun macports-installed--installed-items ()
  "Return linewise output of `port installed'."
  (let* ((cmd (concat macports-command " -q installed"))
         (output (string-trim (shell-command-to-string cmd))))
    (unless (string-empty-p output)
      (mapcar
       (lambda (line) (split-string (string-trim line)))
       (split-string output "\n")))))

(defun macports-installed--leaf-items ()
  "Return linewise output of `port echo leaves'."
  (let* ((cmd (concat macports-command " -q echo leaves"))
         (output (string-trim (shell-command-to-string cmd))))
    (unless (string-empty-p output)
      (split-string output))))

(defun macports-installed--requested-items ()
  "Return linewise output of `port echo requested'."
  (let* ((cmd (concat macports-command " -q echo requested"))
         (output (string-trim (shell-command-to-string cmd))))
    (unless (string-empty-p output)
      (split-string output))))

(provide 'macports-installed)
;;; macports-installed.el ends here
