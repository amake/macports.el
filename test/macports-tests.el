;;; macports-tests.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-tests.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later version.
;;
;; macports-tests.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-tests.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: tests

;;; Code:

(require 'macports)
(require 'transient)

(ert-deftest macports-select-refresh-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) (concat "autofoo                 autofoo-1      autofoo-1 autofoo-2  none\n"
                            "foo                     foo1           foo1 foo2 foo3 none\n"))))
    (macports-select-refresh)
    (should (equal '(("autofoo"
                      ["autofoo" "autofoo-1" "autofoo-1 autofoo-2 none"])
                     ("foo"
                      ["foo" "foo1" "foo1 foo2 foo3 none"]))
                   tabulated-list-entries))))

(ert-deftest macports-select-refresh-test-empty ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "\n")))
    (macports-select-refresh)
    (should (equal nil tabulated-list-entries))))

(ert-deftest macports-select-refresh-test-custom-command ()
  (cl-letf ((macports-command "foobar")
            ((symbol-function #'shell-command-to-string)
             (lambda (cmd)
               (should (string-prefix-p "foobar " cmd))
               "\n")))
    (macports-select-refresh)))

(ert-deftest macports-status-strings-test-default ()
  (cl-letf (((symbol-function #'macports-core--async-shell-command-to-string)
             (lambda (cmd callback)
               (funcall
                callback
                (cond ((equal cmd "port -q installed")
                       (concat "  foobar @1.0_0 (active)\n"
                               "  bizzbazz @0.1_0\n"
                               "  bazinga @20220426+blah\n"))
                      ((equal cmd "port -q echo leaves")
                       "bazinga\n")
                      ((equal cmd "port -q echo inactive")
                       "bizzbazz\nbazinga\n")
                      ((equal cmd "port -q outdated")
                       (concat "foobar                               1.0_0 < 2.0_0\n"
                               "bizzbazz                             0.1_0 < 0.1_1\n"))))))
            ((symbol-function #'transient--redisplay)
             (lambda ())))
    (run-hooks 'macports-open-hook)
    (sleep-for 0 100)
    (should (equal '(:outdated "Outdated (2)" :installed "Installed (3 total, 1 leaf, 2 inactive)")
                   macports-status-strings))))

(ert-deftest macports-status-strings-test-empty ()
  (cl-letf (((symbol-function #'macports-core--async-shell-command-to-string)
             (lambda (_ callback)
               (funcall callback "\n")))
            ((symbol-function #'transient--redisplay)
             (lambda ())))
    (run-hooks 'macports-open-hook)
    (sleep-for 0 100)
    (should (equal '(:outdated "Outdated (0)" :installed "Installed (0 total, 0 leaves, 0 inactive)")
                   macports-status-strings))))

(ert-deftest macports-status-strings-test-custom-command ()
  (cl-letf ((macports-command "foobar")
            ((symbol-function #'macports-core--async-shell-command-to-string)
             (lambda (cmd callback)
               (should (string-prefix-p "foobar " cmd))
               (funcall callback "\n")))
            ((symbol-function #'transient--redisplay)
             (lambda ())))
    (run-hooks 'macports-open-hook)
    (sleep-for 0 100)))

(ert-deftest macports-status-strings-test-disabled ()
  (cl-letf (((symbol-function #'transient--redisplay)
             (lambda ())))
    (let ((macports-show-status nil))
      (run-hooks 'macports-open-hook)
      (sleep-for 0 100)
      (should (equal '(:outdated "Outdated" :installed "Installed")
                     macports-status-strings)))))

(ert-deftest macports-install-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "bizz\nbazz\n"))
            ((symbol-function #'completing-read)
             (lambda (_ collection)
               (should (equal '("bizz" "bazz") collection))
               "bazz"))
            ((symbol-function #'macports-core--exec)
             (lambda (cmd &rest _)
               (should (equal "sudo port -N install bazz" cmd)))))
    (macports-install)))

(ert-deftest macports-install-test-custom-command ()
  (cl-letf ((macports-command "foobar")
            ((symbol-function #'shell-command-to-string)
             (lambda (_) "bizz\nbazz\n"))
            ((symbol-function #'completing-read)
             (lambda (_ _) "bazz"))
            ((symbol-function #'macports-core--exec)
             (lambda (cmd &rest _)
               (should (string-prefix-p "sudo foobar " cmd)))))
    (macports-install)))

(provide 'macports-tests)
;;; macports-tests.el ends here
