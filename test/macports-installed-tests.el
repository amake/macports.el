;;; macports-installed-tests.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-installed-tests.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; macports-installed-tests.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-installed-tests.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: tests

;;; Code:

(require 'macports)
(require 'transient)

(ert-deftest macports-installed-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                                  (concat "  foobar @1.0_0 (active)\n"
                                          "  bizzbazz @0.1_0\n"
                                          "  bazinga @20220426+blah\n"))
                                 ((equal cmd "port -q echo leaves")
                                  "bazinga\n")
                                 ((equal cmd "port -q echo requested")
                                  "bizzbazz\n")))))
    (macports-installed)
    (should (equal '(("bazinga@20220426+blah"
                      ["bazinga" "@20220426+blah" "" "" "Yes"])
                     ("bizzbazz@0.1_0"
                      ["bizzbazz" "@0.1_0" "" "Yes" ""])
                     ("foobar@1.0_0"
                      ["foobar" "@1.0_0" "Yes" "" ""]))
                   tabulated-list-entries))))

(ert-deftest macports-installed-test-empty ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "\n")))
    (macports-installed)
    (should (equal nil tabulated-list-entries))))

(ert-deftest macports-installed-refresh-test-custom-command ()
  (cl-letf ((macports-command "foobar")
            ((symbol-function #'shell-command-to-string)
             (lambda (cmd)
               (should (string-prefix-p "foobar " cmd))
               "\n")))
    (macports-installed)))

(ert-deftest macports-installed-mark-toggle-activate-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                                  (concat "  foobar @1.0_0 (active)\n"
                                          "  bizzbazz @0.1_0\n"))
                                 ((equal cmd "port -q echo leaves")
                                  "\n")
                                 ((equal cmd "port -q echo requested")
                                  "\n")
                                 (t (should nil))))))
    (macports-installed)
    (goto-char (point-min))
    (should (not (macports-installed-item-active-p)))
    (macports-installed-mark-toggle-activate)
    (should (macports-installed-item-active-p))
    (macports-installed-mark-toggle-activate)
    (goto-char (point-min))
    (should (eq (char-after) ?A))
    (forward-line)
    (should (eq (char-after) ?D))))

(ert-deftest macports-installed-mark-toggle-requested-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                                  (concat "  foobar @1.0_0 (active)\n"
                                          "  bizzbazz @0.1_0\n"))
                                 ((equal cmd "port -q echo leaves")
                                  "\n")
                                 ((equal cmd "port -q echo requested")
                                  "foobar\n")
                                 (t (should nil))))))
    (macports-installed)
    (goto-char (point-min))
    (should (not (macports-installed-item-requested-p)))
    (macports-installed-mark-toggle-requested)
    (should (macports-installed-item-requested-p))
    (macports-installed-mark-toggle-requested)
    (goto-char (point-min))
    (should (eq (char-after) ?R))
    (forward-line)
    (should (eq (char-after) ?r))))

(ert-deftest macports-installed-mark-inactive-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                                  (concat "  foobar @1.0_0 (active)\n"
                                          "  bizzbazz @0.1_0\n"
                                          "  bazinga @20220426+blah\n"))
                                 ((equal cmd "port -q echo leaves")
                                  "bazinga\n")
                                 ((equal cmd "port -q echo requested")
                                  "bizzbazz\n")
                                 (t (should nil))))))
    (macports-installed)
    (macports-installed-mark-inactive)
    (goto-char (point-min))
    (should (eq (char-after) ?U))
    (forward-line)
    (should (eq (char-after) ?U))
    (forward-line)
    (should (eq (char-after) ?\s))))

(ert-deftest macports-installed-mark-leaves-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                                  (concat "  foobar @1.0_0 (active)\n"
                                          "  bizzbazz @0.1_0\n"
                                          "  bazinga @20220426+blah\n"))
                                 ((equal cmd "port -q echo leaves")
                                  (concat "bizzbazz\n"
                                          "bazinga\n"))
                                 ((equal cmd "port -q echo requested")
                                  "\n")
                                 (t (should nil))))))
    (macports-installed)
    (macports-installed-mark-leaves)
    (goto-char (point-min))
    (should (eq (char-after) ?U))
    (forward-line)
    (should (eq (char-after) ?U))
    (forward-line)
    (should (eq (char-after) ?\s))))

(ert-deftest macports-installed-exec-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                                  (concat "  foobar @1.0_0 (active)\n"
                                          "  bizzbazz @0.1_0\n"
                                          "  bazinga @20220426+blah\n"
                                          "  hogefuga @123_4\n"
                                          "  piyo @2.0.1\n"))
                                 ((equal cmd "port -q echo leaves")
                                  "\n")
                                 ((equal cmd "port -q echo requested")
                                  "bazinga\n")
                                 (t (should nil)))))
            ((symbol-function #'y-or-n-p)
             (lambda (prompt)
               (should (equal (concat "Ports to uninstall: 1 (piyo@2.0.1).  "
                                      "Ports to deactivate: 1 (foobar@1.0_0).  "
                                      "Ports to activate: 1 (hogefuga@123_4).  "
                                      "Ports to set as requested: 1 (bizzbazz@0.1_0).  "
                                      "Ports to set as unrequested: 1 (bazinga@20220426+blah).  "
                                      "Proceed? ")
                              prompt))
               t))
            ((symbol-function #'macports-core--exec)
             (lambda (cmd _)
               (should (equal (concat "sudo port -N uninstall piyo @2.0.1 && "
                                      "sudo port -N deactivate foobar @1.0_0 && "
                                      "sudo port -N activate hogefuga @123_4 && "
                                      "sudo port -N setrequested bizzbazz @0.1_0 && "
                                      "sudo port -N unsetrequested bazinga @20220426+blah")
                              cmd)))))
    (macports-installed)
    (macports-installed-mark-toggle-requested)
    (macports-installed-mark-toggle-requested)
    (macports-installed-mark-toggle-activate)
    (macports-installed-mark-toggle-activate)
    (macports-installed-mark-uninstall)
    (macports-installed-exec)))

(provide 'macports-installed-tests)
;;; macports-installed-tests.el ends here
