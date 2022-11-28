;;; macports-select-tests.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-select-tests.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; macports-select-tests.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-select-tests.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: tests

;;; Code:

(require 'macports)
(require 'transient)

(ert-deftest macports-select-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) (concat "autofoo                 autofoo-1      autofoo-1 autofoo-2  none\n"
                                 "foo                     foo1           foo1 foo2 foo3 none\n"))))
    (macports-select)
    (should (equal '(("autofoo"
                      ["autofoo" "autofoo-1" "autofoo-1 autofoo-2 none"])
                     ("foo"
                      ["foo" "foo1" "foo1 foo2 foo3 none"]))
                   tabulated-list-entries))))

(ert-deftest macports-select-test-empty ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "\n")))
    (macports-select)
    (should (equal nil tabulated-list-entries))))

(ert-deftest macports-select-refresh-test-custom-command ()
  (cl-letf ((macports-command "foobar")
            ((symbol-function #'shell-command-to-string)
             (lambda (cmd)
               (should (string-prefix-p "foobar " cmd))
               "\n")))
    (macports-select)))

(ert-deftest macports-select-port-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) (concat "autofoo                 autofoo-1      autofoo-1 autofoo-2  none\n"
                                 "foo                     foo1           foo1 foo2 foo3 none\n")))
            ((symbol-function #'completing-read)
             (lambda (_ options &rest _)
               (should (equal '("autofoo-1" "autofoo-2" "none")
                              options))
               "autofoo-2"))
            ((symbol-function #'macports-core--exec)
             (lambda (cmd &rest _)
               (should (equal "sudo port -N select --set autofoo autofoo-2"
                              cmd)))))
    (macports-select)
    (macports-select-port)))

(provide 'macports-select-tests)
;;; macports-select-tests.el ends here
