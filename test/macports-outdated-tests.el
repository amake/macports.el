;;; macports-outdated-tests.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-outdated-tests.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; macports-outdated-tests.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-outdated-tests.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: tests

;;; Code:

(require 'macports)
(require 'transient)

(ert-deftest macports-outdated-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) (concat "foobar                               1.0_0 < 2.0_0\n"
                                 "bizzbazz                             0.1_0 < 0.1_1\n"
                                 "bazinga                              0.2_0 < 0.1_0  (epoch 0 < 1)\n"))))
    (macports-outdated)
    (should (equal '(("bazinga"
                      ["bazinga" "0.2_0" "0.1_0" "(epoch 0 < 1)"])
                     ("bizzbazz"
                      ["bizzbazz" "0.1_0" "0.1_1" ""])
                     ("foobar"
                      ["foobar" "1.0_0" "2.0_0" ""]))
                   tabulated-list-entries))))

(ert-deftest macports-outdated-test-empty ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "\n")))
    (macports-outdated)
    (should (equal nil tabulated-list-entries))))

(ert-deftest macports-outdated-test-custom-command ()
  (cl-letf ((macports-command "foobar")
            ((symbol-function #'shell-command-to-string)
             (lambda (cmd)
               (should (string-prefix-p "foobar " cmd))
               "\n")))
    (macports-outdated)))

(ert-deftest macports-outdated-upgrade-test-all ()
  (cl-letf* (((symbol-function #'shell-command-to-string)
              (lambda (_) (concat "foobar                               1.0_0 < 2.0_0\n"
                                  "bizzbazz                             0.1_0 < 0.1_1\n")))
             ((symbol-function #'macports-upgrade)
              (lambda (ports)
                (should (null ports)))))
    (macports-outdated)
    (let ((msg (macports-outdated-mark-upgrades)))
      (should (equal "Outdated ports marked for upgrade: 2"
                     msg)))
    (macports-outdated-upgrade)))

(ert-deftest macports-outdated-upgrade-test-some ()
  (cl-letf* (((symbol-function #'shell-command-to-string)
              (lambda (_) (concat "foobar                               1.0_0 < 2.0_0\n"
                                  "bizzbazz                             0.1_0 < 0.1_1\n")))
             ((symbol-function #'macports-upgrade)
              (lambda (ports)
                (should (equal '("bizzbazz") ports)))))
    (macports-outdated)
    (goto-char (point-min))
    (macports-outdated-mark-upgrade)
    (macports-outdated-upgrade)))

(provide 'macports-outdated-tests)
;;; macports-outdated-tests.el ends here
