;;; macports-describe-tests.el --- A porcelain for MacPorts  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Aaron Madlon-Kay

;; Author: Aaron Madlon-Kay
;; Version: 0.1.0
;; URL: https://github.com/amake/macports.el
;; Package-Requires: ((emacs "25.1") (transient "0.1.0"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; macports-describe-tests.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; macports-describe-tests.el is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
;; A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along with
;; macports-describe-tests.el.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; A porcelain for MacPorts: tests

;;; Code:

(require 'macports)

(defun macports-describe-tests--wait-for-load ()
  "Wait for current BUF to finish loading."
  (while (not (and macports-describe--status (seq-every-p #'identity macports-describe--status)))
    (sleep-for 0 50)))

(defconst macports-test-bin-dir
  (let ((file (or load-file-name buffer-file-name)))
    (concat (file-name-directory file) "bin/")))

(ert-deftest macports-describe-test ()
  (let* ((macports-command (concat macports-test-bin-dir "port-describe"))
         (buf (macports-describe-port "foo"))
         (content (with-current-buffer buf
                    (macports-describe-tests--wait-for-load)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal "foo @1.0 (test)
Variants:             docs

Description:          foo is a thing
Homepage:             http://example.com
Maintainers:          Email: me@example.com, GitHub: me

Dependents:
hoge
  fuga
piyo

Deps:
bar
  baz
buzz
  bazinga*
    bazonga*

 *Build-only dependency
" content))
    (kill-buffer buf)))

(ert-deftest macports-describe-test-no-deps ()
  (let* ((macports-command (concat macports-test-bin-dir "port-describe"))
         (buf (macports-describe-port "bar"))
         (content (with-current-buffer buf
                    (macports-describe-tests--wait-for-load)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal "bar @1.0 (test)
Variants:             docs

Description:          bar is a thing
Homepage:             http://example.com
Maintainers:          Email: me@example.com, GitHub: me

Dependents:
None

Deps:
None

" content))
    (kill-buffer buf)))

(ert-deftest macports-describe-test-no-build-only-deps ()
  (let* ((macports-command (concat macports-test-bin-dir "port-describe"))
         (buf (macports-describe-port "baz"))
         (content (with-current-buffer buf
                    (macports-describe-tests--wait-for-load)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal "baz @1.0 (test)
Variants:             docs

Description:          baz is a thing
Homepage:             http://example.com
Maintainers:          Email: me@example.com, GitHub: me

Dependents:
None

Deps:
foo
  bar
buzz
  bazinga
    bazonga

" content))
    (kill-buffer buf)))

(ert-deftest macports-describe-test-all-build-only-deps ()
  (let* ((macports-command (concat macports-test-bin-dir "port-describe"))
         (buf (macports-describe-port "buzz"))
         (content (with-current-buffer buf
                    (macports-describe-tests--wait-for-load)
                    (buffer-substring-no-properties (point-min) (point-max)))))
    (should (equal "buzz @1.0 (test)
Variants:             docs

Description:          buzz is a thing
Homepage:             http://example.com
Maintainers:          Email: me@example.com, GitHub: me

Dependents:
None

Deps:
foo*
  bar*
baz*
  bazinga*
    bazonga*

 *Build-only dependency
" content))
    (kill-buffer buf)))

(provide 'macports-describe-tests)
;;; macports-describe-tests.el ends here
