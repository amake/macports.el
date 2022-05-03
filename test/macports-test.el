;; -*- lexical-binding: t; -*-

(require 'macports)

(ert-deftest macports-outdated-refresh-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) (concat "foobar                               1.0_0 < 2.0_0\n"
                            "bizzbazz                             0.1_0 < 0.1_1\n"))))
    (macports-outdated-refresh)
    (should (equal '(("foobar"
                      ["foobar" "1.0_0" "2.0_0"])
                     ("bizzbazz"
                      ["bizzbazz" "0.1_0" "0.1_1"]))
                   tabulated-list-entries))))

(ert-deftest macports-outdated-refresh-test-empty ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "\n")))
    (macports-outdated-refresh)
    (should (equal nil tabulated-list-entries))))

(ert-deftest macports-installed-refresh-test ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (cmd) (cond ((equal cmd "port -q installed")
                             (concat "  foobar @1.0_0 (active)\n"
                                     "  bizzbazz @0.1_0\n"
                                     "  bazinga @20220426+blah\n"))
                            ((equal cmd "port -q echo leaves")
                             "bazinga\n")
                            ((equal cmd "port -q echo requested")
                             "bizzbazz\n")))))
    (macports-installed-refresh)
    (should (equal '(("foobar@1.0_0"
                      ["foobar" "@1.0_0" "Yes" "" ""])
                     ("bizzbazz@0.1_0"
                      ["bizzbazz" "@0.1_0" "" "Yes" ""])
                     ("bazinga@20220426+blah"
                      ["bazinga" "@20220426+blah" "" "" "Yes"]))
                   tabulated-list-entries))))

(ert-deftest macports-installed-refresh-test-empty ()
  (cl-letf (((symbol-function #'shell-command-to-string)
             (lambda (_) "\n")))
    (macports-installed-refresh)
    (should (equal nil tabulated-list-entries))))

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
             (lambda (cmd callback)
               (funcall callback "\n")))
            ((symbol-function #'transient--redisplay)
             (lambda ())))
    (run-hooks 'macports-open-hook)
    (sleep-for 0 100)
    (should (equal '(:outdated "Outdated (0)" :installed "Installed (0 total, 0 leaves, 0 inactive)")
                   macports-status-strings))))

(ert-deftest macports-status-strings-test-disabled ()
  (cl-letf (((symbol-function #'transient--redisplay)
             (lambda ())))
    (let ((macports-show-status nil))
      (run-hooks 'macports-open-hook)
      (sleep-for 0 100)
      (should (equal '(:outdated "Outdated" :installed "Installed")
                     macports-status-strings)))))
