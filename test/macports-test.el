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
