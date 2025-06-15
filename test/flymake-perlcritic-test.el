;;; flymake-perlcritic-test.el --- Tests for emacs-flymake-perlcritic  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'flymake-perlcritic)

(ert-deftest flymake-perlcritic-init-with-command-name ()
  "Test `flymake-perlcritic-init' with a string command name."
  (let ((flymake-perlcritic-command "perlcritic"))
    (should (equal (flymake-perlcritic-init)
                   '("perlcritic"
                     "--nocolour"
                     "--verbose" "%s:%l:%c:%m.  %e.  (%p)\\n"
                     "--severity" "4")))))

(ert-deftest flymake-perlcritic-init-with-command-path ()
  "Test `flymake-perlcritic-init' with a path to the `perlcritic' command."
  (let ((flymake-perlcritic-command "/path/to/perlcritic"))
    (should (equal (flymake-perlcritic-init)
                   '("/path/to/perlcritic"
                     "--nocolour"
                     "--verbose" "%s:%l:%c:%m.  %e.  (%p)\\n"
                     "--severity" "4")))))

;;; flymake-perlcritic-test.el ends here
