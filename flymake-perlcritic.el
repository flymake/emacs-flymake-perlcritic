;;; flymake-perlcritic.el --- Flymake handler for Perl to invoke Perl::Critic
;;
;; Author: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;; Maintainer: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;; URL: https://github.com/illusori/emacs-flymake-perlcritic
;; Version: 1.0.1
;; Package-Requires: ((flymake "0.3"))
;;
;;; Commentary:
;;
;; flymake-perlcritic.el adds support for running Perl::Critic
;; (http://search.cpan.org/perldoc?Perl::Critic) to perform static
;; analysis of your Perl code in addition to syntax checking.
;;
;;; Usage:
;; (require 'flymake-perlcritic)

(defcustom flymake-perlcritic-command "perlcritic_flymake"
  "If perlcritic_flymake isn't in your $PATH, set this to the command needed to run it."
  :group 'flymake-perlcritic
  :type 'string)

(defcustom flymake-perlcritic-profile nil
  "The path to the profile file to configure perlcritic, if nil then perlcritic will look in its default location (~/.perlcriticrc)."
  :group 'flymake-perlcritic
  :type 'string)

(defcustom flymake-perlcritic-severity 4
  "The severity to run perlcritic at, values are 1 to 5 with 1 being strictest."
  :group 'flymake-perlcritic
  :type 'integer)

(eval-after-load "flymake"
  '(progn
    ;; Add a new error pattern to catch Perl::Critic output, this is a custom format
    ;; defined in perlcritic_flymake since the Perl::Critic default isn't parsable in
    ;; a way that flymake.el likes.
    (add-to-list 'flymake-err-line-patterns
                 '("\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)" 1 2 3 4))
    (defun flymake-perl-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-with-folder-structure))
             (local-file (file-relative-name temp-file
                           (file-name-directory buffer-file-name)))
             (include-dir (if (fboundp 'flymake-find-perl-lib-dir) (flymake-find-perl-lib-dir buffer-file-name))))
        (if (fboundp 'flymake-perlbrew-path-sync)
          (flymake-perlbrew-path-sync))
        (list flymake-perlcritic-command
          (append
            (if include-dir (list (concat "-I" include-dir)))
            (list local-file)
            (if flymake-perlcritic-profile (list "--profile" flymake-perlcritic-profile))
            (list "--severity" (number-to-string flymake-perlcritic-severity)))))
      )
    (defun flymake-perl-cleanup ()
      "Cleanup after `flymake-perl-init' -- delete temp file and dirs."
      (flymake-safe-delete-file flymake-temp-source-file-name)
      (when flymake-temp-source-file-name
        (flymake-delete-temp-directory
         (file-name-directory flymake-temp-source-file-name))))
    (let ((mode-and-masks (flymake-get-file-name-mode-and-masks "example.pm")))
      (if (nth 1 mode-and-masks)
        (setcdr mode-and-masks (cons 'flymake-perl-cleanup (cddr mode-and-masks)))
        (setcdr mode-and-masks (cons 'flymake-perl-cleanup nil))))
    (add-hook 'perl-mode-hook (lambda() (flymake-mode 1)))
    )
  )

(provide 'flymake-perlcritic)
;;; flymake-perlcritic.el ends here
