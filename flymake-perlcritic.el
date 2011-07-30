;; A replcement flymake handler for Perl to invoke Perl::Critic
;;
;; This makes use of features in my fork of flymake.el found at:
;;   https://github.com/illusori/emacs-flymake
;;
;; Author: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;; Homepage: https://github.com/illusori/emacs-flymake-perlcritic
;;
;; Usage:
;;   (require 'flymake-perlcritic)

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
                          (if (fboundp 'flymake-create-temp-copy)
                            'flymake-create-temp-copy
                            'flymake-create-temp-inplace)))
             (local-file (file-relative-name temp-file
                           (file-name-directory buffer-file-name))))
        (list flymake-perlcritic-command
          (list local-file
            (concat
              (if flymake-perlcritic-profile (concat "--profile " flymake-perlcritic-profile) "")
              (concat "--severity " (number-to-string flymake-perlcritic-severity))))))
      )
    (add-hook 'perl-mode-hook (lambda() (flymake-mode 1)))
    )
  )

(provide 'flymake-perlcritic)
