;;; flymake-perlcritic.el --- Flymake handler for Perl to invoke Perl::Critic
;;
;; Copyright (C) 2011-2012  Free Software Foundation, Inc.
;;
;; Author: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;; Maintainer: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;; URL: https://github.com/illusori/emacs-flymake-perlcritic
;; Version: 1.0.3
;; Package-Requires: ((flymake "0.3"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; flymake-perlcritic.el adds support to flymake.el for running Perl::Critic
;; (http://search.cpan.org/perldoc?Perl::Critic) to perform static
;; analysis of your Perl code in addition to syntax checking.
;;
;;; Usage:
;; (require 'flymake-perlcritic)

(eval-when-compile (require 'flymake))

(defcustom flymake-perlcritic-command (executable-find
  (concat (file-name-directory (or load-file-name buffer-file-name))
          "bin/flymake_perlcritic"))
  "If flymake_perlcritic isn't in your $PATH, set this to the command needed to run it."
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

(defun flymake-perlcritic-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-with-folder-structure))
         (local-file (file-relative-name temp-file
                       (file-name-directory buffer-file-name)))
         (include-dir (if (fboundp 'flymake-find-perl-lib-dir)
                        (flymake-find-perl-lib-dir buffer-file-name))))
    (if (fboundp 'flymake-perlbrew-path-sync)
      (flymake-perlbrew-path-sync))
    (list flymake-perlcritic-command
      (append
        (if include-dir (list (concat "-I" include-dir)))
        (list local-file)
        (if flymake-perlcritic-profile (list "--profile" flymake-perlcritic-profile))
        (list "--severity" (number-to-string flymake-perlcritic-severity))))))

(defun flymake-perlcritic-cleanup ()
  "Cleanup after `flymake-perlcritic-init' - delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-delete-temp-directory
      (file-name-directory flymake-temp-source-file-name))))

(eval-after-load "flymake"
  '(progn
    ;; Add a new error pattern to catch Perl::Critic output, this is a custom
    ;; format defined in perlcritic_flymake since the Perl::Critic default
    ;; isn't parsable in a way that flymake.el likes.
    (add-to-list 'flymake-err-line-patterns
                 '("\\(.*\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)" 1 2 3 4))
    (let ((mode-and-masks (flymake-get-file-name-mode-and-masks "example.pm")))
      (setcar mode-and-masks 'flymake-perlcritic-init)
      (if (nth 1 mode-and-masks)
        (setcar (nthcdr 1 mode-and-masks) 'flymake-perlcritic-cleanup)
        (nconc mode-and-masks (list 'flymake-perlcritic-cleanup))))
    (add-hook 'perl-mode-hook (lambda() (flymake-mode 1)) t)))

(provide 'flymake-perlcritic)
;;; flymake-perlcritic.el ends here
