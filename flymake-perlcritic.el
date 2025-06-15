;;; flymake-perlcritic.el --- Flymake handler for Perl to invoke Perl::Critic
;;
;; Copyright (C) 2011-2024  Free Software Foundation, Inc.
;;
;; Author: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;;         gemmaro <gemmaro.dev@gmail.com>
;; Maintainer: Sam Graham <libflymake-perlcritic-emacs BLAHBLAH illusori.co.uk>
;; URL: https://github.com/illusori/emacs-flymake-perlcritic
;; Version: 1.0.3
;; Package-Requires: ((flymake "1.2"))
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
;; `flymake-perlcritic.el' adds support to `flymake.el' for running
;; `Perl::Critic' (http://search.cpan.org/perldoc?Perl::Critic) to
;; perform static analysis of your Perl code in addition to syntax
;; checking.
;;
;;; Usage:
;; (add-hook 'perl-mode-hook 'flymake-perlcritic-setup)

;;; Code:

(require 'flymake)

(defcustom flymake-perlcritic-command (executable-find "perlcritic")
  "Command of perlcritic.
If `perlcritic' isn't in your `$PATH', set it to the command
needed to run it."
  :group 'flymake-perlcritic
  :type '(choice file string))

(defcustom flymake-perlcritic-profile nil
  "Profile path of perlcritic.
The path to the profile file to configure perlcritic, if nil then
perlcritic will look in its default location (`~/.perlcriticrc')."
  :group 'flymake-perlcritic
  :type '(choice (const :tag "Default" nil)
                 file))

(defconst flymake-perlcritic--severities
  '(choice (const :tag "gentle" 5)
           (const :tag "stern" 4)
           (const :tag "harsh" 3)
           (const :tag "cruel" 2)
           (const :tag "brutal" 1))
  "Severities of perlcritic.
See also the `perlcritic' documentation[1].

[1] https://metacpan.org/dist/Perl-Critic/view/bin/perlcritic#-severity-NAME")

(defcustom flymake-perlcritic-severity 4
  "The perlcritic severity.
The severity to run perlcritic at, values are from \"brutal\" (1)
to \"gentle\" (5) with \"brutal\" (1) being the strictest."
  :group 'flymake-perlcritic
  :type flymake-perlcritic--severities)

(defcustom flymake-perlcritic-error-threshold 5
  "Error threshold for Flymake diagnostic types.
The lower Severities are considered to be at \"warning\" or \"note\" level."
  :group 'flymake-perlcritic
  :type `(choice (const :tag "Default" nil)
                 ,flymake-perlcritic--severities))

(defcustom flymake-perlcritic-warning-threshold nil
  "Warning threshold for Flymake diagnostic types.
The lower severities are considered to be at \"note\" level."
  :group 'flymake-perlcritic
  :type `(choice (const :tag "Default" nil)
                 ,flymake-perlcritic--severities))

(defvar-local flymake-perlcritic--proc nil
  "Flymake perlcritic process.")

(defun flymake-perlcritic-backend (report-fn &rest _args)
  "Flymake for perlcritic.
This calls REPORT-FN to pass diagnostic objects."
  (unless flymake-perlcritic-command
    (error "Cannot find a suitable perlcritic"))
  (when (process-live-p flymake-perlcritic--proc)
    (kill-process flymake-perlcritic--proc))
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      (setq flymake-perlcritic--proc
            (make-process
             :name "flymake-perlcritic"
             :noquery t
             :connection-type 'pipe
             :buffer (generate-new-buffer " *Flymake-perlcritic*")
             :command (flymake-perlcritic-init)
             :sentinel (apply-partially 'flymake-perlcritic--sentinel report-fn source)))
      (process-send-region flymake-perlcritic--proc (point-min) (point-max))
      (process-send-eof flymake-perlcritic--proc))))

(defun flymake-perlcritic--sentinel (report-fn source proc _event)
    "Sentinel for perlcritic process PROC.
SOURCE is the target source for perlcritic.  When perlcritic
exits successfully, it calls REPORT-FN to report to Flymake."
  (when (memq (process-status proc) '(exit signal))
    (unwind-protect
        (if (with-current-buffer source (eq proc flymake-perlcritic--proc))
            (with-current-buffer (process-buffer proc)
              (goto-char (point-min))
              (cl-loop
               while (search-forward-regexp
                      (rx (group (+ digit)) ":"
                          (group (+ digit)) ":"
                          (group (+ digit)) ":"
                          (group (+ not-newline))
                          line-end)
                      nil t)
               for severity    = (string-to-number (match-string 1))
               for line        = (string-to-number (match-string 2))
               for column      = (string-to-number (match-string 3))
               for msg         = (match-string 4)
               for (beg . end) = (flymake-diag-region source line column)
               for type = (cond
                           ((and flymake-perlcritic-error-threshold
                                 (>= severity flymake-perlcritic-error-threshold))
                            :error)
                           ((and flymake-perlcritic-warning-threshold
                                 (>= severity flymake-perlcritic-warning-threshold))
                            :warning)
                           (t :note))
               collect (flymake-make-diagnostic source beg end type msg)
               into diags
               finally (funcall report-fn diags)))
          (flymake-log :warning "Canceling obsolete check %s" proc))
      (kill-buffer (process-buffer proc)))))

(defun flymake-perlcritic-init ()
  "Initialise perlcritic command."
  (let ((command (list flymake-perlcritic-command
                       "--nocolour"
                       "--verbose" "%s:%l:%c:%m.  %e.  (%p)\\n"
                       "--severity" (number-to-string flymake-perlcritic-severity))))
    (if flymake-perlcritic-profile
        (append command (list "--profile" flymake-perlcritic-profile))
      command)))

;;;###autoload
(defun flymake-perlcritic-setup ()
  "Set up Flymake perlcritic.
Add perlcritic to the Flymake diagnostic functions.  To use it,
add this function to `perl-mode-hook'."
  (add-hook 'flymake-diagnostic-functions 'flymake-perlcritic-backend nil 'local))

(provide 'flymake-perlcritic)
;;; flymake-perlcritic.el ends here.
