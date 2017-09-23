;;; flycheck-status-emoji.el --- Show flycheck status using cute, compact emoji -*- lexical-binding: t -*-

;; Copyright (C) 2015–2017 Ben Liblit

;; Author: Ben Liblit <liblit@acm.org>
;; Created: 13 Aug 2015
;; Version: 1.2.2
;; Package-Requires: ((cl-lib "0.1") (emacs "24") (flycheck "0.20") (let-alist "1.0"))
;; Keywords: convenience languages tools
;; URL: https://github.com/liblit/flycheck-status-emoji

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.


;;; Commentary:

;; `flycheck-status-emoji' replaces the standard Flycheck mode-line
;; status indicators with cute, compact emoji that convey the
;; corresponding information.  For example, a buffer shows status “😔”
;; while being checked, then “😱” to report errors, “😟” to report
;; warnings, or “😌” if no problems were found.

;; See <https://github.com/liblit/flycheck-status-emoji#readme> for
;; additional documentation.  Visit
;; <https://github.com/liblit/flycheck-status-emoji/issues> or use
;; command `flycheck-status-emoji-submit-bug-report' to report bugs or
;; offer suggestions for improvement.


;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  dependencies
;;

(require 'cl-lib)
(require 'flycheck)

(eval-when-compile (require 'let-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  customizable cast of characters
;;

(defcustom flycheck-status-emoji-indicator-running ?😔
  "Shown when a syntax check is now running in the current buffer."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-finished-ok ?😌
  "Shown when the current syntax check finished normally with no errors or warnings."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-finished-error ?😱
  "Shown when the current syntax check finished normally, but with one or more errors."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-finished-warning ?😟
  "Shown when the current syntax check finished normally, but with one or more warnings."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-not-checked ?😐
  "Shown when the current buffer was not checked."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-no-checker ?😶
  "Shown when automatic syntax checker selection did not find a suitable syntax checker."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-errored ?😵
  "Shown when the current syntax check has errored."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-interrupted ?😲
  "Shown when the current syntax check was interrupted."
  :group 'flycheck-status-emoji
  :type 'character)

(defcustom flycheck-status-emoji-indicator-suspicious ?😒
  "Shown when the last syntax check had a suspicious result."
  :group 'flycheck-status-emoji
  :type 'character)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  replacement for flycheck's standard mode-line status text
;;

(defun flycheck-status-emoji--check (character)
  "Return a single CHARACTER, but only if displayable on the current frame.

If the current frame cannot display the given CHARACTER, we throw
an exception instead."
  (if (char-displayable-p character)
      character
    (throw 'flycheck-status-emoji--not-displayable nil)))

(defun flycheck-status-emoji--face-count (character count)
  "Concatenate an emoji CHARACTER and a COUNT.

If COUNT is 0, return nil.  If COUNT is 1, return just the emoji
CHARACTER converted to a string.  If COUNT is larger than 1, then
return the CHARACTER followed by COUNT.  Thus, this function
might return “😟2” for a COUNT of 2, but just “😟” for a COUNT of
1.

If the current frame cannot display the given CHARACTER, we throw
an exception instead."
  (when count
    (concat `(,(flycheck-status-emoji--check character))
	    (when (> count 1)
	      (number-to-string count)))))

(defun flycheck-status-emoji-mode-line-text (&optional status)
  "Get a text using emoji to describe STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil.

This function is a drop-in replacement for the standard flycheck
function `flycheck-mode-line-status-text'.  If the selected emoji
cannot be displayed on the current frame,
`flycheck-mode-line-status-text' is automatically used as a
fallback."
  (or (catch 'flycheck-status-emoji--not-displayable
	(let ((pick (cl-ecase (or status flycheck-last-status-change)
		      ('finished
		       (if flycheck-current-errors
			   (let-alist (flycheck-count-errors flycheck-current-errors)
			     (concat
			      (flycheck-status-emoji--face-count flycheck-status-emoji-indicator-finished-error .error)
			      (when (and .error .warning) '(?/))
			      (flycheck-status-emoji--face-count flycheck-status-emoji-indicator-finished-warning .warning)))
			 flycheck-status-emoji-indicator-finished-ok))
		      ('running     flycheck-status-emoji-indicator-running)
		      ('no-checker  flycheck-status-emoji-indicator-no-checker)
		      ('not-checked flycheck-status-emoji-indicator-not-checked)
		      ('errored     flycheck-status-emoji-indicator-errored)
		      ('interrupted flycheck-status-emoji-indicator-interrupted)
		      ('suspicious  flycheck-status-emoji-indicator-suspicious))))
	  `(" "
	    ,(if (characterp pick)
		 (string (flycheck-status-emoji--check pick))
	       pick))))
      (flycheck-mode-line-status-text status)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  customization
;;

(defgroup flycheck-status-emoji nil
  "Show flycheck status using cute, compact emoji"
  :prefix "flycheck-status-emoji-"
  :group 'flycheck
  :link '(url-link :tag "GitHub" "https://github.com/liblit/flycheck-status-emoji"))

;;;###autoload
(define-minor-mode flycheck-status-emoji-mode
  "Toggle Flycheck status emoji mode.

Interactively with no argument, this command toggles the mode.  A
positive prefix argument enables the mode; any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, while `toggle' toggles the state.

When enabled, this mode replaces the standard Flycheck mode-line
status indicators with cute, compact emoji that convey the
corresponding information.  For example, a buffer shows status
“😔” while being checked, then “😱” to report errors, “😟” to report
warnings, or “😌” if no problems were found.

See <https://github.com/liblit/flycheck-status-emoji#readme> for
additional documentation.  Visit
<https://github.com/liblit/flycheck-status-emoji/issues> or use
command `flycheck-status-emoji-submit-bug-report' to report bugs
or offer suggestions for improvement."
  :global t
  :require 'flycheck-status-emoji
  (progn
    (setq flycheck-mode-line
	  (if flycheck-status-emoji-mode
	      '(:eval (flycheck-status-emoji-mode-line-text))
	    (eval (car (or (get 'flycheck-mode-line 'saved-value)
			   (get 'flycheck-mode-line 'standard-value))))))
    (force-mode-line-update t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  bug reporting
;;

(defconst flycheck-status-emoji-version "1.2.2"
  "Package version number for use in bug reports.")

(defconst flycheck-status-emoji-maintainer-address "Ben Liblit <liblit@acm.org>"
  "Package maintainer name and e-mail address for use in bug reports.")

(defun flycheck-status-emoji-submit-bug-report (use-github)
  "Report a `flycheck-status-emoji' bug.
If USE-GITHUB is non-nil, directs web browser to GitHub issue
tracker.  This is the preferred reporting channel.  Otherwise,
initiates (but does not send) e-mail to the package maintainer.
Interactively, prompts for the method to use."
  (interactive
   `(,(y-or-n-p "Can you use a GitHub account for issue reporting? ")))
  (if use-github
      (browse-url "https://github.com/liblit/flycheck-status-emoji/issues")
    (eval-when-compile (require 'reporter))
    (let ((reporter-prompt-for-summary-p t))
      (reporter-submit-bug-report
       flycheck-status-emoji-maintainer-address
       (concat "flycheck-status-emoji.el " flycheck-status-emoji-version)
       '(flycheck-current-errors
	 flycheck-last-status-change
	 flycheck-mode-line
	 flycheck-status-emoji-mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  epilogue
;;

(provide 'flycheck-status-emoji)


;; LocalWords: alist el emacs emoji flycheck github https liblit
;; LocalWords: MERCHANTABILITY readme flycheck's autoload

;;; flycheck-status-emoji.el ends here
