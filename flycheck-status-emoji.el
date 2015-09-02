;;; flycheck-status-emoji.el --- Show flycheck status using cute, compact emoji -*- lexical-binding: t -*-

;; Copyright (C) 2015 Ben Liblit

;; Author: Ben Liblit <liblit@acm.org>
;; Created: 13 Aug 2015
;; Version: 1.0
;; Package-Requires: ((emacs "24") (flycheck "0.20") (let-alist "1.0"))
;; Keywords: convenience languages tools
;; Homepage: https://github.com/liblit/flycheck-status-emoji

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

(require 'flycheck)

(eval-when-compile (require 'let-alist))


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
    (concat (list (flycheck-status-emoji--check character))
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
	(let ((pick (pcase (or status flycheck-last-status-change)
		      (`finished
		       (if flycheck-current-errors
			   (let-alist (flycheck-count-errors flycheck-current-errors)
			     (concat
			      (flycheck-status-emoji--face-count ?😱 .error)
			      (when (and .error .warning) '(?/))
			      (flycheck-status-emoji--face-count ?😟 .warning)))
			 ?😌))
		      (`running     ?😔)
		      (`no-checker  ?😶)
		      (`not-checked ?😐)
		      (`errored     ?😵)
		      (`interrupted ?😲)
		      (`suspicious  ?😒))))
	  (list " "
		(if (characterp pick)
		    (string (flycheck-status-emoji--check pick))
		  pick))))
      (flycheck-mode-line-status-text status)))

(setq flycheck-mode-line '(:eval (flycheck-status-emoji-mode-line-text)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  bug reporting
;;

(defconst flycheck-status-emoji-version "1.0"
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
   (list (y-or-n-p "Can you use a GitHub account for issue reporting? ")))
  (if use-github
      (browse-url "https://github.com/liblit/flycheck-status-emoji/issues")
    (eval-when-compile (require 'reporter))
    (let ((reporter-prompt-for-summary-p t))
      (reporter-submit-bug-report
       flycheck-status-emoji-maintainer-address
       (concat "flycheck-status-emoji.el " flycheck-status-emoji-version)
       (list 'flycheck-last-status-change
	     'flycheck-current-errors)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
;;
;;  epilogue
;;

(provide 'flycheck-status-emoji)


;; LocalWords: alist el emacs emoji flycheck github https liblit
;; LocalWords: MERCHANTABILITY readme

;;; flycheck-status-emoji.el ends here
