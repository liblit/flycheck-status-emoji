# Flycheck Status Emoji

`flycheck-status-emoji` replaces the standard
[Flycheck mode-line status indicators](http://www.flycheck.org/manual/latest/Mode-line-display.html#Mode-line-display)
with cute, compact emoji that convey the corresponding information.
For example, a buffer shows status `😔` while being checked, then `😱`
to report errors, `😟` to report warnings, or `😌` if no problems were
found. Personal tastes vary, but I find the emoji more intuitive than
Flycheck’s native indicators, at least for the common statuses. The
emoji indicators are also quite compact, which can be useful in a
crowded mode-line with many minor modes.

## Status Legend

Status Description | Original Indicator | Emoji Indicator | Emoji Description
:----------------- | :----------------: | :-------------: | :----------------
A syntax check is now running in the current buffer. | `FlyC*` | `😔` | [Pensive Face](http://emojipedia.org/pensive-face/)
The current syntax check finished normally with no errors or warnings. | `FlyC` | `😌` | [Relieved Face](http://emojipedia.org/relieved-face/)
The current syntax check finished normally with three errors and five warnings. | `FlyC:3/5` | `😱3/😟5` | [Face Screaming In Fear](http://emojipedia.org/face-screaming-in-fear/), [Worried Face](http://emojipedia.org/worried-face/)
The current buffer was not checked. | `FlyC` | `😐` | [Neutral Face](http://emojipedia.org/neutral-face/)
Automatic syntax checker selection did not find a suitable syntax checker. | `FlyC-` | `😶` | [Face Without Mouth](http://emojipedia.org/face-without-mouth/)
The current syntax check has errored. | `FlyC!` | `😵` | [Dizzy Face](http://emojipedia.org/dizzy-face/)
The current syntax check was interrupted. | `FlyC-` | `😲` | [Astonished Face](http://emojipedia.org/astonished-face/)
The last syntax check had a suspicious result. | `FlyC?` | `😒` | [Unamused Face](http://emojipedia.org/unamused-face/)

If errors or warnings are found, we abbreviate the `😱…/😟…`
indicator smartly:

* omit `😱0` entirely when there are no errors
* omit `😟0` entirely when there are no warnings
* show `😱` instead of `😱1` when there is exactly one error
* show `😟` instead of `😟1` when there is exactly one warning

Note: you may see multicolored icons for several of the emoji faces above. This is your browser’s doing. The actual Emacs package uses monochrome emoji, not colorful icons.

## Quick Start

Install `flycheck-status-emoji` from the fantastic
[<abbr title="Milkypostman’s Emacs Lisp Package Archive">MELPA</abbr>](https://melpa.org/#/getting-started)
repository:
[![MELPA](http://melpa.org/packages/flycheck-status-emoji-badge.svg)](http://melpa.org/#/flycheck-status-emoji),
[![MELPA Stable](http://stable.melpa.org/packages/flycheck-status-emoji-badge.svg)](http://stable.melpa.org/#/flycheck-status-emoji). Or
[save `flycheck-status-emoji.el`](https://raw2.github.com/liblit/flycheck-status-emoji/master/flycheck-status-emoji.el)
somewhere in your Emacs
[`load-path`](http://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Search.html),
then use `M-x load-library RET flycheck-status-emoji RET` to load the
package.

Now use `M-x flycheck-status-emoji-mode` to toggle this package on and
off.  When on, you should see Flycheck status using cute, compact
emoji.  When off, you will see the standard textual Flycheck status
indicators instead.  To enable status emoji permanently, set and save
the `flycheck-status-emoji-mode`
[customization option](https://www.gnu.org/software/emacs/manual/html_node/emacs/Easy-Customization.html).

## Dependencies

`flycheck-status-emoji` builds upon `flycheck-mode`; you should
already have Flycheck installed and working before adding
`flycheck-status-emoji` into the mix. Visit the
[Flycheck home page](http://www.flycheck.org/) or the
[Flycheck GitHub project page](https://github.com/flycheck/flycheck)
for information and instructions.

`flycheck-status-emoji` requires suitable fonts that support the emoji
characters we use. I recommend the
[Symbola font](http://users.teilar.gr/~g1951d/Symbola.zip) by
[George Douros](http://users.teilar.gr/~g1951d/): it covers all emoji
we need with a clean, consistent design. Symbola may also be available
from your preferred Linux distribution. For example, Fedora provides
this font as the
[`gdouros-symbola-fonts` package](http://fedoraproject.org/wiki/Gdouros_Symbola).

After installing an emoji-covering font, you may also need to
configure Emacs itself to use that font where appropriate. For the
Symbola font recommended above, add the following to your Emacs
start-up file:

````elisp
(set-fontset-font t nil "Symbola")
````

The Endless Parentheses blog has
[additional advice on Emacs Unicode fallback font configuration](http://endlessparentheses.com/manually-choose-a-fallback-font-for-unicode.html). See
also
[Roland Walker’s unicode-fonts package](https://github.com/rolandwalker/unicode-fonts)
for a more comprehensive approach to Emacs Unicode font management.

<!-- LocalWords: Flycheck flycheck FlyC errored Milkypostman MELPA -->
<!-- LocalWords: el RET GitHub init Symbola Douros gdouros symbola -->
