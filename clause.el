;;; clause.el --- Clause functions -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Marty Hiatt
;; Author: Marty Hiatt <martianhiatus [a t] riseup [dot] net>
;; Keywords: wp, convenience, sentences
;; Version: 0.1
;; URL: https://codeberg.org/martianh/clause.el
;; Package-Requires: ((emacs "27.1") (segment "0.1") (mark-thing-at "0.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Functions for moving, marking and killing by clause.

;; A clause is delimited by , ; : ( ) and –. You can add extra
;; delimiters by customizing `clause-extra-delimiters'

;;; Code:
(require 'segment)
(require 'mark-thing-at)

(defgroup clause nil
  "Clause functions."
  :group 'convenience)

(defcustom clause-use-segment nil
  "Whether to use `segment' to determine clause and sentence endings.
Note that segment's rules are language-based. Call
`segment-set-language-for-buffer' to specify which language rules
to use."
  :type 'boolean)

(defcustom clause-handle-org-footnotes nil
  "Whether to handle org-footnotes."
  :type 'boolean)

(defcustom clause-extra-delimiters nil
  "Extra characters to add to the clause-ending regex."
  :type 'string)

(defvar clause-simplified-org-footnote-re
  "\\(\\[fn:[-_[:word:]]+\\]\\)?")

(defvar clause-non-sentence-end-clause-re
  ;; en dash, opening paren, em dash, 2 to 3 -
  "\\([–(—]\\|\\(-\\)\\{2,3\\}\\)")

(defun clause-forward-sentence-function ()
  "Return the forkward sentence function to use."
  (if clause-use-segment
      'segment-forward-sentence
    'forward-sentence))

(defun clause-backward-sentence-function ()
  "Return the backward sentence function to use."
  (if clause-use-segment
      'segment-backward-sentence
    'backward-sentence))

(defun clause-forward-sentence (&optional arg)
  "Call `clause-forward-sentence-function' with ARG."
  (apply (clause-forward-sentence-function) (list arg)))

(defun clause-backward-sentence (&optional arg)
  "Call `clause-backward-sentence-function' with ARG."
  (apply (clause-backward-sentence-function) (list arg)))

(defun clause--sentence-end-base-clause-re ()
  "Return a `sentence-end-base' type regex with clause-ending characters added."
  (concat ;"[)]*" ; poss closing parens prior to clause char
   "[" ; opening of sentence-end-base regex
   "),;:" ; our separators
   clause-extra-delimiters
   (substring sentence-end-base 1)
   (when clause-handle-org-footnotes
     clause-simplified-org-footnote-re)))

(defun clause--after-space-clause-char ()
  "Go to next clause character within the reach of `clause-forward-sentence'.
Returns the position just after the character."
  (re-search-forward clause-non-sentence-end-clause-re
                     ;; limit search:
                     (save-excursion (clause-forward-sentence)
                                     (point))
                     t)) ; nil when nothing found

;;;###autoload
(defun clause-forward-clause (&optional arg)
  "Move forward to beginning of next clause.
With ARG, do this that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (dotimes (_count (or arg 1))
      (skip-chars-forward " –(—]-") ; move past space + clause char
      (or (clause--after-space-clause-char)
          (clause-forward-sentence)))))

;;;###autoload
(defun clause-backward-clause (&optional arg)
  "Move backward to beginning of current clause.
With ARG, do this that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (dotimes (_count (or arg 1))
      (skip-chars-backward "–(—]-") ; move before char + space
      (or (when
              (re-search-backward clause-non-sentence-end-clause-re
                                  ;; limit search:
                                  (save-excursion (clause-backward-sentence)
                                                  (point))
                                  t)
            (skip-chars-forward "–(—]-")) ; leave point after char + space
          (clause-backward-sentence)))))

;;;###autoload
(defun clause-kill-to-clause (&optional arg)
  "Kill text up to the next clause.
With ARG, do so that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (dotimes (_count (or arg 1))
      (if (save-excursion (clause--after-space-clause-char))
          (kill-region (point) (re-search-forward clause-non-sentence-end-clause-re))
        (kill-sentence)))))

;;;###autoload
(defun clause-kill-current-clause (&optional arg)
  "Kill the current clause.
With ARG, do so that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (clause-backward-clause)
    (dotimes (_count (or arg 1))
      (if (clause--after-space-clause-char)
          (kill-region (point) (re-search-forward clause-non-sentence-end-clause-re))
        (kill-sentence)))))

;;;###autoload
(defun clause-mark-clause ()
  "Mark current clause."
  (interactive)
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (mark-sentence))) ; ext dep

;;;###autoload
(defun clause-mark-to-clause (&optional arg)
  "Mark from point to end of current clause.
With ARG, do so that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (mark-end-of-sentence arg)))


(provide 'clause)
;;; clause.el ends here
