;;; clause.el --- Clause functions -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Marty Hiatt
;; Author: Marty Hiatt <martianhiatus [a t] riseup [dot] net>
;; Keywords: wp, convenience, sentences
;; Version: 0.2
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

;; A clause is delimited by , ; : ( ) and en or em dashes. You can add extra
;; delimiters by customizing `clause-extra-delimiters'.

;; We do our best to imitate `forward-sentence'/`backward-sentence'
;; functionity, rather than rolling with our own preferences, even though with
;; clauses it can only be approximated. So moving forward should leave point
;; after the (last) clause character, before any space, while moving backward should
;; leave point after any clause character and after any space.

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
  ;; en dash surrounded by spaces, opening paren, em dash no spaces,
  ;; or 2 to 3 -
  "\\([[:space:]]–[[:space:]]\\|(\\|—\\|\\(-\\)\\{2,3\\}\\)")

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
  (concat "[" ; opening of sentence-end-base regex
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
                                     (skip-chars-forward " ")
                                     (point))
                     t)) ; nil when nothing found

(defun clause--move-past-clause-char (&optional backward nospace)
  "Move past a clause character.
With BACKWARD, move backwards.
With NOSPACE, don't move past whitespace."
  (let ((clause-chars (if nospace
                          "\\-–(),;:.!?—"
                        "\\- –(),;:.!?—"))
        moved-p) ; order matters for dashes
    (if backward
        (skip-chars-backward (concat clause-chars
                                     clause-extra-delimiters))
      (setq moved-p (skip-chars-forward (concat clause-chars
                                                clause-extra-delimiters)))
      (when (and nospace (looking-at "[[:space:]]–"))
        ;; only advance if whitespace + en dash
        ;; run separate to not skip space after en dash
        (skip-chars-forward " ")
        (skip-chars-forward "–"))
      moved-p))) ; return value for backward-clause

;;;###autoload
(defun clause-forward-clause (&optional arg)
  "Move forward to beginning of next clause.
With ARG, do this that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (dotimes (_count (or arg 1))
      (clause--move-past-clause-char)
      (or (when (clause--after-space-clause-char)
            (skip-chars-backward " ")) ; for en dash + spaces
          (clause-forward-sentence))
      (clause--move-past-clause-char nil :nospace))))

;;;###autoload
(defun clause-backward-clause (&optional arg)
  "Move backward to beginning of current clause.
With ARG, do this that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (dotimes (_count (or arg 1))
      (clause--move-past-clause-char :backward)
      (or (when
              (re-search-backward clause-non-sentence-end-clause-re
                                  ;; limit search:
                                  (save-excursion (clause-backward-sentence)
                                                  (skip-chars-backward " ")
                                                  (point))
                                  t)
            (clause--move-past-clause-char))
          (clause-backward-sentence)))))

;;;###autoload
(defun clause-kill-to-clause (&optional arg)
  "Kill text up to the next clause.
With ARG, do so that many times."
  (interactive "p")
  (let ((sentence-end-base (clause--sentence-end-base-clause-re)))
    (dotimes (_count (or arg 1))
      (if (save-excursion (clause--after-space-clause-char))
          (kill-region (point) (re-search-forward
                                clause-non-sentence-end-clause-re))
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
          (kill-region (point) (re-search-forward
                                clause-non-sentence-end-clause-re))
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
