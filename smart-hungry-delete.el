;;; smart-hungry-delete.el --- smart hungry deletion of whitespace

;; Copyright (C) 2017 Hauke Rehfeld

;; Author: Hauke Rehfeld <emacs@haukerehfeld.de>
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: convenience
;; URL: https://github.com/hrehfeld/emacs-smart-hungry-delete

;;; License:

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Delete whitespace between words with some smartness (dumbness) about words, parenthesis and other delimiters

;;; Code:

(defvar-local smart-hungry-delete/char-kill-regexp "[ \t]\\{2,\\}" 
  "The regexp for chars that get skipped over and killed.")

(defvar-local smart-hungry-delete/char-trigger-killall-regexps '((".\\|\n" . "[ \t\n]")
                                                                 ("[ \t\n]" . ".\\|\n"))
  "A list of pairs of matching regexp that trigger a full delete. The first element needs to match at the beginning of the delete region, the second at the end (always in the same directions).")

(defun smart-hungry-delete/add-regexps-left-right (left-char right-char)
  "Add regexps that remove all whitespace right of LEFT-CHAR and left of RIGHT-CHAR to the buffer-local smart-hungry-delete/char-trigger-killall-regexps list.

For example with \"(\" \")\", whitespace to the left of ) will be completely deleted."
  (mapc (lambda (e)
		  (add-to-list 'smart-hungry-delete/char-trigger-killall-regexps e))
		`((,left-char . ".")
		  ("." . ,right-char))))

(defun smart-hungry-delete/default-prog-mode-hook ()
  (progn
	(smart-hungry-delete/add-regexps-left-right "(" ")")
	(smart-hungry-delete/add-regexps-left-right "[" "]")
	(add-to-list 'smart-hungry-delete/char-trigger-killall-regexps '("." . ","))
	))

(defun smart-hungry-delete/default-c-mode-common-hook ()
  (progn
	(smart-hungry-delete/add-regexps-left-right "<" ">")
	(add-to-list 'smart-hungry-delete/char-trigger-killall-regexps '("." . ":"))))


(add-hook 'prog-mode-hook 'smart-hungry-delete/default-prog-mode-hook)
(add-hook 'c-mode-common-hook 'smart-hungry-delete/default-c-mode-common-hook)
(add-hook 'python-mode-hook 'smart-hungry-delete/default-c-mode-common-hook)
(add-hook 'text-mode-hook
          (lambda () 
            (add-to-list 'smart-hungry-delete/char-trigger-killall-regexps
                         '("." . "\\."))
            ))
                                                  
;;;###autoload
(defun smart-hungry-delete/backward-char (arg)
  "If there is more than one char of whitespace between previous word and point, delete all but one unless there's whitespace or newline directly after the point--which will delete all whitespace back to word--, else fall back to (delete-backward-char 1)"
  (interactive "P")
  (prefix-command-preserve-state)
  (smart-hungry-delete/char arg t))

;;;###autoload
(defun smart-hungry-delete/forward-char (arg)
  "If there is more than one char of whitespace between point and next word, delete all but one unless there's whitespace or newline directly before the point--which will delete all whitespace up to word--, else fall back to (delete-char 1)"
  (interactive "P")
  (prefix-command-preserve-state)
  (smart-hungry-delete/char arg))

(defun smart-hungry-delete/char-trigger (to from) 
  "Return t if the region (FROM TO) should be killed completely."
  (save-excursion
    (let ((from (min to from))
          (to (max to from)))
      ;rest of file matched
      (if (or (equal to (point-max))
              (equal from (point-min)))
          t
        (catch 'matched
          (mapc (lambda (checks)
                  (let ((left-check (car checks))
                        (right-check (cdr checks)))
                    (when (and (progn (goto-char from)
                                      (looking-back left-check))
                               (progn (goto-char to)
                                      (looking-at right-check)))
                      (throw 'matched t))))
                smart-hungry-delete/char-trigger-killall-regexps)
          nil)))))
(defun smart-hungry-delete/char (prefix &optional backwards)
  "If there is more than one char of whitespace between previous word and point, delete all but one unless there's whitespace or newline directly after the point--which will delete all whitespace back to word--, else fall back to (delpete-backward-char 1)"
  (interactive "P")
  (if prefix
      (if backwards (delete-char -1) (delete-char 1))
  (let (check kill-end-match change-point fallback)
    (if backwards
        (setq check (lambda (regexp) (looking-back regexp  0 t))
              kill-end-match 'match-beginning
              change-point '1+
              fallback 'delete-backward-char)
        (setq check 'looking-at
              kill-end-match 'match-end
              change-point '1-
              fallback 'delete-char)
      )
    (if (funcall check smart-hungry-delete/char-kill-regexp)
        (let* ((start (funcall kill-end-match 0))
               (kill-start (if (smart-hungry-delete/char-trigger start (point)) 
                               start
                             (funcall change-point start)
                             )))
          (kill-region (min kill-start (point)) (max kill-start (point))))
      ;just fallback to normal delete
      (funcall fallback 1)
             ))))
  


(provide 'smart-hungry-delete)
