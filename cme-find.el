;;; cme-find.el --- Overrides for semantic find

;; Copyright (C) 2021 Consciencia

;; Author: Consciencia <consciencia@protonmail.com>
;; Version: 1.0.0
;; Keywords: c c++ cme cedet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; :(

;;; Code:

;; Fixed bad handling of macros. Bodies of macro definitions were not
;; detected by old implementation so ugly regex hack was used in order
;; to fix it.
(defun semantic-current-tag ()
  "Return the current tag in the current buffer.
If there are more than one in the same location, return the
smallest tag.  Return nil if there is no tag here."
  (if (or (equal major-mode 'c-mode)
          (equal major-mode 'c++-mode))
      (or (car (nreverse (semantic-find-tag-by-overlay)))
          (save-excursion
            (let* ((old-pos (point))
                   (tag (or (car (nreverse (semantic-find-tag-by-overlay)))
                            (semantic-find-tag-by-overlay-prev)))
                   (real-start nil))
              (when (semantic-tag-variable-constant-p tag)
                (goto-char (semantic-tag-start tag))
                (beginning-of-visual-line)
                (setq real-start (point))
                (save-match-data
                  (when (looking-at
                         (pcre-to-elisp
                          "\\s*#define\\s(?:[^\\\n]+\\\n)*[^\n]+\n"))
                    (goto-char (match-end 0))))
                (when (>= (1- (point)) old-pos)
                  ;; Readjust the tag overlay to reflect real size.
                  (semantic-tag-set-bounds tag
                                           real-start
                                           (1- (point)))
                  tag)))))
    (car (nreverse (semantic-find-tag-by-overlay)))))


(provide 'cme-find)
;;; cme-find.el ends here
