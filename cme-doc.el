;;; cme-doc.el --- Overrides for semantic doc

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

;; Original implementation found first matching tag, extracted
;; documentation from it and then displayed it in simple popup.
;;
;; This implementation collects all matching tags, extract
;; documentation from all of them and then displays the best one
;; in popup with support for doxygen and live type links. Also the
;; documentation extraction process from tag was enhanced because
;; the original one was not enough generic to be able to handle
;; all styles of documentation formatting.
(defun semantic-ia-show-doc (point)
  "Display the code-level documentation for the symbol at POINT."
  (interactive "d")
  (let* ((actual-buffer (current-buffer))
         (ctxt (semantic-analyze-current-context point))
         (pf (and ctxt (reverse (oref ctxt prefix))))
         (tag (car pf)))
    (when (and (equal (length pf) 1)
               (stringp tag))
      (cme-with-disabled-grep-db
          (let ((tags (semanticdb-strip-find-results
                       (semanticdb-deep-find-tags-by-name
                        tag (current-buffer) t) t)))
            (if tags
                (setq tag (cme-choose-tag tags))))))
    (cond
     ((stringp tag)
      (message "Incomplete symbol name."))
     ((semantic-tag-p tag)
      ;; Here we are calling semantic-analyze-tag-references in order
      ;; to find either prototype or definition of current tag because
      ;; in some projects, main documentation reside above prototype
      ;; and in other, above implementation so we need to acquire
      ;; documentation for both and use the one which is more
      ;; complete. Currently indication of completeness is string
      ;; length which is probably not optimal but good enough for now.
      (let* ((sar (semantic-analyze-tag-references tag))
             (other-tags (if (semantic-tag-prototype-p tag)
                             (semantic-analyze-refs-impl sar t)
                           (semantic-analyze-refs-proto sar t)))
             (doc (or (semantic-documentation-for-tag tag) ""))
             (other-doc (or (if other-tags
                                (cme-longest-string
                                 (loop for other-tag in other-tags
                                       collect (semantic-documentation-for-tag
                                                other-tag))))
                            ""))
             (args-list-fat
              (loop for arg in (semantic-tag-function-arguments tag)
                    if (not (equal (semantic-tag-name arg) ""))
                    collect (let ((formatted-arg
                                   (semantic-format-tag-prototype arg
                                                                  nil
                                                                  t)))
                              (list (pcre-to-elisp/cached
                                     (concat "(\\W)"
                                             (semantic-tag-name arg)
                                             "(\\W)"))
                                    (concat "\\1("
                                            formatted-arg
                                            ")\\2")
                                    (cme-find-type-tags-of-var arg
                                                               actual-buffer)))))
             (args-list (loop for entry in args-list-fat
                              collect (cons (cme-at entry 0)
                                            (cme-at entry 1)))))
        (if (and (or (null doc)
                     (string= doc ""))
                 (or (null other-doc)
                     (string= other-doc "")))
            (message "Doc unavailable!")
          (cme-with-simple-pop-up (format "*Semantic DocView (%s)*"
                                          (semantic-tag-name tag))
            (setq kill-on-quit nil)
            (insert (semantic-format-tag-prototype tag nil t))
            (insert "\n")
            (insert "\n")
            (insert (if (> (length doc) (length other-doc))
                        (cme-replace-all args-list doc)
                      (cme-replace-all args-list other-doc)))
            (loop for entry in args-list-fat
                  do (cme-attach-buttons-to-label-occurences
                      (substring (cme-at entry 1) 2 -2)
                      (cme-at entry 2)))))))
     (t (message "No tag found.")))))

(defalias 'cme-doc 'semantic-ia-show-doc)

(defun cme-find-type-tags-of-var (tag &optional buffer)
  (if (not buffer)
      (setq buffer (current-buffer)))
  (cme-with-disabled-grep-db
      (force)
      (semanticdb-strip-find-results
       (semanticdb-deep-find-tags-by-name
        (if (equal (semantic-tag-class tag) 'variable)
            (let ((tag-type (semantic-tag-type tag)))
              (if (stringp tag-type)
                  tag-type
                (semantic-tag-name tag-type)))
          (error "Found invalid tag '%s' with name '%s'!"
                 (semantic-tag-class tag)
                 (semantic-tag-name tag)))
        buffer t)
       t)))

(defun cme-attach-buttons-to-label-occurences (string tags)
  (loop for (beg . end) in (cme-find-str-occurences string)
        do (progn (make-button beg end
                               'mouse-face 'custom-button-pressed-face
                               'face nil
                               'action 'cme-doc-button-handler
                               'tags tags)
                  (add-face-text-property beg end 'underline)
                  (add-face-text-property beg end 'bold))))

(defun cme-doc-button-handler (&optional button)
  (interactive)
  (let* ((tags (button-get button 'tags))
         (selected-tag (cme-choose-tag tags)))
    (cme-goto-tag selected-tag)))

(define-overloadable-function semantic-documentation-for-tag
  (&optional tag nosnarf)
  "Find documentation from TAG and return it as a clean string.
  TAG might have DOCUMENTATION set in it already.  If not, there may be
  some documentation in a comment preceding TAG's definition which we
  can look for.  When appropriate, this can be overridden by a language specific
  enhancement.
  Optional argument NOSNARF means to only return the lexical analyzer token for it.
  If NOSNARF is `lex', then only return the lex token."
  (if (not tag)
      (setq tag (semantic-current-tag)))
  (when (semantic-tag-buffer tag)
    (with-current-buffer (semantic-tag-buffer tag)
      (:override
       ;; No override.  Try something simple to find documentation nearby
       (save-excursion
         (semantic-go-to-tag tag)
         (let ((doctmp (semantic-tag-docstring tag (current-buffer))))
           (or doctmp
               (when (semantic-tag-with-position-p tag)
                 (semantic-documentation-comment-preceding-tag tag nosnarf))
               nil)))))))

(defun semantic-documentation-comment-preceding-tag (&optional tag nosnarf)
  (if (not tag)
      (setq tag (semantic-current-tag)))
  (save-excursion
    (semantic-go-to-tag tag)
    (let* ((tag-start (semantic-tag-start tag))
           ;; semantic-find-tag-by-overlay-prev
           (starttag (senator-previous-tag-or-parent tag-start))
           (start (if starttag
                      (if (cme-val-in-range tag-start starttag)
                          (semantic-tag-start starttag)
                        (semantic-tag-end starttag))
                    (point-min)))
           (stop (semantic-tag-start tag))
           (raw-comment (cme-extract-comments-from-region start stop)))
      ;; Comment cleanup. Removes comment symbols and preprocess doxygen.
      (cme-chain-forms
          (s-replace-regexp (pcre-to-elisp/cached
                             "(?:///+[ \\t]*|//-+[ \\t]*)")
                            "" raw-comment)
          (s-replace-regexp (pcre-to-elisp/cached
                             "\\A\\s*|\\s*\\Z")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "^[ \t]*")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "/\\*[ \t]*")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "[ \t]*\\*/")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "@brief[ \t]*")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "@return[ \t]*")
                            "=== return ===\n")
          (s-replace-regexp (pcre-to-elisp/cached
                             "@retval[ \t]*(\\w+)[ \t]*")
                            "Option \\1 = ")
          (s-replace-regexp (pcre-to-elisp/cached
                             "@param(?:\\[([^\\]]*)\\])?[ \t]*(\\w+)[ \t]*")
                            "Parameter[\\1] \\2 = ")
          (s-replace-regexp (pcre-to-elisp/cached
                             "^//\\s*")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "^\\*+$")
                            "")
          (s-replace-regexp (pcre-to-elisp/cached
                             "^\\*\\s*(.+)\\s*\\*$")
                            "\\1")
          (s-replace-regexp (pcre-to-elisp/cached
                             "^\\*\\s*(.+)\\s*$")
                            "\\1")
          (s-replace-regexp (pcre-to-elisp/cached
                             "(\\s)@p\\s+([^\\s\\.]+)([\\s\\.])")
                            "\\1[param: \\2]\\3")))))


(provide 'cme-doc)
;;; cme-doc.el ends here
