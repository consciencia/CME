;;; cme-ia.el --- Overrides for semantic ia

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

;; Added no function args version of semanticdb-deep-find-tags-by-name.
;; Users generally don't want to jump to some argument of some function
;; somewhere.
(defun cme-db-deep-no-args-find-tags-by-name (name
                                              &optional
                                              path
                                              find-file-match)
  "Search for all tags matching NAME on PATH.
Search also in all components of top level tags founds.
See `semanticdb-find-translate-path' for details on PATH.
FIND-FILE-MATCH indicates that any time a match is found, the file
associated with that tag should be loaded into a buffer."
  (semanticdb-find-tags-collector
   (lambda (table tags)
     (cme-db-deep-no-args-find-tags-by-name-method table name tags))
   path find-file-match))

;; Added no function args version of semanticdb-deep-find-tags-by-name-method.
;; Needed by cme-db-deep-no-args-find-tags-by-name
(cl-defmethod cme-db-deep-no-args-find-tags-by-name-method ((table semanticdb-abstract-table)
                                                            name
                                                            &optional tags)
  "In TABLE, find all occurrences of tags with NAME.
Search in all tags in TABLE, and all components of top level tags in
TABLE.
Optional argument TAGS is a list of tags to search.
Return a table of all matching tags."
  (semantic-find-tags-by-name name
                              (cme-flatten-tags-table
                               (or tags (semanticdb-get-tags table)))))

(defun semantic-ia-fast-jump (point &optional ext-ctx)
  "Jump to the tag referred to by the code at POINT.
Uses `semantic-analyze-current-context' output to identify an accurate
origin of the code at point."
  (interactive "d")
  ;; Reparse buffer when needed.
  (semantic-fetch-tags)
  (cme-load-all-project-dbs)
  (cme-with-disabled-grep-db
      (let* ((ctxt (or ext-ctx
                       (semantic-analyze-current-context point)))
             (pf (and ctxt (reverse (oref ctxt prefix))))
             ;; In the analyzer context, the PREFIX is the list of items
             ;; that makes up the code context at point.  Thus the c++ code
             ;; this.that().theothe
             ;; would make a list:
             ;; ( ("this" variable ..) ("that" function ...) "theothe")
             ;; Where the first two elements are the semantic tags of the prefix.
             ;;
             ;; PF is the reverse of this list.  If the first item is a string,
             ;; then it is an incomplete symbol, thus we pick the second.
             ;; The second cannot be a string, as that would have been an error.
             (pf-len (length pf))
             (first (car pf))
             (second (nth 1 pf)))
        (cond
         ((semantic-tag-p first)
          ;; We have a match.  Just go there.
          (semantic-ia--fast-jump-helper first))
         ((semantic-tag-p second)
          ;; Because FIRST failed, we should visit our second tag.
          ;; HOWEVER, the tag we actually want that was only an unfound
          ;; string may be related to some take in the datatype that belongs
          ;; to SECOND.  Thus, instead of visiting second directly, we
          ;; can offer to find the type of SECOND, and go there.
          (let ((secondclass (car (reverse (oref ctxt prefixtypes)))))
            (cond
             ((and (semantic-tag-with-position-p secondclass)
                   (y-or-n-p (format-message
                              "Could not find `%s'.  Jump to %s? "
                              first (semantic-tag-name secondclass))))
              (semantic-ia--fast-jump-helper secondclass))
             ;; If we missed out on the class of the second item, then
             ;; just visit SECOND.
             ((and (semantic-tag-p second)
                   (y-or-n-p (format-message
                              "Could not find `%s'.  Jump to %s? "
                              first (semantic-tag-name second))))
              (semantic-ia--fast-jump-helper second)))))
         ((semantic-tag-of-class-p (semantic-current-tag) 'include)
          ;; Just borrow this cool fcn.
          (require 'semantic/decorate/include)
          ;; Push the mark, so you can pop global mark back, or
          ;; use semantic-mru-bookmark mode to do so.
          (xref-push-marker-stack)
          (semantic-decoration-include-visit))
         ((and (equal pf-len 1)
               (stringp first))
          ;; Lets try to handle enums and macros. These things are ignored
          ;; by context analyzer so we must do it here.
          (cme-with-disabled-grep-db
              (let* ((tags (semanticdb-strip-find-results
                            (cme-db-deep-no-args-find-tags-by-name
                             first
                             (current-buffer)
                             t)
                            t)))
                (if tags
                    (cme-goto-tag (cme-choose-tag tags))
                  ;; Last regard, use brute force.
                  (cme-find-anything first)))))
         ;; Semantical analysis failed, there's not much to do now.
         ;; Just use brute force and let user do the filtering.
         ((stringp first)
          (cme-find-anything first))
         (t (error "Don't know what to do here, sorry..."))))))

(defalias 'cme-jump 'semantic-ia-fast-jump)

(defun semantic-ia--fast-jump-helper (dest)
  "Jump to DEST, a Semantic tag.
This helper manages the mark, buffer switching, and pulsing."
  ;; We have a tag, but in C++, we usually get a prototype instead
  ;; because of header files.  Let's try to find the actual
  ;; implementation instead.
  (when (semantic-tag-prototype-p dest)
    (cme-with-enabled-grep-db
        (let* ((refs (semantic-analyze-tag-references dest))
               (impl (semantic-analyze-refs-impl refs t)))
          (when impl (setq dest (cme-choose-tag impl))))))
  ;; Make sure we have a place to go...
  (if (not (and (or (semantic-tag-with-position-p dest)
                    (semantic-tag-get-attribute dest :line))
                (semantic-tag-file-name dest)))
      (error "Tag %s has no buffer information"
             (semantic-format-tag-name dest)))
  (cme-goto-tag dest))

(defun cme-find-subclasses (&optional tag)
  (interactive)
  (semantic-fetch-tags)
  (cme-load-all-project-dbs)
  (let ((curr-tag (or tag (semantic-current-tag)))
        (curr-parent-tag (semantic-current-tag-parent))
        (class-tag nil))
    (cond ((and curr-tag
                (semantic-tag-of-class-p curr-tag 'type)
                (member (semantic-tag-type curr-tag)
                        '("class" "struct")))
           (setq class-tag curr-tag))
          ((and curr-parent-tag
                (semantic-tag-of-class-p curr-parent-tag 'type)
                (member (semantic-tag-type curr-parent-tag)
                        '("class" "struct")))
           (setq class-tag curr-parent-tag))
          ((and curr-tag
                (equal (semantic-tag-class curr-tag) 'function))
           (setq class-tag (semantic-up-reference curr-tag)))
          ((and curr-parent-tag
                (semantic-tag-of-class-p curr-parent-tag 'function))
           (setq class-tag (semantic-up-reference curr-parent-tag)))
          (t
           (if curr-tag
               (error "Failed to find parent class of current tag!")
             (error "Failed to find parent class of current context!"))))
    (if (and (semantic-tag-of-class-p class-tag 'type)
             (member (semantic-tag-type class-tag) '("class" "struct")))
        (let ((tags (cme-get-subclasses class-tag)))
          (if tags
              (cme-goto-tag (cme-choose-tag tags))
            (error "Failed to find children of '%s'!"
                   (semantic-tag-name class-tag))))
      (error "Failed to find parent class of current tag!"))))

(defun cme-ctxt-current-symbol-and-bounds-detached (expr)
  (let ((parent-s-table semantic-lex-syntax-table))
    (with-temp-buffer
      (setq semantic-lex-syntax-table parent-s-table)
      (insert expr)
      (semantic-ctxt-current-symbol-and-bounds))))

(defun cme-analyze-current-context-detached (expr
                                             &optional position)
  (when (null position)
    (setq position (point)))
  (let* ((semantic-analyze-error-stack nil)
         (prefixandbounds
          (cme-ctxt-current-symbol-and-bounds-detached expr))
         (prefix (car prefixandbounds))
         (bounds (nth 2 prefixandbounds))
         ;; @todo - vv too early to really know this answer! vv
         (prefixclass '(function variable type))
         (prefixtypes nil)
         (scope (semantic-calculate-scope position))
         newseq)
    ;; Only do work if we have bounds (meaning a prefix to complete)
    (when bounds
      (if debug-on-error
          (catch 'unfindable
            (setq prefix (semantic-analyze-find-tag-sequence
                          prefix scope 'prefixtypes 'unfindable))
            ;; If there's an alias, dereference it and analyze
            ;; sequence again.
            (when (setq newseq
                        (semantic-analyze-dereference-alias prefix))
              (setq prefix (semantic-analyze-find-tag-sequence
                            newseq scope 'prefixtypes 'unfindable))))
        ;; Debug on error is off.  Capture errors and move on
        (condition-case err
            ;; NOTE: This line is duplicated in
            ;;       semantic-analyzer-debug-global-symbol
            ;;       You will need to update both places.
            (progn
              (setq prefix (semantic-analyze-find-tag-sequence
                            prefix scope 'prefixtypes))
              (when (setq newseq
                          (semantic-analyze-dereference-alias prefix))
                (setq prefix
                      (semantic-analyze-find-tag-sequence newseq
                                                          scope
                                                          'prefixtypes))))
          (error (semantic-analyze-push-error err))))
      (semantic-analyze-context "context"
                                :buffer (current-buffer)
                                :scope scope
                                :bounds bounds
                                :prefix prefix
                                :prefixclass prefixclass
                                :prefixtypes prefixtypes
                                :errors semantic-analyze-error-stack))))

(defun cme-find-anything (sym)
  (interactive (list (cme-with-disabled-grep-db
                         (cme-semantic-complete-read-tag-project
                          "Look for symbol: "
                          nil
                          (thing-at-point 'symbol)))))
  (if (> (length
          (car (cme-ctxt-current-symbol-and-bounds-detached sym)))
         1)
      (let ((ctx (cme-best-tag-user-assist-enabled type+user
                     (cme-analyze-current-context-detached sym))))
        (if ctx
            (cme-jump (point) ctx)
          (error "Failed to construct detached context")))
    ;; Use modified symref module for getting all tags with target name in
    ;; current project and all its dependencies.
    ;; Bypasses bug when brute deep searching all tables in project
    ;; using standard semantic find routines.
    ;;
    ;; NOTE: That bug was probably caused by GNU Global which was fired
    ;; because it SUCKED.
    (cme-with-enabled-grep-db
        (let ((tags (or (let ((res (semantic-symref-find-tags-by-name sym)))
                          (if res
                              (semantic-symref-result-get-tags-as-is res t)))
                        ;; When symrefing fails, it means it might be in system
                        ;; dependencies, use full scope search then.
                        (cme-best-tag-user-assist-enabled user
                            (semantic-analyze-find-tag sym))
                        ;; Still nothing? Maybe it is in local variables.
                        (semantic-find-tags-by-name
                         sym
                         (cme-get-local-variables)))))
          (when (semantic-tag-p tags)
            (setq tags (list tags)))
          (if tags
              (let* ((chosen-tag (cme-choose-tag tags)))
                (cme-goto-tag chosen-tag))
            (message "No tags found for %s" sym))))))

(defvar-local *cme-is-prefix-pointer-state* (cons nil 'allow))
(defun cme-is-prefix-pointer-p (&optional point)
  (interactive "d")
  (if (not point)
      (setq point (point)))
  (when (and (not (cme-pos-is-in-comment point))
             (not (cme-pos-is-in-string point))
             (semantic-current-tag)
             (equal (semantic-tag-class (semantic-current-tag))
                    'function)
             (not (semantic-tag-prototype-p (semantic-current-tag))))
    (let* ((guess-if-pointer
            (lambda (sym)
              (if sym
                  (let ((case-fold-search nil))
                    (s-match (pcre-to-elisp/cached "^p[A-Z].*") sym)))))
           (ctx-sym (cadr (semantic-ctxt-current-symbol-and-bounds point)))
           (curr-fun-name (cme-get-current-function-name))
           (decision (not
                      (and
                       (equal curr-fun-name
                              (car *cme-is-prefix-pointer-state*))
                       (not (equal (cdr *cme-is-prefix-pointer-state*)
                                   'allow)))))
           (ctx-with-time
            (cme-with-measure-time
                (if decision
                    (ignore-errors
                      (cme-with-disabled-grep-db
                          (semantic-analyze-current-context point))))))
           (ctx (car ctx-with-time))
           (run-time (cdr ctx-with-time))
           (prefix (if ctx (oref ctx prefix) nil))
           (last-pref (car (last prefix))))
      (when (> run-time 20)
        (setq *cme-is-prefix-pointer-state*
              (cons curr-fun-name 'no-ctx-fetch))
        (when (yes-or-no-p
               (format
                (concat "Context fetch was too slow (%s) so it was "
                        "disabled temporary for this function (%s). Do "
                        "you want to enable type guessing (pSomething "
                        "is considered as a pointer)?")
                run-time
                curr-fun-name))
          (setq *cme-is-prefix-pointer-state*
                (cons curr-fun-name 'guess))))
      (if last-pref
          (if (semantic-tag-p last-pref)
              (semantic-tag-get-attribute last-pref
                                          :pointer)
            (if (equal (cdr *cme-is-prefix-pointer-state*)
                       'guess)
                (funcall guess-if-pointer ctx-sym)))
        (if (equal (cdr *cme-is-prefix-pointer-state*)
                   'guess)
            (funcall guess-if-pointer ctx-sym))))))

(defun cme-choose-tag (tags &optional no-error)
  (when (and (not no-error)
             (equal (length tags) 0))
    (error "No tags provided for selection!"))
  (setq tags (loop for tag in tags
                   collect (semantic-tag-copy tag nil t)))
  (if (equal (length tags) 1)
      (car tags)
    (when (not (equal (length tags) 0))
      (let* ((tag-summary-f
              (lambda (tag)
                (format "%s = %s"
                        (let* ((str
                                (concat
                                 (if (semantic-tag-prototype-p tag)
                                     "[prototype] "
                                   "")
                                 (semantic-format-tag-prototype tag nil t)))
                               (real-len (length str))
                               (limit-len 97))
                          (if (> real-len limit-len)
                              (concat (substring str 0 limit-len)
                                      "...")
                            str))
                        (let* ((str (buffer-file-name
                                     (semantic-tag-buffer tag)))
                               (real-len (length str))
                               (limit-len 77))
                          (if (> real-len limit-len)
                              (concat "..."
                                      (substring str (- real-len
                                                        limit-len)
                                                 real-len))
                            str)))))
             (get-tag-by-summary-f (lambda (summary summaries)
                                     (loop for (summary2 tag) in summaries
                                           if (equal summary2 summary)
                                           return tag)))
             (seen-summaries (cme-map-create))
             (summaries
              (loop for tag in tags
                    for name = (semantic-tag-name tag)
                    for summary = (funcall tag-summary-f tag)
                    unless (prog1
                               (cme-map-get summary seen-summaries)
                             (cme-map-set summary t seen-summaries))
                    collect (list summary tag)))
             (chosen-summary
              (if (equal (length summaries) 1)
                  (caar summaries)
                (ido-completing-read "Choose tag: "
                                     (loop for summary in summaries
                                           collect (car summary)))))
             (chosen-tag (funcall get-tag-by-summary-f
                                  chosen-summary
                                  summaries)))
        chosen-tag))))

(defun cme-goto-tag (tag)
  (cme-push-mark)
  (find-file (buffer-file-name (semantic-tag-buffer tag)))
  (semantic-go-to-tag tag)
  (if (not semantic-idle-scheduler-mode)
      (semantic-idle-scheduler-mode))
  (semantic-refresh-tags-safe)
  ;; Maybe database was from some reason outdated, we need to somehow
  ;; hide that from user and land in correct location.
  (let* ((curr-pos (point))
         (curr-name (semantic-tag-name tag))
         (enclosing-tag (semantic-current-tag))
         (enclosing-tag-name (if enclosing-tag
                                 (semantic-tag-name enclosing-tag)))
         (expected-pos (if enclosing-tag
                           (semantic-tag-start enclosing-tag)))
         (candidate-tags nil)
         (nearest-tag nil)
         (nearest-tag-distance 0)
         (temp-distance 0)
         (temp-distance2 0))
    (cond ((and (equal curr-pos expected-pos)
                (equal curr-name enclosing-tag-name))
           ;; All is ok.
           )
          ;; Local variables have no overlays thus no enclosing tag can
          ;; be found for them. In order to correctly detect valid
          ;; landing for local variables, we must iterate through all local
          ;; variables in current scope and check if one of them match
          ;; to visited tag.
          ((loop for var-tag in (semantic-get-local-variables)
                 if (and (equal curr-pos (semantic-tag-start var-tag))
                         (equal curr-name (semantic-tag-name var-tag)))
                 return t)
           ;; All is ok.
           )
          ((equal curr-name enclosing-tag-name)
           ;; We are not exactly ok, but we landed somehow somewhere
           ;; in correct tag, just readjust.
           (goto-char expected-pos)
           (setq tag enclosing-tag))
          (t
           ;; All is wrong, we landed completely somewhere else.
           ;; As an fix, list all tags with same name and jump to
           ;; nearest one.
           (setq candidate-tags
                 (cme-search-file-no-includes curr-name))
           (loop for candidate-tag in candidate-tags
                 do (progn
                      (setq temp-distance
                            (abs (- curr-pos
                                    (semantic-tag-start candidate-tag)))
                            temp-distance2
                            (abs (- curr-pos
                                    (semantic-tag-end candidate-tag))))
                      (when (< temp-distance2 temp-distance)
                        (setq temp-distance temp-distance2))
                      (if nearest-tag
                          (when (<= temp-distance nearest-tag-distance)
                            (setq nearest-tag-distance temp-distance
                                  nearest-tag candidate-tag))
                        (setq nearest-tag-distance temp-distance
                              nearest-tag candidate-tag))))
           (if nearest-tag
               (setq tag nearest-tag)
             (setq tag nil))
           (if nearest-tag
               ;; We found tag with same name which is nearest from
               ;; our position of arrival.
               (goto-char (semantic-tag-start nearest-tag))
             ;; We failed to find nearest tag. Not good. We must
             ;; resort to solution based on search-forward and search-backward.
             ;; Choose the nearest.
             (let ((f-pos (save-excursion (search-forward curr-name nil t)))
                   (b-pos (save-excursion (search-backward curr-name nil t))))
               (cond ((and f-pos b-pos)
                      (if (< (abs (- curr-pos f-pos))
                             (abs (- curr-pos b-pos)))
                          (goto-char f-pos)
                        (goto-char b-pos)))
                     (f-pos
                      (goto-char f-pos))
                     (b-pos
                      (goto-char b-pos))
                     (t
                      ;; Sorry, DB had really shitty data.
                      (error (concat "Invalid DB record was "
                                     "detected and recovery "
                                     "failed!")))))))))
  (recenter)
  (when tag
    (pulse-momentary-highlight-region (semantic-tag-start tag)
                                      (semantic-tag-end tag))))


(provide 'cme-ia)
;;; cme-ia.el ends here
