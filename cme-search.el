;;; cme-search.el --- Overrides for semantic search

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

;; NOTES:
;;
;; Return eieio object with analyzed current context.
;; For inspection, use cme-inspect-eieio.
;;     (semantic-analyze-current-context)
;;
;; Return eieio object with analyzed current scope.
;; It is functional subset of semantic-analyze-current-context.
;;     (semantic-calculate-scope)
;;
;; Control tag sources when quering DB
;;     (semanticdb-find-default-throttle)
;;
;; Add preprocesor value for all projects
;;     (semantic-c-add-preprocessor-symbol "__SYM__" "VAL")
;;
;; System includes are not project includes, these apply for
;; every project (and are not saved)
;;     (semantic-add-system-include include-root-dir symbol-for-mode)
;;     (semantic-remove-system-include include-root-dir symbol-for-mode)

;; Overriden because default implementation did not flatten tag
;; hierarchy (ie. deep search). As an result, all classes and struct
;; inside namespaces were skipped from search. That sucked.
(cl-defmethod semanticdb-find-tags-subclasses-of-type-method
  ((table semanticdb-abstract-table) parent &optional tags)
  "In TABLE, find all occurrences of tags whose parent is the PARENT type.
  Optional argument TAGS is a list of tags to search.
  Returns a table of all matching tags."
  (require 'semantic/find)
  (semantic-find-tags-subclasses-of-type parent
                                         (or tags
                                             (cme-flatten-tags-table
                                              (semanticdb-get-tags table)))))

(defun cme-get-local-variables (&optional point)
  (interactive "d")
  (let ((tags (semantic-get-all-local-variables)))
    (if (called-interactively-p 'any)
        (if tags
            (cme-with-simple-pop-up "*FUNCTION VARS*"
              (setq kill-on-quit t)
              (dolist (tag tags)
                (insert (semantic-format-tag-prototype tag nil t)
                        "\n")))
          (error "Failed to find local variables!"))
      tags)))

(defun cme-get-superclasses (tag)
  (when (and (semantic-tag-p tag)
             (semantic-tag-of-class-p tag 'type)
             (or (semantic-tag-type-superclasses tag)
                 (semantic-tag-type-interfaces tag)))
    (let ((tag-buffer (semantic-tag-buffer tag))
          (tag-start (if (semantic-tag-with-position-p tag)
                         (semantic-tag-start tag))))
      (when (and tag-buffer tag-start)
        (with-current-buffer tag-buffer
          (save-excursion
            (goto-char tag-start)
            (let ((scope (semantic-calculate-scope tag-start))
                  (parents (or (semantic-tag-type-superclasses tag)
                               (semantic-tag-type-interfaces tag))))
              (loop for parent in parents
                    collect (cme-best-tag-user-assist-enabled type+user
                                (semantic-analyze-find-tag parent
                                                           'type
                                                           scope))))))))))

(defun cme-get-subclasses (tag)
  (when (and (semantic-tag-p tag)
             (semantic-tag-of-class-p tag 'type)
             (member (semantic-tag-type tag) '("class" "struct")))
    (let* ((tagname (semantic-tag-name tag))
           ;; Make sure grep backend is enabled.
           (semantic-symref-tool 'grep)
           ;; Load all files with mentioned class name into memory
           ;; together with their DBs.
           ;;
           ;; Extremely important step, otherwise only currently
           ;; loaded databases will be searched.
           (_ (semantic-symref-find-tags-by-name tagname))
           (result (semanticdb-find-tags-subclasses-of-type tagname
                                                            nil
                                                            t)))
      (semanticdb-strip-find-results result t))))

(defun cme-search-db (sym
                      &optional
                      buffer
                      find-file-match)
  (let ((tags (semanticdb-find-tags-by-name sym
                                            buffer
                                            find-file-match)))
    (if find-file-match
        (semanticdb-strip-find-results tags find-file-match)
      (semanticdb-fast-strip-find-results tags))))

(defun cme-search-db-deep (sym
                           &optional
                           buffer
                           find-file-match)
  (let ((tags (semanticdb-deep-find-tags-by-name sym
                                                 buffer
                                                 find-file-match)))
    (if find-file-match
        (semanticdb-strip-find-results tags find-file-match)
      (semanticdb-fast-strip-find-results tags))))

(defun cme-search-db-brute-deep (sym &optional find-file-match)
  (let ((tags (semanticdb-brute-deep-find-tags-by-name sym
                                                       nil
                                                       find-file-match)))
    (if find-file-match
        (semanticdb-strip-find-results tags find-file-match)
      (semanticdb-fast-strip-find-results tags))))


(defun cme-flatten-enum-tags-table (&optional table)
  (let* ((table (semantic-something-to-tag-table table))
         (lists (list table)))
    (mapc (lambda (tag)
            ;; Unroll all unnamed enums in typedefs.
            (when (and (equal (semantic-tag-class tag) 'type)
                       (equal (semantic-tag-type tag) "typedef"))
              (setq tag (semantic-tag-get-attribute tag :typedef)))
            (when (and (equal (semantic-tag-class tag) 'type)
                       (equal (semantic-tag-type tag) "enum"))
              (let ((components (semantic-tag-components tag)))
                (if (and components
                         ;; unpositioned tags can be hazardous to
                         ;; completion.  Do we need any type of tag
                         ;; here?  - EL
                         (semantic-tag-with-position-p (car components)))
                    (push (cme-flatten-enum-tags-table components)
                          lists)))))
          table)
    (apply 'append (nreverse lists))))

(defun cme-flatten-tags-table (&optional table)
  (let* ((table (semantic-something-to-tag-table table))
         (lists (list table)))
    (mapc (lambda (tag)
            ;; Optimize out function args. We are not interested in
            ;; them when processing grep hits.
            (when (not (equal (semantic-tag-class tag) 'function))
              (let ((components (semantic-tag-components tag)))
                (if (and components
                         ;; unpositioned tags can be hazardous to
                         ;; completion.  Do we need any type of tag
                         ;; here?  - EL
                         (semantic-tag-with-position-p (car components)))
                    (push (cme-flatten-tags-table components)
                          lists)))))
          table)
    (apply 'append (nreverse lists))))

(defun cme-search-file-no-includes (symbol &optional file)
  (when (not file)
    (setq file (current-buffer)))
  (with-current-buffer (if (bufferp file)
                           file
                         (find-file-noselect file))
    (save-excursion
      (let ((tags (cme-flatten-tags-table (semantic-fetch-tags))))
        (semantic-find-tags-by-name symbol tags)))))

(defun cme-get-tag-code-symbols (tag &optional collector)
  (interactive (list (semantic-current-tag)))
  (with-current-buffer (semantic-tag-buffer tag)
    (save-excursion
      (let* ((tokens (if (equal (semantic-tag-class tag)
                                'function)
                         (semantic-lex (save-excursion
                                         (goto-char (semantic-tag-start tag))
                                         (search-forward "{"))
                                       (semantic-tag-end tag)
                                       99)))
             (symbols (if collector
                          (loop for token in tokens
                                for type = (car token)
                                for range = (cdr token)
                                if (equal type 'symbol)
                                do (funcall collector
                                            (buffer-substring-no-properties
                                             (car range)
                                             (cdr range))))
                        (loop for token in tokens
                              for type = (car token)
                              for range = (cdr token)
                              if (equal type 'symbol)
                              collect (buffer-substring-no-properties
                                       (car range)
                                       (cdr range))))))
        (if (called-interactively-p)
            (cme-with-simple-pop-up "*Semantic Tag Code Symbols*"
              (setq kill-on-quit t)
              (loop for sym in symbols
                    do (insert sym "\n")))
          symbols)))))

(defun cme-get-tag-code-symbols-as-map (tag)
  (let ((map (cme-map-create))
        (collector (lambda (sym) (cme-map-set sym t map))))
    (cme-get-tag-code-symbols tag collector)
    map))

(defun cme-get-current-function-tag ()
  (let ((tag (semantic-current-tag)))
    (if (or (not tag)
            (not (equal (semantic-tag-class tag)
                        'function)))
        nil
      tag)))

(defun cme-get-current-function-name ()
  (let ((tag (cme-get-current-function-tag)))
    (if tag
        (semantic-tag-name tag)
      nil)))


(provide 'cme-search)
;;; cme-search.el ends here
