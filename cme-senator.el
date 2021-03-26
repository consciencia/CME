;;; cme-senator.el --- Overrides for semantic senator

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

(defun senator-go-to-up-reference (&optional tag)
  "Move up one reference from the current TAG.
A \"reference\" could be any interesting feature of TAG.
In C++, a function may have a `parent' which is non-local.
If that parent which is only a reference in the function tag
is found, we can jump to it.
Some tags such as includes have other reference features."
  (interactive)
  (semantic-error-if-unparsed)
  (let ((result (semantic-up-reference (or tag (semantic-current-tag)))))
    (if (not result)
        (error "No up reference found!")
      (push-mark)
      (cond
       ;; A tag
       ((semantic-tag-p result)
        (cme-goto-tag result))
       ;; Buffers
       ((bufferp result)
        (xref-push-marker-stack)
        (pop-to-buffer-same-window result))
       ;; Files
       ((and (stringp result) (file-exists-p result))
        (xref-push-marker-stack)
        (find-file result))
       (t
        (error "Unknown result type from `semantic-up-reference'"))))))

(defalias 'cme-follow-ref-up 'senator-go-to-up-reference)

(defun semantic-up-reference-default (tag)
  "Return a tag that is referred to by TAG.
Makes C/C++ language like assumptions."
  (cme-with-disabled-grep-db
      (cond ((semantic-tag-faux-p tag)
             ;; Faux tags should have a real tag in some other location.
             (require 'semantic/sort)
             (let ((options (semantic-tag-external-class tag)))
               (cme-choose-tag options)))

            ;; Include always point to another file.
            ((eq (semantic-tag-class tag) 'include)
             (let ((file (semantic-dependency-tag-file tag)))
               (cond
                ((or (not file) (not (file-exists-p file)))
                 (error "Could not locate include %s"
                        (semantic-tag-name tag)))
                ((get-file-buffer file)
                 (get-file-buffer file))
                ((stringp file)
                 file))))

            ;; Is there a parent of the function to jump to?
            ((and  (semantic-tag-of-class-p tag 'function)
                   (not (semantic-tag-get-attribute tag :prototype-flag)))
             (let* ((scope (semantic-calculate-scope (point))))
               (cme-choose-tag (oref scope parents))))

            ;; Is there a non-prototype version of the tag to jump to?
            ((semantic-tag-get-attribute tag :prototype-flag)
             (require 'semantic/analyze/refs)
             (cme-with-enabled-grep-db
                 (let* ((sar (semantic-analyze-tag-references tag))
                        (impls (semantic-analyze-refs-impl sar t)))
                   (cme-choose-tag impls))))

            ;; If this is a datatype, and we have superclasses.
            ((and (semantic-tag-of-class-p tag 'type)
                  (semantic-tag-type-superclasses tag))
             (require 'semantic/analyze)
             (let* ((scope (semantic-calculate-scope (point)))
                    (parents (semantic-tag-type-superclasses tag))
                    (parent (if (<= (length parents) 1)
                                (car-safe parents)
                              (ido-completing-read "Choose parent: "
                                                   parents))))
               (or (cme-best-tag-user-assist-enabled type+user
                       (semantic-analyze-find-tag parent 'type scope))
                   (if (y-or-n-p (format (concat "Failed to find '%s' "
                                                 "intelligently, try "
                                                 "brute force?")
                                         parent))
                       (cme-with-enabled-grep-db
                           (let* ((res (semantic-symref-find-tags-by-name
                                        parent))
                                  (tags
                                   (if res
                                       (semantic-symref-result-get-tags-as-is
                                        res t))))
                             (setq tags (semantic-find-tags-by-class 'type
                                                                     tags))
                             (if tags (cme-choose-tag tags))))))))

            ;; Get the data type, and try to find that.
            ((semantic-tag-type tag)
             (require 'semantic/analyze)
             (let ((scope (semantic-calculate-scope (point))))
               (semantic-analyze-tag-type tag scope)))

            (t nil))))


(provide 'cme-senator)
;;; cme-senator.el ends here
