;;; cme-refs.el --- Overrides for semantic refs

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

;; Added more relaxed checking for types because old implementation
;; failed to infer type definition from forward type declaration.
(cl-defmethod semantic-analyze-refs-impl ((refs semantic-analyze-references)
                                          &optional
                                          in-buffer)
  "Return the implementations derived in the reference analyzer REFS.
Optional argument IN-BUFFER indicates that the returned tag should be
in an active buffer."
  (let ((allhits (oref refs rawsearchdata))
        (tag (oref refs :tag))
        (impl nil))
    (semanticdb-find-result-mapc
     (lambda (T DB)
       "Examine T in the database DB, and sort it."
       (let* ((ans (semanticdb-normalize-one-tag DB T))
              (aT (cdr ans))
              (aDB (car ans)))
         (when (and (not (semantic-tag-prototype-p aT))
                    ;; When searching for type implementations,
                    ;; semantic-tag-similar-p is too strict, so we
                    ;; need to use our simplified tag comparison.
                    (if (equal (semantic-tag-class tag) 'type)
                        (and (equal (semantic-tag-type tag)
                                    (semantic-tag-type aT))
                             (equal (semantic-tag-name tag)
                                    (semantic-tag-name aT)))
                      (semantic-tag-similar-p tag aT
                                              :prototype-flag
                                              :parent
                                              :typemodifiers
                                              :default-value)))
           (when in-buffer (save-excursion (semantic-go-to-tag aT aDB)))
           (push aT impl))))
     allhits)
    impl))

;; Overriden because old implementation did not handle multiple
;; candidates well. In fact, it just jumped to the first one. Also
;; new implementation use CME goto tag infrastructure which is more
;; robust than original way of jumping
(defun semantic-analyze-proto-impl-toggle ()
  "Toggle between the implementation, and a prototype of tag under point."
  (interactive)
  (require 'semantic/decorate)
  (semantic-fetch-tags)
  (cme-load-all-project-dbs)
  (let* ((tag (semantic-current-tag))
         (sar (if tag
                  (semantic-analyze-tag-references tag)
                (error "Point must be in a declaration")))
         (target (if (semantic-tag-prototype-p tag)
                     (cme-choose-tag
                      (semantic-analyze-refs-impl sar t))
                   (cme-choose-tag
                    (semantic-analyze-refs-proto sar t)))))
    (when (not target)
      (error "Could not find suitable %s"
             (if (semantic-tag-prototype-p tag)
                 "implementation"
               "prototype")))
    (cme-goto-tag target)))

(defalias 'cme-proto-impl-toggle 'semantic-analyze-proto-impl-toggle)


(provide 'cme-refs)
;;; cme-refs.el ends here
