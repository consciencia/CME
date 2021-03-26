;;; cme-analyze.el --- Overrides for semantic analyze

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

(setq *cme-select-best-tag-by-user* nil)
(defmacro cme-best-tag-user-assist-enabled (method &rest forms)
  (declare (indent 2) (debug t))
  `(let ((*cme-select-best-tag-by-user* ',method))
     ,@forms))

(defmacro cme-best-tag-user-assist-disabled (&rest forms)
  `(let ((*cme-select-best-tag-by-user* nil))
     ,@forms))

;; Added support for optional user intervention when choosing best tag.
(defun semantic-analyze-select-best-tag (sequence &optional tagclass)
  "For a SEQUENCE of tags, all with good names, pick the best one.
If SEQUENCE is made up of namespaces, merge the namespaces together.
If SEQUENCE has several prototypes, find the non-prototype.
If SEQUENCE has some items w/ no type information, find the one with a type.
If SEQUENCE is all prototypes, or has no prototypes, get the first one.
Optional TAGCLASS indicates to restrict the return to only
tags of TAGCLASS."
  ;; If there is a srew up and we get just one tag.. massage over it.
  (when (semantic-tag-p sequence)
    (setq sequence (list sequence)))
  ;; Filter out anything not of TAGCLASS
  (when tagclass
    (setq sequence
          (semantic-find-tags-by-class tagclass
                                       sequence)))
  (if (< (length sequence) 2)
      ;; If the remaining sequence is 1 tag or less, just return it
      ;; and skip the rest of this mumbo-jumbo.
      (car sequence)
    ;; 1)
    ;; This step will eliminate a vast majority of the types,
    ;; in addition to merging namespaces together.
    ;;
    ;; 2)
    ;; It will also remove prototypes.
    (require 'semantic/db-typecache)
    (setq sequence (semanticdb-typecache-merge-streams sequence nil))
    (if (< (length sequence) 2)
        ;; If the remaining sequence after the merge is 1 tag or less,
        ;; just return it and skip the rest of this mumbo-jumbo.
        (car sequence)
      (let ((best nil)
            (notypeinfo nil))
        (if *cme-select-best-tag-by-user*
            (cond
             ((equal *cme-select-best-tag-by-user*
                     'user)
              (setq best (cme-choose-tag sequence)))
             ((equal *cme-select-best-tag-by-user*
                     'type+user)
              (setq best (semantic-find-tags-by-class 'type sequence))
              (if (> (length best) 1)
                  (setq best (cme-choose-tag best))
                (setq best (car-safe best))))
             (t (error "Unknown selector %s"
                       *cme-select-best-tag-by-user*)))
          (while (and (not best) sequence)
            ;; 3) select a non-prototype.
            (if (not (semantic-tag-type (car sequence)))
                (setq notypeinfo (car sequence))
              (setq best (car sequence)))
            (setq sequence (cdr sequence))))
        ;; Select the best, or at least the prototype.
        (or best notypeinfo)))))


(provide 'cme-analyze)
;;; cme-analyze.el ends here
