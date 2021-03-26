;;; cme-ede-generic-proj.el --- CME generic project implementation

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

(defclass cme-generic-target (ede-target)
  ((project :initform nil
            :initarg :project)))

;;;###autoload
(defclass cme-generic-proj (ede-project)
  ())

(defun cme-generic-proj-load (dir &optional _rootproj)
  (cme-generic-proj
   :directory (file-name-as-directory dir)
   :file (expand-file-name "cme-project.json" dir)))

;;;###autoload
(ede-add-project-autoload
 (make-instance 'ede-project-autoload
                :name "CME Generic Project"
                :file 'cme-ede-generic-proj
                :proj-file "cme-project.json"
                :load-type 'cme-generic-proj-load
                :class-sym 'cme-generic-proj
                :new-p nil
                :safe-p t)
 'unique)

(cl-defmethod initialize-instance ((this cme-generic-proj)
                                   &rest fields)
  (cl-call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this targets nil)))

(cl-defmethod get-attr-from-config ((this cme-generic-proj) attr)
  (let* ((conf-path (oref this file))
         (schema '((global-includes . listp)
                   (local-includes . listp)
                   (macro-files . listp)
                   (macro-table . (null hash-table-p))
                   (source-roots . listp)
                   (build-dir . (null stringp))
                   (build-cmd . (null stringp))))
         (data (cme-read-json-cached conf-path schema))
         result)
    (if (null data)
        (error "Can't load project attributes!"))
    (cond ((symbolp attr)
           (setq result (cme-map-get (format "%s" attr)
                                     data)))
          ((stringp attr)
           (setq result (cme-map-get attr data)))
          (t (error "Bad attribute %s!" attr)))
    (if (or (string= attr "macro-table")
            (equal attr 'macro-table))
        (setq result (cme-map-to-alist result)))
    (if (or (string= attr "global-includes")
            (equal attr 'global-includes)
            (string= attr "macro-files")
            (equal attr 'macro-files)
            (string= attr "source-roots")
            (equal attr 'source-roots))
        (setq result (delete-dups result)))
    (if (or (string= attr "local-includes")
            (equal attr 'local-includes))
        (setq result (delete-dups
                      (append result
                              '("/include" "../include")))))
    result))

(cl-defmethod ede-find-subproject-for-directory ((proj cme-generic-proj) dir)
  proj)

(cl-defmethod ede-find-target ((proj cme-generic-proj) buffer)
  (let* ((targets (oref proj targets))
	     (dir default-directory)
	     (ans (object-assoc dir :path targets)))
    (when (not ans)
      (setq ans (cme-generic-target dir
                                    :name (file-name-nondirectory
			                               (directory-file-name dir))
		                            :path dir
		                            :source nil
		                            :project proj))
      (object-add-to-list proj :targets ans))
    ans))

(cl-defmethod ede-expand-filename-impl ((proj cme-generic-proj) name)
  (let ((ans (cl-call-next-method)))
    (unless ans
      (if (cme-generic-proj-header-file-p proj name)
	      (let ((ip (get-attr-from-config proj 'local-includes))
		        (tmp nil))
		    (while ip
		      (setq tmp
                    (cme-generic-proj-translate-file proj
                                                     (car ip)))
		      (setq tmp (expand-file-name name tmp))
		      (if (file-exists-p tmp)
		          (setq ans tmp))
		      (setq ip (cdr ip))))
	    (setq ans (cl-call-next-method))))
    (or ans (cl-call-next-method))))

(cl-defmethod ede-project-root ((this cme-generic-proj))
  this)

(cl-defmethod ede-project-root-directory ((this cme-generic-proj))
  (oref this directory))

(cl-defmethod cme-generic-proj-header-file-p ((proj cme-generic-proj) name)
  "Non-nil if in PROJ the filename NAME is a header."
  (save-match-data
    (string-match "\\.\\(h\\(h\\|xx\\|pp\\|\\+\\+\\)?\\|H\\)$\\|\\<\\w+$"
                  name)))

(cl-defmethod cme-generic-proj-translate-file ((proj cme-generic-proj)
                                               filename)
  (let ((dir (file-name-directory (oref proj file))))
    (if (and (not (string= filename "")) (= (aref filename 0) ?/))
	    ;; Check relative to root of project
	    (setq filename (expand-file-name (substring filename 1)
					                     dir))
      (setq filename (expand-file-name filename)))
    filename))

(cl-defmethod ede-include-path ((this cme-generic-proj))
  (get-attr-from-config this 'local-includes))

(cl-defmethod ede-system-include-path ((this cme-generic-proj))
  (get-attr-from-config this 'global-includes))

(cl-defmethod ede-source-roots ((this cme-generic-proj))
  (get-attr-from-config this 'source-roots))

(cl-defmethod ede-preprocessor-map ((this cme-generic-proj))
  (require 'semantic/db)
  (let ((spp (get-attr-from-config this 'macro-table))
	    (root (ede-project-root this)))
    (mapc
     (lambda (F)
       (let* ((expfile (ede-expand-filename root F))
	          (table (when expfile
		               ;; Disable EDE init on preprocessor file load
		               ;; otherwise we recurse, cause errs, etc.
		               (let ((ede-constructing t))
			             (semanticdb-file-table-object expfile)))))
	     (cond
	      ((not (file-exists-p expfile))
	       (message "Cannot find file %s in project." F))
	      ((string= expfile (buffer-file-name))
	       ;; Don't include this file in it's own spp table.
	       )
	      ((not table)
	       (message "No db table available for %s." expfile))
	      (t
	       (when (semanticdb-needs-refresh-p table)
	         (semanticdb-refresh-table table))
	       (setq spp (append spp (oref table lexical-table)))))))
     (get-attr-from-config this 'macro-files))
    spp))

(cl-defmethod ede-include-path ((this cme-generic-target))
  (ede-include-path (ede-target-parent this)))

(cl-defmethod ede-system-include-path ((this cme-generic-target))
  (ede-system-include-path (ede-target-parent this)))

(cl-defmethod ede-preprocessor-map ((this cme-generic-target))
  (ede-preprocessor-map (ede-target-parent this)))

(cl-defmethod project-compile-project ((proj cme-generic-proj)
                                       &optional command)
  (let ((cmd-str (or command
                     (get-attr-from-config proj 'build-cmd)
                     "make")))
    (when cmd-str
      (let ((default-directory
              (or (get-attr-from-config proj
                                        'build-dir)
                  (ede-project-root-directory proj))))
	    (compile cmd-str)))))

(cl-defmethod project-compile-target ((obj cme-generic-target)
                                      &optional command)
  (when (oref obj project)
    (project-compile-project (oref obj project) command)))

(cl-defmethod project-rescan ((this cme-generic-proj))
  (message "Nothing to rescan"))


(provide 'cme-ede-generic-proj)
;;; cme-ede-generic-proj.el ends here
