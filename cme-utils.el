;;; cme-utils.el --- CME utilities

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

(defun cme-map-create ()
  (make-hash-table :test 'equal))

(defun cme-map-get (key map)
  (gethash key map))

(defun cme-map-set (key val map)
  (puthash key val map))

(defun cme-map-rem (key map)
  (remhash key map))

(defun cme-map-everything (fun list)
  (cond
   ((listp list)
    (loop for x in list collect (funcall fun x)))
   ((hash-table-p list)
    (loop for key being the hash-keys of list using (hash-values val)
          collect (funcall fun key val)))
   (t (error "Bad input to cme-map-everything"))))

(defun cme-map-to-alist (map)
  (if (hash-table-p map)
      (let ((acc nil))
        (maphash (lambda (k v)
                   (setq acc (cons (cons k v) acc)))
                 map)
        acc)
    nil))

(defun cme-at (list idx)
  (cond
   ((listp list) (nth idx list))
   ((hash-table-p list) (gethash idx list))
   (t (error "Bad input to cme-at"))))

(defun cme-delete-dups-eq (list)
  (let ((hash (make-hash-table :test #'eq :size (length list)))
        (tail list) retail)
    (puthash (car list) t hash)
    (while (setq retail (cdr tail))
      (let ((elt (car retail)))
        (if (gethash elt hash)
            (setcdr tail (cdr retail))
          (puthash elt t hash)
          (setq tail retail)))))
  list)

(defun cme-list-init (list &optional unsafe)
  (if unsafe
      (nreverse (nthcdr 1 (nreverse list)))
    (nreverse (nthcdr 1 (reverse list)))))

(defun cme-append-new-backbone (&rest lists)
  (let ((result nil))
    (dolist (list lists)
      (dolist (element list)
        (push element result)))
    result))

(defun cme-longest-string (strings)
  (let ((longest (car strings))
        (head (cdr strings)))
    (while head
      (when (> (length (car head))
               (length longest))
        (setq longest (car head)))
      (setq head (cdr head)))
    longest))

;; Function s-replace-all not works correctly for all inputs
;; in all versions of emacs. This is workaround for that.
;; This function also handle correctly empty replacement list.
(defun cme-replace-all (replacements str)
  (loop for (from . to) in replacements
        do (setq str (s-replace-regexp from to str)))
  str)

(defun cme-find-str-occurences (string)
  (save-excursion
    (let ((len (length string))
          (result nil))
      (goto-char (point-min))
      (while (search-forward string nil 0)
        (push (cons (- (point) len) (point)) result))
      result)))

(defun cme-val-in-range (val range)
  (when (semantic-tag-p range)
    (setq range
          (cons (semantic-tag-start range)
                (semantic-tag-end range))))
  (and (>= val (car range))
       (<= val (cdr range))))

(defmacro cme-chain-forms (&rest forms)
  (declare (indent 99))
  (apply #'cme-chain-forms-helper (nreverse forms)))

(defun cme-chain-forms-helper (&rest forms)
  (if forms
      (append (car forms)
              (if (cdr forms)
                  (list (apply #'cme-chain-forms-helper
                               (cdr forms)))))))

(defun cme-get-buffer (name)
  (loop for buff in (buffer-list)
        for buffname = (buffer-name buff)
        if (string= buffname name)
        return (cons buffname buff)))

(defun cme-pos-is-in-comment (&optional pos)
  (interactive)
  (if (not pos)
      (setq pos (point)))
  (let ((fontfaces (get-text-property pos 'face))
        (comment-faces '(font-lock-comment-face
                         font-lock-comment-delimiter-face))
        (result nil))
    (when (not (listp fontfaces))
      (setf fontfaces (list fontfaces)))
    (setq result
          (if font-lock-mode
              (loop for font-face in fontfaces
                    if (memq font-face comment-faces)
                    return t)
            (nth 4 (syntax-ppss pos))))
    (if (called-interactively-p 'any)
        (message "Comment state: %s" result)
      result)))

(defun cme-pos-is-in-string (&optional pos)
  (if (not pos)
      (setq pos (point)))
  (if font-lock-mode
      (eq (get-text-property pos 'face)
          'font-lock-string-face)
    (nth 3 (syntax-ppss pos))))

(defun cme-extract-comments-from-region (start stop)
  (when font-lock-mode
    ;; There is possibility that target region is in buffer with font lock
    ;; enabled but with no font locking done yet. In such case, we must
    ;; explicitly fontify that region where we search for comments.
    ;; Of course there is backup for buffers without font locking, but
    ;; this backup is enabled only when font lock mode is disabled so
    ;; its not usable in this situation.
    (font-lock-fontify-region start stop))
  (let ((result "")
        (len (- stop start))
        (finger start)
        (was-in nil)
        (is-in nil))
    (dotimes (finger len result)
      (setq is-in (cme-pos-is-in-comment (+ start finger)))
      (if is-in
          (setq result (concat result
                               (string
                                (char-after (+ start
                                               finger))))))
      (if (and was-in (not is-in))
          (setq result (concat result
                               (if (equal (string
                                           (aref result
                                                 (1- (length result)))) "\n")
                                   "\n"
                                 "\n\n"))))
      (setq was-in is-in))))

(defun cme-check-schema (obj schema)
  (let ((result t))
    (loop for entry in schema
          for field = (format "%s" (car entry))
          for val = (cme-map-get field obj)
          for predicates = (if (listp (cdr entry))
                               (cdr entry)
                             `(,(cdr entry)))
          for predicates-output = (loop for p in predicates
                                        collect (funcall p val))
          do (progn
               (if (not (eval `(or ,@predicates-output)))
                   (setq result nil))))
    (when (not result)
      (message "Schema check failed!"))
    result))

(setq *cme-read-json-file-cache* (cme-map-create))
(setq *cme-read-json-ts-cache* (cme-map-create))
(defun cme-read-json-cached (path &optional schema)
  (let ((json-object-type 'hash-table)
        (json-array-type 'list)
        (json-key-type 'string)
        ts
        result)
    (if (file-exists-p path)
        (progn
          (setq ts (file-attribute-modification-time
                    (file-attributes path)))

          (if (equal ts
                     (cme-map-get path
                                  *cme-read-json-ts-cache*))
              (setq result
                    (cme-map-get path
                                 *cme-read-json-file-cache*))
            (progn
              (setq result (json-read-file path))
              (cme-map-set path
                           result
                           *cme-read-json-file-cache*)
              (cme-map-set path
                           ts
                           *cme-read-json-ts-cache*)))
          (if (and result
                   schema
                   (not (cme-check-schema result schema)))
              (setq result nil)))
      (progn
        (cme-map-rem path *cme-read-json-file-cache*)
        (cme-map-rem path *cme-read-json-ts-cache*)))
    (when (null result)
      (message "Failed to read JSON from %s!"
               path))
    result))

(defun cme-mark-string ()
  (interactive)
  (ignore-errors
    (when (eq (get-text-property (point) 'face)
              'font-lock-string-face)
      (let ((p (point)))
        (while (eq (get-text-property (point) 'face)
                   'font-lock-string-face)
          (forward-char 1))
        (skip-chars-backward " \n\t\r")
        (set-mark (point))
        (goto-char p)
        (while (eq (get-text-property (point) 'face)
                   'font-lock-string-face)
          (forward-char -1))
        (forward-char 1)
        (skip-chars-forward " \n\t\r"))))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defun cme-mark-tag ()
  (interactive)
  (if (eq (get-text-property (point) 'face)
          'font-lock-string-face)
      (cme-mark-string)
    (if (semantic-current-tag)
        (let* ((tag (semantic-current-tag))
               (bounds (semantic-tag-bounds tag))
               (b (car bounds))
               (e (cadr bounds)))
          (goto-char e)
          (set-mark (point))
          (goto-char b))))
  (setq transient-mark-mode (cons 'only transient-mark-mode)))

(defmacro cme-with-simple-pop-up (name &rest content)
  (declare (indent 1))
  (let ((buffsym (gensym)))
    `(let ((,buffsym (cme-get-buffer ,name))
           (kill-on-quit nil))
       (if ,buffsym
           (setq ,buffsym (cdr ,buffsym))
         (setq ,buffsym (generate-new-buffer ,name)))
       (with-current-buffer ,buffsym
         (read-only-mode -1)
         (erase-buffer)
         ,@content
         (read-only-mode t))
       (switch-to-buffer-other-window ,name)
       (shrink-window-if-larger-than-buffer)
       (goto-char (point-min))
       (if kill-on-quit
           (local-set-key "q"
                          (lambda ()
                            (interactive)
                            (delete-window)
                            (kill-buffer ,name)))
         (local-set-key "q" 'delete-window)))))

(defmacro cme-with-measure-time (&rest body)
  (declare (indent 99))
  `(let ((time (current-time)))
     (cons (progn
             ,@body)
           (float-time (time-since time)))))

(defun cme-push-mark ()
  (interactive)
  (xref-push-marker-stack))

(defun cme-pop-mark ()
  (interactive)
  (deactivate-mark t)
  (let* ((buff (current-buffer))
         (win (selected-window))
         (start (window-start win))
         (end (window-end win)))
    (xref-pop-marker-stack)
    (if (not (and (eq buff (current-buffer))
                  (>= (point) start)
                  (<= (point) end)))
        (recenter))
    (pulse-momentary-highlight-one-line (point))))

(defun cme-inspect-eieio (obj)
  (require 'eieio-datadebug)
  (data-debug-new-buffer "*Inspector*")
  (data-debug-insert-object-slots obj ">>"))


(provide 'cme-utils)
;;; cme-utils.el ends here
