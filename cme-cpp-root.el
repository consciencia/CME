;;; cme-cpp-root.el --- Overrides for ede generic cpp project

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

;; Hack used for company complete c headers. EDE is not able to return
;; project local headers without this.
(cl-defmethod ede-include-path ((this ede-cpp-root-project))
  "Get the system include path used by project THIS."
  (oref this include-path))

(cl-defmethod ede-include-path ((this ede-cpp-root-target))
  "Get the system include path used by target THIS."
  (ede-include-path (ede-target-parent this)))


(provide 'cme-cpp-root)
;;; cme-cpp-root.el ends here
