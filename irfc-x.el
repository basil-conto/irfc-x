;;; irfc-x.el --- Extension of irfc.el

;; Copyright (C) 2015 Tsunenobu Kai

;; Author: Tsunenobu Kai <kai2nenobu@gmail.com>
;; Version: 0.0.1
;; Keywords: docs RFC IETF
;; Homepage: https://github.com/kai2nenobu/irfc-x
;; Package-Requires: ((emacs "24.1") (irfc "0.5.6"))

;; This file is not part of GNU Emacs.

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

;; This library provides `irfc-x-list' command. Its feature is to
;; - Download the index xml of RFCs automatically
;; - List all RFCs (You don't remember the number of RFC any more)
;;
;; * Requirements
;; - irfc.el
;; - Emacs compiled with libxml2 support (version 24.1 or higher)

;;; Code:

(unless (fboundp 'libxml-parse-xml-region)
  (error "irfc-x requires Emacs compiled with libxml2"))

(require 'xml)
(require 'irfc)

(defvar irfc-x--rfc-entries-cache nil)
(defconst irfc-x--index-url (concat irfc-download-base-url "rfc-index.xml"))

(defun irfc--index-file (&optional dummy)
  "Return a location of the index xml.
DUMMY is a dummy variable for `url-cache-creation-function'."
  (expand-file-name "rfc-index.xml" irfc-directory))

(defun irfc-x--ensure-index-file ()
  "Ensure that a index file exists.  If not exist, download it."
  ;; TODO: consider in offline environment
  (make-directory (file-name-directory (irfc--index-file)) :force)
  (let ((url-cache-creation-function #'irfc--index-file)) ; treat a downloaded index as cache
    (url-copy-file irfc-x--index-url (irfc--index-file) t t)))

(defun irfc-x--parse-index ()
  "Return rfc entries in a index file."
  (irfc-x--ensure-index-file)
  (with-temp-buffer
    (insert-file-contents (irfc--index-file))
    (let* ((index-tree (libxml-parse-xml-region (point-min) (point-max)))
           (rfc-entries (xml-get-children index-tree 'rfc-entry)))
      (mapc (lambda (rfc-entry)
              ;; Filter unnecessary information to reduce memory usage
              (cl-delete-if-not
               (lambda (elm) (memq (car elm) '(doc-id title)))
               (xml-node-children rfc-entry)))
            rfc-entries)
      rfc-entries)))

(defun irfc-x-rfc-entries ()
  "Return rfc entries in a index file.
If a cache doesn't exist, parse the index file."
  (or irfc-x--rfc-entries-cache
      (setq irfc-x--rfc-entries-cache (irfc-x--parse-index))))

(defun irfc-x-real-to-display (rfc-entry)
  "Convert RFC-ENTRY to a string representation."
  (concat (propertize (format "[%s]" (irfc-x--rfc-child-value rfc-entry 'doc-id))
                      'face 'font-lock-keyword-face)
          " "
          (irfc-x--rfc-child-value rfc-entry 'title)))

(defun irfc-x--rfc-child-value (rfc-entry child-name)
  (let ((nodes (xml-get-children rfc-entry child-name)))
    (unless (= (length nodes) 1)
      (error (format "Invalid rfc-entry: multiple <%s> tags" child-name)))
    (let ((values (xml-node-children (nth 0 nodes))))
      (unless (and (= (length values) 1)
                   (stringp (nth 0 values)))
        (error (format "Invalid rfc-entry: the value of <%s> must be string" child-name)))
      (nth 0 values))))

;;;###autoload
(defun irfc-x-list ()
  "Prompt all RFCs and visit one of them."
  (interactive)
  (let* ((completion-ignore-case t)
         (input (completing-read
                 "Choose one of RFC: "
                 (mapcar #'irfc-x-real-to-display (irfc-x-rfc-entries))
                 nil ; predicate
                 :require-match)))
    (if (string-match "\\[RFC\\([0-9]+\\)\\]" input)
        (irfc-visit (replace-regexp-in-string "^0*" "" ; delete leading 0
                                              (match-string 1 input)))
      (error "Invalid RFC number"))))

(provide 'irfc-x)
;;; irfc-x.el ends here
