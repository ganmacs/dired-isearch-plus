;;; dired-isearch+.el --- provide extend isearch on dired ;;; -*- lexical-binding: t -*-

;; Copyright (C) 2014-2015 ganmacs

;; Author: ganmacs <ganmacs_at_gmail.com>
;; Maintainer: ganmacs <ganmacs_at_gmail.com>
;; URL: https://github.com/ganmacs/dired-isearch-plus.el
;; Version: 0.0.1
;; Keywords: dired, isearch

;; This file is NOT part of GNU Emacs.

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

;;; Code:

(defvar dired-isearch+-status-rgex "[0-9][0-9]:?[0-9][0-9] ")
(defvar dired-isearch+-end-regex "[^ \n]+$")
(defvar dired-isearch+-prompt "dired-isearch+: %s")
(defvar dired-isearch+-quit "\C-g")
(defvar dired-isearch+-backspace "\C-h")
(defvar dired-isearch+-return "\C-m")

(defun dired-isearch+-character? (key)
  (and (>= key ?!) (<= key ?~)))

(defun dired-isearch+-enter-key? (key)
  (= key (string-to-char dired-isearch+-return)))

(defun dired-isearch+-finish-key? (key)
  (= key (string-to-char dired-isearch+-quit)))

(defun dired-isearch+-backspace-key? (key)
  (or (= key (string-to-char "\C-?"))   ; backspace
      (= key (string-to-char dired-isearch+-backspace))))

(defun dired-isearch+-continue? (key)
  (or (dired-isearch+-character? key)
      (dired-isearch+-backspace-key? key)))

(defun dired-isearch+-search-format (str)
  (concat dired-isearch+-status-rgex str dired-isearch+-end-regex))

(defun dired-isearch+-message-prompt (&optional str)
  (let ((s (if str str "")))
    (message dired-isearch+-prompt s)))

(defun dired-isearch+-after-search (key)
  (cond
   ((dired-isearch+-enter-key? key) (dired-find-file))
   ((dired-isearch+-finish-key? key) (goto-char (last point-histories)))))

;;;###autoload
(defun dired-isearch+ ()
  (interactive)
  (dired-isearch+-message-prompt)
  (let ((key (read-char)) (point-histories (list (point))) input)
    (goto-char (point-min))
    (save-match-data
      (while (dired-isearch+-continue? key)
        (cond
         ((dired-isearch+-character? key)
          (setq input (concat input (char-to-string key)))
          (beginning-of-line)
          (re-search-forward (dired-isearch+-search-format input) nil t)
          (setq point-histories (cons (point) point-histories)))
         ((dired-isearch+-backspace-key? key)
          (setq input (cond
                       ((> 2 (length input))
                        (goto-char (point-min))
                        (if (= 1 (length input)) (substring input 0 -1) input))
                       (t
                        (setq point-histories (cdr point-histories))
                        (goto-char (car point-histories))
                        (substring input 0 -1))))))
        (dired-move-to-filename)
        (dired-isearch+-message-prompt input)
        (setq key (read-char))))
    (dired-isearch+-after-search key)))

(provide 'dired-isearch+)

;; dired-isearch+.el ends here
