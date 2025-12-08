;;; chezmoi-ediff.el --- Ediff integration for chezmoi -*- lexical-binding: t -*-

;; Author: Harrison Pielke-Lombardo
;; Maintainer: Yuriy Artemyev
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1"))
;; Homepage: http://www.github.com/anuvyklack/chezmoi.el


;; This file is not part of GNU Emacs
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;;
;; Provides `ediff' integration for `chezmoi'.
;;
;;; Code:

(require 'chezmoi)
(require 'ediff)

(defcustom chezmoi-ediff-force-overwrite t
  "Whether to force file overwrite when ediff finishes with identical buffers."
  :type '(boolean)
  :group 'chezmoi)

(defcustom chezmoi-ediff-template-use-ediff3 t
  "If `chezmoi-ediff' between template files should .
This creates false diffs for every template element, but allows easily
changing the source template file."
  :type '(boolean)
  :group 'chezmoi)

(defvar-local chezmoi-ediff--source-file nil
  "Current ediff source-file.")

(defun chezmoi-ediff--ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
  "An overriding fn for `ediff-get-region-contents'.
Converts and applies template diffs from the source-file.
N, BUF-TYPE, CTRL-BUF, START, and END are all passed to `ediff'."
  (ediff-with-current-buffer
      (ediff-with-current-buffer ctrl-buf (ediff-get-buffer buf-type))
    (if (string-equal chezmoi-ediff--source-file (buffer-file-name))
        (chezmoi-template-execute (buffer-substring-no-properties
                                   (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
                                   (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))
      (buffer-substring
       (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
       (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf))))))

(defun chezmoi-ediff--ediff-cleanup-hook ()
  (when chezmoi-ediff-force-overwrite
    (when-let (source-file (or (with-current-buffer ediff-buffer-A chezmoi-ediff--source-file)
			       (with-current-buffer ediff-buffer-B chezmoi-ediff--source-file)))
      (when (equal (with-current-buffer ediff-buffer-A (buffer-string))
		   (with-current-buffer ediff-buffer-B (buffer-string)))
	(chezmoi-write source-file t)))))

(defvar chezmoi-ediff--ediff-quit-hook ()
  (advice-remove 'ediff-get-region-contents #'chezmoi-ediff--ediff-get-region-contents))

(defun chezmoi--get-ancestor (source-file)
  "Create a temp file for the source file at git HEAD ."
  (let* ((relative (substring source-file (length chezmoi-root)))
         (rev (substring (shell-command-to-string "git rev-parse --short HEAD") 0 -1))
         (temp-name (expand-file-name relative (expand-file-name rev temporary-file-directory))))
    (make-directory (file-name-directory temp-name) t)
    (with-temp-file temp-name
      (shell-command (concat "git show " (shell-quote-argument (concat rev ":" relative)))
                     (current-buffer)))
    temp-name))

;;;###autoload
(defun chezmoi-ediff-merge (file)
  "Start an `ediff-merge-with-ancestor' session of `FILE'.
Merge source, target, and ancestor.

Note: Does not run =chezmoi merge=."
  (interactive (list (buffer-file-name)))
  (let* ((target (chezmoi-target-file-p file))
         (sourcef (if target
                    (chezmoi-source-file file)
                  file))
        (targetf (if target
                     file
                   (chezmoi-target-file file))))
    (unless (and sourcef targetf)
      (user-error "Error finding source and target files."))
    (ediff-merge-buffers-with-ancestor
     (if (chezmoi-template-file-p sourcef)
         (chezmoi-template--buffer sourcef)
       (find-file sourcef))
     (find-file-noselect targetf)
     (find-file-noselect (chezmoi--get-ancestor sourcef)))))

(defun chezmoi-template--buffer (template-file)
  "Execute template from `TEMPLATE-FILE' and insert into a new buffer.
Return the new buffer."
  (unless (chezmoi-template-file-p template-file)
    (error "File: %s is not a chezmoi template file" template-file))
  (let ((buf (get-buffer-create (make-temp-name template-file))))
        (shell-command (format "%s execute-template %s"
                       chezmoi-command
                       (shell-quote-argument
                        (with-temp-buffer (insert-file-contents template-file) (buffer-string))))
               buf)
        buf))

;;;###autoload
(defun chezmoi-ediff (file)
  "Choose a FILE to merge with its source using `ediff'.
If the current file is in `chezmoi-mode', diff the current file.
Otherwise, or if used with a prefix arg, choose from all chezmoi
managed files.

Note: Does not run =chezmoi merge=."
  (interactive
   (list (if (and chezmoi-mode (not current-prefix-arg))
             (chezmoi-target-file (buffer-file-name))
           (chezmoi--completing-read "Select a dotfile to merge: "
				   (chezmoi-changed-files)
				   'project-file))))
  (let* ((source-file (chezmoi-find file)))
      (if (and chezmoi-ediff-template-use-ediff3
               (not (chezmoi-encrypted-p source-file))
               (chezmoi-template-file-p source-file))
          (progn (let ((temp (make-temp-file (file-name-nondirectory file))))
                   (with-temp-file temp
                     (shell-command (format "%s execute-template %s"
                                            chezmoi-command
                                            (shell-quote-argument
                                             (with-temp-buffer (insert-file-contents source-file) (buffer-string))))
                                    (current-buffer)))
                (ediff3 temp file source-file)))
      (advice-add 'ediff-get-region-contents :override #'chezmoi-ediff--ediff-get-region-contents)
      (setq chezmoi-ediff--source-file source-file)
      (ediff source-file file)
      ;; (ediff-merge-files-with-ancestor source-file file (chezmoi--get-ancestor source-file) nil file)
      (add-hook 'ediff-cleanup-hook #'chezmoi-ediff--ediff-cleanup-hook nil t)
      (add-hook 'ediff-quit-hook #'chezmoi-ediff--ediff-quit-hook nil t))))

(provide 'chezmoi-ediff)
;;; chezmoi-ediff.el ends here
