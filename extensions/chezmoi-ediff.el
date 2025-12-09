;;; chezmoi-ediff.el --- Interact with chezmoi from Emacs -*- lexical-binding: t -*-
(eval-when-compile
  (require 'ediff))

(defcustom chezmoi-ediff-force-overwrite t
  "Whether to force file overwrite when ediff finishes with identical buffers."
  :type 'boolean
  :group 'chezmoi)

(defcustom chezmoi-ediff-template-use-ediff3 t
  "If `chezmoi-ediff' between template files should .
This creates false diffs for every template element, but allows easily
changing the source template file."
  :type 'boolean
  :group 'chezmoi)

(defvar-local chezmoi-ediff--source-file nil
  "Current ediff source-file.")

(defun chezmoi-ediff--ediff-cleanup-hook ()
  (when chezmoi-ediff-force-overwrite
    (when-let* ((source-file
                 (or (with-current-buffer ediff-buffer-A chezmoi-ediff--source-file)
		     (with-current-buffer ediff-buffer-B chezmoi-ediff--source-file)))
                ((equal (with-current-buffer ediff-buffer-A (buffer-string))
		        (with-current-buffer ediff-buffer-B (buffer-string)))))
      (chezmoi-write source-file t))))

;;;###autoload
;; (defun chezmoi-ediff (file)
;;   "Choose a FILE to merge with its source using `ediff'."
;;   (interactive (list
;;                 (chezmoi--completing-read "chezmoi merge: "
;;                                           (chezmoi-modified-files)
;;                                           'project-file)))
;;   (let ((source-file (chezmoi-find file))
;;         (chezmoi-ediff--source-file source-file))
;;     (if (and chezmoi-ediff-template-use-ediff3
;;              (not (chezmoi-encrypted-p source-file))
;;              (chezmoi-template-file-p source-file))
;;         (let ((temp (make-temp-file (file-name-nondirectory file))))
;;           (with-temp-file temp
;;             (call-process "chezmoi" nil t nil
;;                           "execute-template"
;;                           (f-read-text source-file)))
;;           (ediff3 temp file source-file))
;;       ;; Patch `ediff-get-region-contents' to take into account templates in
;;       ;; source-files.
;;       (cl-flet ((ediff-get-region-contents (n buf-type ctrl-buf &optional start end)
;;                   (ediff-with-current-buffer (ediff-with-current-buffer ctrl-buf
;;                                                (ediff-get-buffer buf-type))
;;                     (let ((substr (buffer-substring
;;                                    (or start (ediff-get-diff-posn buf-type 'beg n ctrl-buf))
;;                                    (or end (ediff-get-diff-posn buf-type 'end n ctrl-buf)))))
;;                       (if (string-equal source-file (buffer-file-name))
;;                           (chezmoi-template-execute substr)
;;                         substr)))))
;;         (ediff source-file file))
;;       (add-hook 'ediff-cleanup-hook #'chezmoi-ediff--ediff-cleanup-hook nil t))))

;; (defun chezmoi--get-ancestor (source-file)
;;   "Create a temp file for the source file at git HEAD ."
;;   (let* ((relative (substring source-file (length (chezmoi-source-path))))
;;          (rev (-> (shell-command-to-string "git rev-parse --short HEAD")
;;                   (substring 0 -1)))
;;          (temp-name (expand-file-name relative (expand-file-name rev temporary-file-directory))))
;;     (make-directory (file-name-directory temp-name) t)
;;     (with-temp-file temp-name
;;       (shell-command (concat "git show " (shell-quote-argument (concat rev ":" relative)))
;;                      (current-buffer)))
;;     temp-name))
;;
;; (defun chezmoi-template--buffer (template-file)
;;   "Execute template from `TEMPLATE-FILE' and insert into a new buffer.
;; Return the new buffer."
;;   (unless (chezmoi-template-file-p template-file)
;;     (error "File: %s is not a chezmoi template file" template-file))
;;   (let ((buf (get-buffer-create (make-temp-name template-file))))
;;     (shell-command (format "%s execute-template %s"
;;                            chezmoi-command
;;                            (shell-quote-argument
;;                             (with-temp-buffer
;;                               (insert-file-contents template-file)
;;                               (buffer-string))))
;;                    buf)
;;     buf))
;;
;; ;;;###autoload
;; (defun chezmoi-ediff-merge (file)
;;   "Start an `ediff-merge-with-ancestor' session of `FILE'.
;; Merge source, target, and ancestor."
;;   (interactive (list (buffer-file-name)))
;;   (let* ((target (chezmoi-target-file-p file))
;;          (sourcef (if target
;;                       (chezmoi-source-file file)
;;                     file))
;;          (targetf (if target
;;                       file
;;                     (chezmoi-target-file file))))
;;     (unless (and sourcef targetf)
;;       (user-error "Error finding source and target files."))
;;     (ediff-merge-buffers-with-ancestor
;;      (if (chezmoi-template-file-p sourcef)
;;          (chezmoi-template--buffer sourcef)
;;        (find-file sourcef))
;;      (find-file-noselect targetf)
;;      (find-file-noselect (chezmoi--get-ancestor sourcef)))))

(provide 'chezmoi-ediff)
;;; chezmoi-ediff.el ends here
