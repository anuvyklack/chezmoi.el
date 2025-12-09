;;; chezmoi.el --- Interact with chezmoi from Emacs -*- lexical-binding: t -*-

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
;; Chezmoi is a dotfile manager that uses a source-target state architecture.
;; This package provides convenience functions for maintaining synchronization
;; between the source and target states when making changes to your dotfiles
;; through Emacs. It provides alternatives to `find-file' and `save-buffer' for
;; source state files which maintain synchronization to the target state. It
;; also provides diff/ediff tools for resolving when dotfiles get out of sync.
;; Dired and magit integration is also provided.
;;
;;; Code:
(require 'cl-lib)
(require 'custom)
(require 'shell)
(require 'subr-x)
(require 'dash)

;;; Variables

(defgroup chezmoi nil
  "`chezmoi' package customization group."
  :group 'chezmoi)

(defvar chezmoi-source-state-prefix-attrs
  '("after_"
    "before_"
    "create_"
    "dot_"
    "empty_"
    "encrypted_"
    "exact_"
    "executable_"
    "literal_"
    "modify_"
    "once_"
    "onchange_"
    "private_"
    "readonly_"
    "remove_"
    "run_"
    "symlink_")
  "Source state attribute prefixes.")

(defvar chezmoi-source-state-suffix-attrs '(".literal" ".tmpl")
  "Source state attribute suffixes.")

(defcustom chezmoi-template-regex "{{ *\\(\\.[^[:space:]]* *\\)}}" ; (pcre-to-elisp "\\{\\{ \\.\\S+ \\}\\}")
  "Regex for detecting chezmoi templates."
  :type '(choice string regexp)
  :group 'chezmoi)

;;; Functions

(defun chezmoi-version ()
  "Return chezmoi version."
  (let ((ver-string (car (process-lines "chezmoi" "--version")))
        (regexp-stable "chezmoi version v\\([0-9]+\\.[0-9]+\\.[0-9]+\\)")
        (regexp-dev "chezmoi version \\(dev\\)"))
    (if (or (string-match regexp-stable ver-string)
            (string-match regexp-dev ver-string))
        (match-string 1 ver-string)
      (error "Cannot determine chezmoi version"))))

(defun chezmoi-version<= (ver)
  (let ((chezmoi-ver (chezmoi-version)))
    (or (string= "dev" chezmoi-ver)
        (version<= ver chezmoi-ver))))

(defun chezmoi-source-path ()
  "Path to directory with chezmoi source files."
  (-> (process-lines "chezmoi" "source-path")
      (-first-item)
      (file-name-as-directory)))

(defun chezmoi-target-path ()
  "Chezmoi target path."
  (-> (process-lines "chezmoi" "target-path")
      (-first-item)
      (file-name-as-directory)))

(defun chezmoi-managed ()
  "List with files and directories managed by chezmoi."
  (let ((target-path (chezmoi-target-path)))
    (->> (process-lines "chezmoi" "managed")
         (-map (lambda (file)
                 (file-name-concat target-path file))))))

(defun chezmoi-managed-files ()
  "List with files managed by chezmoi."
  (->> (chezmoi-managed)
       (-remove #'file-directory-p)))

(defun chezmoi-managed-p (file)
  "Return non-nil when FILE either chezmoi target of source file."
  (or (chezmoi-target-file-p file)
      (chezmoi-source-file-p file)))

(defun chezmoi-target-file-p (file)
  "Return non-nil if FILE is a chezmoi target file."
  (member (expand-file-name file)
          (chezmoi-managed-files)))

(defun chezmoi-source-file-p (file)
  "Return non-nil if FILE is a chezmoi source file."
  (string-match-p (regexp-quote (chezmoi-source-path))
                  (expand-file-name file)))

(defun chezmoi-encrypted-p (file)
  "Returns non-nil if FILE is chezmoi encrypted source file."
  (or (string-match "encrypted_" file)
      (string-match "encrypted_" (or (chezmoi-source-file file) ""))))

(defun chezmoi-template-file-p (file)
  "Returns non-nil if FILE is chezmoi template file.
Does not check if the file is managed by chezmoi."
  (string-match "\\.tmpl$" (if (chezmoi-source-file-p file)
                               file
                             (chezmoi-source-file file))))

(defun chezmoi-target-file (file)
  "Return the chezmoi target file for FILE."
  (cond ((chezmoi-target-file-p file)
         file)
        ((chezmoi-version<= "2.12.0")
         (condition-case nil
             (car (process-lines "chezmoi" "target-path" file))
           (error
            (user-error "chezmoi doesn't manage: %s" file))))
        (t
         (error "chezmoi version is too old"))))

(defun chezmoi-source-file (file)
  "Return the chezmoi source file for FILE."
  (if (chezmoi-source-file-p file)
      file
    (condition-case nil
        (car (process-lines "chezmoi" "source-path" file))
      (error
       (user-error "chezmoi doesn't manage: %s" file)))))

(defun chezmoi-modified-files ()
  "Return list of files for which target file is differ from source file."
  (let ((target-path (chezmoi-target-path)))
    (->> (process-lines "chezmoi" "status")
         (-keep (lambda (line)
                  (if (string-match "^[ ADMR]M \\(.+\\)" line)
                      (match-string 1 line))))
         (-map (lambda (file)
                 (file-name-concat target-path file))))))

(defun chezmoi-data ()
  "Return \"chezmoi data\" output."
  (-> (with-output-to-string
        (call-process "chezmoi" nil standard-output nil
                      "data"))
      (json-parse-string)))

(defun chezmoi-config ()
  "Return \"chezmoi dump-config\" output."
  (unless (chezmoi-version<= "2.27.0")
    (error "chezmoi version is too old"))
  (-> (with-output-to-string
        (call-process "chezmoi" nil standard-output nil
                      "dump-config"))
      (json-parse-string :array-type 'list
		         :null-object nil)))

;;;###autoload
(defun chezmoi-re-add ()
  "chezmoi re-add"
  (interactive)
  (if (zerop (call-process "chezmoi" nil nil nil "re-add"))
      (message "\"chezmoi re-add\" finish successfully")
    (user-error "\"chezmoi re-add\" finished with error")))

;;;###autoload
(defun chezmoi-diff ()
  "Show output of \"chezmoi diff\"."
  (interactive)
  (with-current-buffer (get-buffer-create "*chezmoi-diff*")
    (erase-buffer)
    (when (zerop (call-process "chezmoi" nil t nil
                               "diff" "--use-builtin-diff" "--no-pager"))
      (diff-mode)
      (whitespace-mode))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;;;###autoload
(defun chezmoi-write (&optional file arg)
  "Synchronize source and target FILE.
With \\[universal-argument] forcefully synchronize files."
  (interactive (list (buffer-file-name) current-prefix-arg))
  (or file (setq file (buffer-file-name)))
  (let (other-file)
    (if (zerop (cond ((chezmoi-target-file-p file)
                      (setq other-file (chezmoi-source-file file))
                      (apply #'call-process "chezmoi" nil nil nil
                             (->> (list "re-add" file (if arg "--force"))
                                  (delq nil))))
                     ((chezmoi-source-file-p file)
                      (setq other-file (chezmoi-target-file file))
                      (apply #'call-process "chezmoi" nil nil nil
                             (->> (list "apply" other-file (if arg "--force"))
                                  (delq nil))))
                     (t
                      (user-error "chezmoi doesn't manage %s" file))))
        (message "chezmoi synced with: %s" (abbreviate-file-name other-file))
      (message "chezmoit failed to sync"))))

(defun chezmoi--completing-read (prompt choices category)
  "Completing read with meta data.
PROMPT, CHOICES, and CATEGORY are passed to `complete-with-action'."
  (completing-read prompt
		   (lambda (string predicate action)
		     (if (eq action 'metadata)
			 `(metadata (category . ,category))
		       (complete-with-action action choices string predicate)))
		   nil t))

;;;###autoload
(defun chezmoi-find (file)
  "Edit a source FILE managed by chezmoi.
If the target file has the same state as the source file, add a hook to
`save-buffer' that applies the source state to the target state.  This way, when
the buffer editing the source state is saved the target state is kept in sync."
  (interactive (list
                (chezmoi--completing-read "chezmoi edit: "
			                  (chezmoi-managed-files)
			                  'project-file)))
  (let ((target-file (chezmoi-target-file file))
        (source-file (chezmoi-source-file file)))
    (find-file source-file)
    (when-let* ((mode (assoc-default target-file auto-mode-alist 'string-match)))
      (funcall (if (and (listp mode)
                        (null (car mode)))
                   (save-window-excursion
                     (let* ((existed (get-file-buffer target-file))
                            (_ (find-file target-file))
                            (mode major-mode))
                       (unless existed (kill-current-buffer))
                       mode))
	         mode)))
    (message target-file)
    (unless chezmoi-sync-mode (chezmoi-sync-mode))
    source-file))

;;;###autoload
(defun chezmoi-open-other (file)
  "Open buffer's target FILE."
  (interactive (list (buffer-file-name)))
  (cl-assert file "Current buffer does't visit file")
  (if (chezmoi-target-file-p file)
      (chezmoi-find file)
    (find-file (chezmoi-target-file file))))

(defun chezmoi-changed-p (file)
  "Return non-nil of if source and target files for FILE are diverge."
  (->> (chezmoi-target-file file)
       (expand-file-name)
       (-contains? (chezmoi-modified-files))))

;;;###autoload
(defun chezmoi-dired ()
  "Open \"chezmoi source-path\" in Dired buffer."
  (interactive)
  (dired (chezmoi-source-path)))

(defun chezmoi-font-lock-keywords ()
  "Keywords for font lock."
  `((,chezmoi-template-regex 0 'chezmoi-template-face prepend)))

;;; Ediff

;;;###autoload
(defun chezmoi-ediff (file)
  "For FILE compare target and source files in Ediff.
Traget file is in A buffer, source file in B buffer."
  (interactive (list
                (chezmoi--completing-read "chezmoi merge: "
                                          (->> (chezmoi-modified-files)
                                               (-map #'abbreviate-file-name))
                                          'project-file)))
  (ediff (chezmoi-target-file file)
         (chezmoi-source-file file)))

;;; Dired

;;;###autoload
(defun chezmoi-dired-add-marked-files ()
  "Execute \"chezmoi add\" on files marked in Dired buffer."
  (declare (interactive-only t))
  (interactive nil dired-mode)
  (dolist (file (dired-get-marked-files))
    (call-process "chezmoi" nil nil nil "add" file)))

;;; Magit

;;;###autoload
(defun chezmoi-magit-status ()
  "Open `magit-status' in the chezmoi source repository."
  (interactive)
  (magit-status-setup-buffer (chezmoi-source-path)))

;;; Minor mode

;;;###autoload
(define-minor-mode chezmoi-sync-mode
  "Minot mode to keep synchronized chezmoi source and target files."
  :global nil
  :group 'chezmoi
  (if chezmoi-sync-mode
      (progn
        (when-let* ((file (buffer-file-name))
                    ((not (chezmoi-changed-p file))))
          (add-hook 'after-save-hook #'chezmoi-write 0 t))
	;; (add-hook 'after-change-functions #'chezmoi-template--after-change nil 1)
	;; (font-lock-add-keywords nil (chezmoi-font-lock-keywords) 'append)
	;; (chezmoi-template-buffer-display t)
	;; (font-lock-ensure (point-min) (point-max))
        )
    ;; (chezmoi-template-buffer-display nil)
    (remove-hook 'after-save-hook #'chezmoi-write t)
    ;; (remove-hook 'after-change-functions #'chezmoi-template--after-change t)
    ;; (font-lock-remove-keywords nil (chezmoi-font-lock-keywords))
    ;; (font-lock-ensure (point-min) (point-max))
    ))

;;;###autoload
(defun chezmoi-maybe-enable-sync-mode ()
  "Activate `chezmoi-sync-mode' if file is managed by chezmoi."
  (when (-some-> (buffer-file-name)
          (chezmoi-managed-p))
    (unless chezmoi-sync-mode (chezmoi-sync-mode))))

;; (add-hook 'find-file-hook #'chezmoi-maybe-enable-sync-mode)

;;; .
(provide 'chezmoi)
;;; chezmoi.el ends here
