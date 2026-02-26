;;; init-org-preview.el --- Org preview configuration -*- lexical-binding: t; -*-
;;; commentary:

;;; from https://abode.karthinks.com/org-latex-preview/

;;; code:

(require 'org-latex-preview)
(setq org-latex-preview-numbered t)
(plist-put org-latex-preview-appearance-options :page-width 0.8)
(add-hook 'org-mode-hook 'org-latex-preview-mode)
(add-hook 'latex-mode-hook (lambda () (org-latex-preview-mode -1)))
(add-hook 'LaTeX-mode-hook (lambda () (org-latex-preview-mode -1)))
(setq org-latex-preview-mode-display-live t)
(setq org-latex-preview-mode-update-delay 0.25)
(defun my/org-latex-preview-uncenter (ov)
  (overlay-put ov 'before-string nil))
(defun my/org-latex-preview-recenter (ov)
  (overlay-put ov 'before-string (overlay-get ov 'justify)))
(defun my/org-latex-preview-center (ov)
  (save-excursion
    (when (derived-mode-p 'org-mode)
      (goto-char (overlay-start ov))
      (when-let* 
        (
          (elem (org-element-context))
          (
            (or 
              (eq (org-element-type elem) 'latex-environment)
              (string-match-p "^\\\\\\[" (org-element-property :value elem))
            )
          )
          (img (overlay-get ov 'display))
          (prop `(space :align-to (- center (0.55 . ,img))))
          (justify (propertize " " 'display prop 'face 'default))
        )
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify))
      )
    )
  )
)
(define-minor-mode org-latex-preview-center-mode
  "Center equations previewed with `org-latex-preview'."
  :global nil
  (if org-latex-preview-center-mode
    (progn
      (add-hook 'org-latex-preview-overlay-open-functions
              #'my/org-latex-preview-uncenter nil :local)
      (add-hook 'org-latex-preview-overlay-close-functions
              #'my/org-latex-preview-recenter nil :local)
      (add-hook 'org-latex-preview-overlay-update-functions
              #'my/org-latex-preview-center nil :local)
    )
    (remove-hook 'org-latex-preview-overlay-close-functions
                  #'my/org-latex-preview-recenter)
    (remove-hook 'org-latex-preview-overlay-update-functions
                  #'my/org-latex-preview-center)
    (remove-hook 'org-latex-preview-overlay-open-functions
                  #'my/org-latex-preview-uncenter)
  )
)
(add-hook 'org-latex-preview-mode-hook #'org-latex-preview-center-mode)

(defun my/org-latex-preview-cleanup (exit-code process-buffer processing-info)
  "生成图片后删除中间文件."
  (when (= exit-code 0)
    (let ((texfile (plist-get processing-info :texfile)))
      (when texfile
        (dolist (ext '(".aux" ".log" ".tex" ".out" ".fls" ".fdb_latexmk" ".dvi"))
          (let ((file (concat (file-name-sans-extension texfile) ext)))
            (when (file-exists-p file)
              (delete-file file))))))))
(add-hook 'org-latex-preview-process-finish-functions #'my/org-latex-preview-cleanup)

(provide 'init-org-latex-preview)
;;; init-org-latex-preview.el ends here. 
