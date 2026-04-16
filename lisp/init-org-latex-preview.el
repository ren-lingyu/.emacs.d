;;; init-org-latex-preview.el --- Org preview configuration -*- lexical-binding: t; -*-
;;; commentary:

;;; from https://abode.karthinks.com/org-latex-preview/

;;; code:

;; 预览
(with-eval-after-load 'org-latex-preview
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)
  (setq org-latex-preview-process-default 'dvisvgm)
  ;; (add-hook 'org-mode-hook 'org-latex-preview-mode)
  (setq org-latex-preview-mode-ignored-commands
        '(next-line previous-line mwheel-scroll
          scroll-up-command scroll-down-command))
  (setq org-latex-preview-numbered t)
  (setq org-latex-preview-mode-display-live nil)
  (setq org-latex-preview-mode-update-delay 1)
  (setq org-latex-preview-mode-track-inserts nil)
  (setq org-startup-with-latex-preview nil)
  (setq org-latex-preview-mode-ignored-environments nil)
  (setq org-latex-preview-process-precompile t)
  (setq org-latex-preview-mode nil)
  ;; (defun my/org-latex-preview-uncenter (ov)
  ;;   (overlay-put ov 'before-string nil))
  ;; (defun my/org-latex-preview-recenter (ov)
  ;;   (overlay-put ov 'before-string (overlay-get ov 'justify)))
  ;; (defun my/org-latex-preview-center (ov)
  ;;   (save-excursion
  ;;     (goto-char (overlay-start ov))
  ;;     (when-let* ((elem (org-element-context))
  ;;                 ((or (eq (org-element-type elem) 'latex-environment)
  ;;                      (string-match-p "^\\\\\\[" (org-element-property :value elem))))
  ;;                 (img (overlay-get ov 'display))
  ;;                 (prop `(space :align-to (- center (0.55 . ,img))))
  ;;                 (justify (propertize " " 'display prop 'face 'default)))
  ;;       (overlay-put ov 'justify justify)
  ;;       (overlay-put ov 'before-string (overlay-get ov 'justify)))))
  ;; (define-minor-mode org-latex-preview-center-mode
  ;;   "Center equations previewed with `org-latex-preview'."
  ;;   :global nil
  ;;   (if org-latex-preview-center-mode
  ;;       (progn
  ;;         (add-hook 'org-latex-preview-overlay-open-functions
  ;;                   #'my/org-latex-preview-uncenter nil :local)
  ;;         (add-hook 'org-latex-preview-overlay-close-functions
  ;;                   #'my/org-latex-preview-recenter nil :local)
  ;;         (add-hook 'org-latex-preview-overlay-update-functions
  ;;                   #'my/org-latex-preview-center nil :local))
  ;;     (remove-hook 'org-latex-preview-overlay-close-functions
  ;;                  #'my/org-latex-preview-recenter)
  ;;     (remove-hook 'org-latex-preview-overlay-update-functions
  ;;                  #'my/org-latex-preview-center)
  ;;     (remove-hook 'org-latex-preview-overlay-open-functions
  ;;                  #'my/org-latex-preview-uncenter)))
  )

(provide 'init-org-latex-preview)
;;; init-org-latex-preview.el ends here. 
