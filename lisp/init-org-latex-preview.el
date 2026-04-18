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
  (setq org-latex-preview-process-precompile nil) ; lualatex does not support precompile
  (setq org-latex-preview-mode nil))

(provide 'init-org-latex-preview)
;;; init-org-latex-preview.el ends here. 
