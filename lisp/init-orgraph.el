;;; init-orgraph.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(defvar orgraph-directory (expand-file-name "~/org/"))

(defvar my/org-roam-directory (expand-file-name "./roam/" orgraph-directory))

(let*
  (
    (dir_list 
      (list 
        (expand-file-name "./fleeting/" my/org-roam-directory)
        (expand-file-name "./permanent/" my/org-roam-directory)
        (expand-file-name "./literature/" my/org-roam-directory)
        (expand-file-name "./moc/" my/org-roam-directory)
      )
    )
  )
  (dolist 
    (dir dir_list)
    (unless 
      (file-exists-p dir)
      (make-directory dir t)
    )
  )
)

(require 'init-org-roam) ; org-roam及相关设置

(require 'init-org-roam-organize) ; org-roam节点整理

(require 'init-org-roam-citar) ; org-roam中citar及相关设置

(require 'init-org-latex-preview) ; org-latex-preview

(with-eval-after-load 'init-org-roam-organize
  (setq org-roam-organize/directory my/org-roam-directory)
  (setq org-roam-organize/directory-p t)
  (setq org-roam-organize/moc-directory (expand-file-name "./moc/" my/org-roam-directory))
  (setq org-roam-organize/fleeting-directory (expand-file-name "./fleeting/" my/org-roam-directory))
  (setq org-roam-organize/permanent-directory (expand-file-name "./permanent/" my/org-roam-directory))
  (setq org-roam-organize/move-target-directory (expand-file-name "./permanent/" my/org-roam-directory))
)

(with-eval-after-load 'citar
  (setq citar-notes-paths 
    (list (expand-file-name "./literature/" my/org-roam-directory))
    citar-library-paths nil
  )
)

(load-file (expand-file-name "./config/init-local.el" orgraph-directory)) ; 加载项目配置

(unless 
  (file-equal-p orgraph-directory local-directory) 
  (error "全局配置中的项目目录与局域配置中的项目目录不一致")
)

(mapc #'require '(org org-roam org-gtd consult consult-org-roam auctex vertico orderless marginalia))

(require 'init-blog-publish) ; 博客发布设置

;; (setq compile-command "emacs --batch --load ~/.emacs.d/init.el --eval \"(org-publish-all t)\"")

(add-hook 'after-init-hook #'org-roam-organize-mode)

;; 钩子
(add-hook 'before-save-hook 
  (lambda 
    ()
    (let* 
      ((post_file_list (my/blog-files)))
      (cond
        (
          (and 
            buffer-file-name
            (file-in-directory-p buffer-file-name my/org-roam-directory)
            (not (file-in-directory-p buffer-file-name (expand-file-name "./literature/" my/org-roam-directory)))
            (not (member buffer-file-name post_file_list))
          )
          (my/update-and-insert-or-not-date-in-org-file 
            orgraph-directory
            "<%Y-%m-%d %a %z>" 
            t
          )
          (message "Updated DATE in %s" (buffer-file-name))
        )
        (
          (and
            buffer-file-name
            (file-in-directory-p buffer-file-name (expand-file-name "./permanent/" my/org-roam-directory))
            (member buffer-file-name         
              (list
                (expand-file-name "sitemap.org" (expand-file-name "./roam/permanent/" my/org-blog-directory))
                (expand-file-name "style.org" (expand-file-name "./roam/permanent/" my/org-blog-directory))
              )
            )
          )
          (my/update-and-insert-or-not-date-in-org-file 
            orgraph-directory
            "<%Y-%m-%d %a %z>"
            nil
          )
        )
        (t nil)
      )
    )
  )
)

(add-hook 'after-save-hook
  (lambda 
    ()
    (let* 
      (
        (post_file_list (my/blog-files))
        (filename (buffer-file-name))
      )
      (when 
        (and
          filename
          (file-in-directory-p filename (expand-file-name "./permanent/" my/org-roam-directory))
          (member filename post_file_list)
        )
	      (org-publish-all)
        (message "[INFO] Publish finished. ")
;;        (let
;;          ((display-buffer-alist '((".*" (display-buffer-no-window)))))
;;          (message "[INFO] Publish begin. ")
;;          (compile "emacs --batch --load ~/.emacs.d/init.el --eval \"(org-publish-all)\"")
;;          (add-hook 'compilation-finish-functions
;;            (lambda 
;;              (_buffer status)
;;              (if 
;;                (string-match "exited abnormally" status)
;;                (message "[WARNING] Publish failed. Please check *compilation* buffer for details. ")
;;                (message "[INFO] Publish finished. ")
;;              )
;;              (remove-hook 'compilation-finish-functions (car (last compilation-finish-functions)))
;;            )
;;          )
;;        )
      )
    )
  )
)

;; load init-local.el
(defun load-local ()
    (interactive)
    (load-file (expand-file-name "./config/init-local.el" orgraph-directory))
)

(provide 'init-orgraph)
;;; init-orgraph.el ends here
