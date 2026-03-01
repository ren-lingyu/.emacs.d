;;; init-org-roam.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; org-roam
(use-package org-roam
  :defer
  ;; :demand t
  :after (org)
  :custom
  (org-roam-directory (expand-file-name "./roam/" org-directory))   ;; 你的笔记根目录
  (org-roam-db-location (expand-file-name "./roam/.org-roam.db" org-directory)) ;; 数据库文件位置
  (org-roam-dailies-directory "journal/")
  (org-roam-completion-everywhere t)
  (org-roam-node-display-template "${title} ${tags}")
  (org-roam-db-update-method 'immediate) 
  :bind 
  (("C-c n f" . org-roam-node-find)
    ("C-c n i" . org-roam-node-insert)
    ("C-c n l" . org-roam-buffer-toggle)
    ("C-c n c" . org-roam-capture))
  :config
  (org-roam-db-autosync-mode)
  ;; (advice-add 'org-roam-mode-hook :after
  ;;   (lambda (&rest _)
  ;;     (display-line-numbers-mode 1)
  ;;     (font-lock-mode 1)
  ;;   )
  ;; )
  (setq org-roam-node-display-template
    (concat 
      "${title:*} "
      (propertize "${tags:30}" 'face 'org-tag)))
  (setq org-roam-capture-templates
    (append org-roam-capture-templates
      `(
        ("f" "fleeting" plain "%?"
          :if-new 
          (file+head  
            "fleeting/${slug}.org"
            ,(concat
              "#+TITLE: ${title}\n"
              "#+AUTHOR: Lingyu Ren\n"
              "#+DATE: " (format-time-string "%Y-%m-%d %A %H:%M:%S %z") "\n"
              "#+FILETAGS: :idea:\n"
              "#+DESCRIPTION:\n"
            ))
          :unnarrowed t)
        ("p" "permanent" plain "%?"
          :if-new 
          (file+head  
            "permanent/${id}/${slug}.org"
            ,(concat
              "#+TITLE: ${title}\n"
              "#+AUTHOR: Lingyu Ren\n"
              "#+DATE: " (format-time-string "%Y-%m-%d %A %H:%M:%S %z") "\n"
              "#+FILETAGS: :zettel:\n"
              "#+DESCRIPTION:\n"
            ))
          :unnarrowed t)
        ("I" "post index" plain "%?"
          :if-new 
          (file+head  
          "permanent/${slug}.org"
            ,(concat
              "#+TITLE: ${title}\n"
              "#+INDEX: ${title}\n"
              "#+DESCRIPTION:\n"
              "#+AUTHOR: aRenCoco\n"
              "#+EMAIL: aRen_Coco@outlook.com\n"
              "#+DATE: " (format-time-string "<%Y-%m-%d %a %z>") "\n"
              "#+FILETAGS: :zettel:blog:\n"
            ))
          :unnarrowed t)
        ("P" "post article" plain "%?"
          :if-new 
          (file+head  
            "permanent/${id}/${slug}.org"
            ,(concat
              "#+TITLE: ${title}\n"
              "#+INDEX: ${title}\n"
              "#+AUTHOR: aRenCoco\n"
              "#+EMAIL: aRen_Coco@outlook.com\n"
              "#+DATE: " (format-time-string "<%Y-%m-%d %a %z>") "\n"
              "#+FILETAGS: :zettel:blog:post:\n"
            ))
          :unnarrowed t)
        ("D" "draft article" plain "%?"
          :if-new 
          (file+head  
          "permanent/${id}/${slug}.org"
            ,(concat
              "#+TITLE: ${title}\n"
              "#+INDEX: ${title}\n"
              "#+AUTHOR: aRenCoco\n"
              "#+EMAIL: aRen_Coco@outlook.com\n"
              "#+DATE: " (format-time-string "<%Y-%m-%d %a %z>") "\n"
              "#+FILETAGS: :zettel:blog:\n"
            ))
          :unnarrowed t)))))

(use-package org-roam-organize
  :straight (:host github :repo "ren-lingyu/org-roam-organize")
  :after org-roam
  :custom
  (org-roam-organize/directory org-roam-directory)
  (org-roam-organize/moc-directory (expand-file-name "./moc/" org-roam-directory))
  (org-roam-organize/fleeting-directory (expand-file-name "./fleeting/" org-roam-directory))
  (org-roam-organize/permanent-directory (expand-file-name "./permanent/" org-roam-directory))
  (org-roam-organize/move-target-directory (expand-file-name "./permanent/" org-roam-directory))
  (org-roam-organize/directory-p t)
  (org-roam-organize/move-source-tag "idea")
  (org-roam-organize/move-target-tag "zettel")
  (org-roam-organize/top-moc-file (expand-file-name "./moc/maps.org" org-roam-directory))
  (org-roam-organize/move-target-moc-file (expand-file-name "./moc/permanent.org" org-roam-directory))
  (org-roam-organize/capture-templates `(("m" "map of contents" plain "%?"
                                          :if-new 
                                          (file+head  
                                            "moc/${slug}.org"
                                            ,(concat
                                              "#+TITLE: ${title}\n"
                                              "#+AUTHOR: Lingyu Ren\n"
                                              "#+DATE: " (format-time-string "<%Y-%m-%d %A %H:%M:%S %z>") "\n"
                                              "#+FILETAGS: :map:\n"
                                              "#+DESCRIPTION:\n"))
                                        :unnarrowed t)))
  (org-roam-organize/move-target-directory-id-or-not t)
  (org-roam-organize/move-target-filename-id-or-not nil)
  :config
  (org-roam-organize-create-directory)
  :bind 
  (("C-c o o" . org-roam-organize-mode)
   ("C-c o c" . org-roam-organize-check-variables))
  :hook (after-init-hook . org-roam-organize-mode))

(global-set-key (kbd "C-c h d") (lambda () (interactive) (insert (concat "\n* " (format-time-string "%Y-%m-%d %A %z") "\n"))))

(use-package consult-org-roam
  :after (org-roam consult)
  :init
  (require 'org-roam)
  (require 'consult)
  (require 'consult-org-roam)
  :custom
  ;; Use `ripgrep' for searching with `consult-org-roam-search'
  (consult-org-roam-grep-func #'consult-ripgrep)
  ;; Configure a custom narrow key for `consult-buffer'
  (consult-org-roam-buffer-narrow-key ?r)
  ;; Display org-roam buffers right after non-org-roam buffers
  ;; in consult-buffer (and not down at the bottom)
  (consult-org-roam-buffer-after-buffers t)
  :config
  ;; Activate the minor mode
  (consult-org-roam-mode t)
  ;; Eventually suppress previewing for certain functions
  (consult-customize
    consult-org-roam-forward-links
    ;; :preview-key "TAB"
    :preview-key "M-."
    ;; :preview-key 'any
  )
  ;; (advice-add 'consult-org-roam-mode-hook :after
  ;;   (lambda (&rest _)
  ;;     (display-line-numbers-mode 1)
  ;;     (font-lock-mode 1)
  ;;   )
  ;; )
  :bind
  ;; Define some convenient keybindings as an addition
  ("C-c c f" . consult-org-roam-file-find)
  ("C-c c b" . consult-org-roam-backlinks)
  ("C-c c B" . consult-org-roam-backlinks-recursive)
  ("C-c c l" . consult-org-roam-forward-links)
  ("C-c c s" . consult-org-roam-search))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; 卡片工作台
(use-package org-workbench
  :straight (:host github :repo "yibie/org-workbench")
  :after org-roam ; 或 org-supertag、org-brain 等
  :config
  (org-workbench-setup))

;; DATE
(use-package org-roam-timestamps
  :straight (:host github :repo "tefkah/org-roam-timestamps")
  :after org-roam
  :config
  (org-roam-timestamps-mode 1)
  (setq org-roam-timestamps-parent-file t)
  (setq org-roam-timestamps-remember-timestamps t))

;; =============================
;; 自定义函数
;; =============================

(defun my/update-link-description ()
  "Update the description of the link at point to match the title of the corresponding Org-roam node in the database. 
  If the link is not an Org-roam ID link or the node cannot be found, display an appropriate message without making changes."
  (interactive)
  (require 'org-roam)
  (let ((element (org-element-context)))
    ;; Check if the cursor is on a link element
    (if (eq (org-element-type element) 'link)
      (let* (;; Link type, e.g., "id"
          (link-type (org-element-property :type element))
          ;; Link target (ID for Org-roam)
          (link-path (org-element-property :path element))
          ;; Start of description text
          (desc-begin (org-element-property :contents-begin element))
          ;; End of description text
          (desc-end (org-element-property :contents-end element)))
        ;; Ensure the link is an Org-roam ID link
        (if (and (string= link-type "id") link-path)
          (let* ((node-id link-path)
              ;; Query Org-roam database for the node title using EmacSQL
              (title (caar (org-roam-db-query
                    [:select title :from nodes :where (= id $s1)]
                    node-id))))
            (cond
              ;; Case 1: Node not found
              ((null title)
                (message "No node found with id=%s." node-id))
              ;; Case 2: Description already matches the node title
              ((and 
                desc-begin 
                desc-end
                  (string= title (buffer-substring-no-properties desc-begin desc-end)))
                (message "The link description is already up-to-date."))
              ;; Case 3: Update description to match node title
              (t
                (save-excursion
                  (goto-char desc-begin)
                  (delete-region desc-begin desc-end)
                  (insert title))
                (message "Description updated to: %s" title))))
          ;; Not an ID link
          (message "The current link is not an Org-roam ID link.")))
      ;; Cursor is not on a link
      (message "The cursor is not on a link."))))

(provide 'init-org-roam)
;;; init-org-roam.el ends here