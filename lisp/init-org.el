;;; init-org.el --- Org mode configuration -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(use-package org
  ;; :defer
  :demand t
  :straight 
  `(org
    :fork 
      (
        :host nil
        :repo "https://git.tecosaur.net/tec/org-mode.git"
        :branch "dev"
        :remote "tecosaur"
      )
    :files (:defaults "etc")
    :build t
    :pre-build
    (with-temp-file "org-version.el"
      (require 'lisp-mnt)
      (let 
        (
          (version
            (with-temp-buffer
              (insert-file-contents "lisp/org.el")
              (lm-header "version")
            )
          )
          (git-version
            (string-trim
              (with-temp-buffer
                (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
                (buffer-string)
              )
            )
          )
        )
        (insert
          (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
          (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
          "(provide 'org-version)\n"
        )
      )
    )
    :pin nil
  )
  :config
  (setq org-startup-indented t)
  (setq org-startup-truncated nil)
  (setq org-startup-folded nil)
  (setq org-hide-leading-stars nil)
  (setq org-latex-default-packages-alist nil)
  (setq org-latex-packages-alist nil)
  (setq org-cite-export-processors '((latex bibtex) (html csl) (t basic)))
  (setq org-src-fontify-natively t)             ; Org代码块语法高亮
  (setq org-src-tab-acts-natively t)            ; Org代码块TAB缩进
  (setq org-timestamp-formats  '("%Y-%m-%d %a %z" . "%Y-%m-%d %H:%M:%S %z"))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
)

(require 'org-element)

(with-eval-after-load 'org
  (set-face-attribute 'org-footnote nil 
    :underline nil        ; 移除下划线
    :foreground "#79a8ff" ; 保留颜色但不设置其他属性
    :background 'unspecified
    :inherit nil
    :inverse-video 'unspecified
    :weight 'unspecified
  )
  (set-face-attribute 'org-link nil
    :underline t
    :foreground "#6b6b6b"
    :background "#ffffff"
    :weight 'ultra-bold
    :inverse-video t
  )
)

;; 嵌入
;; (use-package org-transclusion
;;   :after org
;;   :straight (:host github :repo "nobiot/org-transclusion" :branch "main")
;;   :hook (org-mode . org-transclusion-mode)
;; )

;; (use-package org-include-inline
;;   :after org
;;   :straight (:host github :repo "yibie/org-include-inline" :branch "main")
;;   :hook (org-mode . org-include-inline-mode)
;; )

;; GTD and agenda
(use-package org-edna
  :after org
  :config 
  (org-edna-mode 1)
  (setq org-edna-use-inheritance t)
)
(use-package org-gtd
  :straight (:host github :repo "Trevoke/org-gtd.el" :branch "master")
  :after (org org-edna)
  :init
  (setq org-gtd-update-ack "4.0.0")
  (setq org-gtd-directory (expand-file-name "./agenda" user-emacs-directory))
  (unless
    (file-directory-p org-gtd-directory)
    (make-directory org-gtd-directory t)
  )
  :custom
  (org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))
  (org-gtd-keyword-mapping 
    '(
      (todo . "TODO")
      (next . "NEXT")
      (wait . "WAIT")
      (canceled . "CNCL")
    )
  )
  :config
  (setq org-agenda-files (list org-gtd-directory))
    (with-eval-after-load 'org-gtd-id
    (advice-add 'org-gtd-id--generate :override
      (lambda () (org-id-new))
    )
  )
  :bind
  (
    ("C-c d c" . org-gtd-capture)
    ("C-c d e" . org-gtd-engage)
    ("C-c d p" . org-gtd-process-inbox)
    ("C-c d n" . org-gtd-show-all-next)
    ("C-c d s" . org-gtd-reflect-stuck-projects)
  ;; ("C-c d s" . my/org-gtd-auto-save-files)
    :map org-gtd-clarify-mode-map
    ("C-c c" . org-gtd-organize)
    :map org-agenda-mode-map
    ("C-c ." . org-gtd-agenda-transient)
  )
)

;; 中文标记处理
;; (font-lock-add-keywords 
;;   'org-mode
;;   '(
;;     (
;;       "\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
;;       (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) "")))
;;     )
;;     (
;;       "\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
;;       (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))
;;     )
;;   )
;;   'append
;; )
;; (with-eval-after-load 'ox
;;   (defun eli-strip-ws-maybe (text _backend _info)
;;     (let* 
;;       (
;;         ;; remove whitespace from line break
;;         (text (replace-regexp-in-string "\\(\\cc\\) *\n *\\(\\cc\\)" "\\1\\2" text))
;;         ;; remove whitespace from `org-emphasis-alist'
;;         (text (replace-regexp-in-string "\\(\\cc\\) \\(.*?\\) \\(\\cc\\)" "\\1\\2\\3" text))
;;         ;; restore whitespace between English words and Chinese words
;;         (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)" "\\1 \\2 \\3" text))
;;       )
;;       text
;;     )
;;   )
;;   (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe)
;; )

;; 更新文件头
(defun my/update-and-insert-or-not-date-in-org-file (path time_string insert_bool)
  "Update #+DATE only for specific Org files."
  (when 
    (and  
      (eq major-mode 'org-mode)
      (buffer-file-name) ; 确保有文件名
      (file-directory-p path)
      (file-in-directory-p (buffer-file-name) path)
      (stringp time_string)
      (booleanp insert_bool)
    )
    (save-excursion
      (goto-char (point-min))
      (let* 
        (
          (now (format-time-string time_string))
        )
        (if 
          (re-search-forward "^#\\+DATE:" nil t)
          ;; 如果找到 DATE 关键字, 更新它
          (progn
            (delete-region (point) (line-end-position))
            (insert " " now)
          )
          ;; 如果没有找到 DATE 关键字, 在适当位置插入
          (progn
            (when
              insert_bool
              (progn
                (goto-char (point-min))
                (let  
                  (
                    (insert-pos (point-min))
                    (keywords '("SETUPFILE" "TITLE" "AUTHOR" "EMAIL" "INDEX"))
                  )
                  ;; 查找最后一个存在的关键字位置
                  (dolist 
                    (keyword keywords)
                    (when (re-search-forward (concat "^#\\+" keyword ":") nil t)
                      (setq insert-pos (line-end-position))
                    )
                  )
                  ;; 移动到插入位置并插入新的 DATE 关键字
                  (goto-char insert-pos)
                  (if 
                    (= insert-pos (point-min))
                    ;; 如果没有找到任何关键字, 在文件开头插入
                    (insert "#+DATE: " now "\n")
                    ;; 在最后一个关键字后插入
                    (forward-line)
                    (insert "#+DATE: " now "\n")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;; (defun my/update-setupfile-in-org-file (path)
;;   "Update #+SETUPFILE only for specific Org files."
;;   (when (and  (eq major-mode 'org-mode)
;;               (buffer-file-name)
;;               (file-in-directory-p (buffer-file-name) path)
;;               )
;;         )
;;     (save-excursion
;;       (goto-char (point-min))
;;       (let ((setupfile-path "~/dataspace/knowhub/ProjectFiles/setup.org"))
;;         (when (re-search-forward "^#\\+SETUPFILE:" nil t)
;;           ;; 只替换路径, 不删掉关键字
;;           (delete-region (match-end 0) (line-end-position))
;;           (insert " " setupfile-path)
;;         )
;;       )
;;     )
;;   )
;; )

;; 快捷键
(global-set-key 
  (kbd "C-c u d") 
  (lambda 
    () 
    (interactive) 
    (when 
      (buffer-file-name)
      (my/update-and-insert-or-not-date-in-org-file 
        (file-name-directory (buffer-file-name))
        "<%Y-%m-%d %a %z>" 
        nil
      )
    )
  )
)

(provide 'init-org)
;;; init-org.el ends here.
