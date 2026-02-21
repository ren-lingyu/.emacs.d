;;; init-basic.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; 加载最新的.el文件
(setq load-prefer-newer t)
(setq package-enable-at-startup nil)

;; time zone
(setenv "TZ" "Asia/Shanghai")

;; temporary directory
(setq temporary-file-directory (expand-file-name "~/tmp/emacs/"))
(unless (file-directory-p temporary-file-directory)
  (make-directory temporary-file-directory t))
(setq small-temporary-file-directory (expand-file-name temporary-file-directory))
(setq org-babel-remote-temporary-directory (expand-file-name temporary-file-directory))

;; straight.el 引导
(defvar bootstrap-version)
(let 
    ((bootstrap-file
      (expand-file-name
       "straight/repos/straight.el/bootstrap.el"
       (or
        (bound-and-true-p straight-base-dir)
        user-emacs-directory)))
     (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq package-enable-at-startup nil) ; 禁用package.el
(setq straight-vc-git-default-clone-depth 1) ; 使用浅克隆
(setq straight-vc-git-default-protocol 'https)
(setq straight-host-usernames '((github . "git")))
(setq native-comp-async-report-warnings-errors nil)

;; 基础编码和启动界面设置, 基本外观设置
(setq inhibit-startup-message t) ;; 关闭启动界面
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(unless (display-graphic-p)
  (setq frame-background-mode 'dark)
  (set-face-background 'default "black")
  (set-face-foreground 'default "white"))     ; 终端背景/前景色

(setq initial-frame-alist
      '((fullscreen . fullboth)
	(top . 10)
	(left . 10)))
(setq default-frame-alist
      '((fullscreen . fullboth)
	(top . 10)
	(left . 10)))

(menu-bar-mode -1)                            ; 关闭菜单栏
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq blink-cursor-mode nil)

(show-paren-mode 1)                           ; 高亮匹配括号
(global-font-lock-mode 1)                     ; 全局语法高亮

(column-number-mode t)                        ; 在 Mode line 显示列号
(global-display-line-numbers-mode 1)          ; 全局显示行号
;; (setq display-line-numbers-type 'relative) ; 显示相对行号


;; Emacs行为设置
;; (electric-pair-mode t)                        ; 自动补全括号
(global-auto-revert-mode t)                   ; 文件外部修改自动刷新
(delete-selection-mode t)                     ; 选中文本后输入替换

(setq make-backup-files nil                     ; 备份文件 <filename>~
      auto-save-default nil                     ; 自动保存文件 #<filename>#
      create-lockfiles nil)                     ; 锁文件 .#<filename>

;; Backup and auto-save configuration
(setq backup-directory-alist `((".*" . ,(locate-user-emacs-file "cache/backups/"))))
(setq auto-save-file-name-transforms `((".*" ,(locate-user-emacs-file "cache/auto-save/") t)) create-lockfiles nil)

(add-hook 'prog-mode-hook #'hs-minor-mode)    ; 编程模式折叠代码块
;; (savehist-mode 1)                           ; 保存历史记录

;; (setq confirm-kill-emacs #'y-or-n-p) ; 关闭 Emacs 前确认

(defconst *spell-check-support-enabled* t)    ; 是否启用拼写检查


;; 忽略格式风格警告
(setq byte-compile-warnings '(not docstrings))

;; 有关use-package
(straight-use-package 'use-package)
(eval-when-compile (require 'use-package))

;; 有关窗口显示
(setq display-buffer-alist
      '(("*compilation*"
	 (display-buffer-in-direction)
	 (direction . below)
	 (window-height . 6))))

(setq compilation-scroll-output t)

;; 状态栏美化(终端兼容)
(use-package powerline
  :config 
  (powerline-default-theme))

;; 快捷键提示
(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.7))

;; 历史记录
(use-package savehist
  :hook (after-init . savehist-mode)
  :custom
  ;; 设置保存文件的位置
  (savehist-file (locate-user-emacs-file "cache/savehist"))
  ;; 额外保存剪切板和shell命令行历史
  (savehist-additional-variables '(kill-rings shell-command-history))
  ;; 不保存消息历史
  (savehist-ignored-variables '(message-history))
  ;; 自动去重
  (history-delete-duplicates t)
  ;; 保存历史数据条目
  (history-length 1000))

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-menu-item 10)
  (recentf-save-file (locate-user-emacs-file "cache/recentf")))

;; 语法高亮增强
(use-package rainbow-delimiters
  :hook 
  (prog-mode . rainbow-delimiters-mode))

;; 搜索功能增强
(use-package vertico
  :init
  (vertico-mode)
  (with-eval-after-load 'vertico
    (define-key vertico-map "\DEL" #'vertico-directory-delete-char)
    (define-key vertico-map "\C-d" #'vertico-directory-delete-word)))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-overrides
   '((file (styles partial-completion))
     (command (styles orderless))
     (symbol (styles orderless))
     (variable (styles orderless))))
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package counsel)

(use-package ripgrep)

(use-package consult
  :after ripgrep
  :bind
  (("C-s" . consult-line)               ;; 替代 swiper
   ("C-x b" . consult-buffer)           ;; 替代 ivy-switch-buffer
   ("C-c v" . consult-bookmark)         ;; 替代 ivy-push-view
   ("C-c s" . consult-buffer)           ;; 替代 ivy-switch-view
   ("C-c V" . consult-recent-file)      ;; 替代 ivy-pop-view
   ("C-x C-SPC" . consult-mark)         ;; 替代 counsel-mark-ring
   ("C-x C-@" . consult-mark)           ;; macOS 映射
   ("C-c i" . consult-imenu)
   ("C-c g" . consult-grep)
   ("C-c G" . consult-ripgrep)))

;; Embark：上下文操作
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim))
  :init
  (setq embark-prompter 'embark-completing-read-prompter))

;; Embark 与 Consult 的集成增强
(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode)
  :config
  ;; 显式加载, 确保消除警告
  (require 'embark-consult))

(use-package ace-window
  :bind 
  (("C-x o" . 'ace-window)))

;; undo
(use-package undo-tree
  :straight (:host gitlab :repo "tsc25/undo-tree")
  :init (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history nil))

(use-package vundo
  :straight (:host github :repo "casouri/vundo"))

;; avy
(use-package avy
  :bind
  (("C-c C-SPC" . avy-goto-char-timer)))

;; 文件树 (类似 VSCode 的 sidebar)
(use-package neotree
  :bind 
  ("<f8>" . neotree-toggle))

;; 各种语言和文件格式的简单支持
(use-package markdown-mode)
(use-package yaml-mode)
(use-package dockerfile-mode
  :straight (:host github :repo "spotify/dockerfile-mode")
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :config (put 'dockerfile-image-name 'safe-local-variable #'stringp))
(use-package dotenv-mode
  :straight (:host github :repo "preetpalS/emacs-dotenv-mode")
  :mode ("\\.env\\..*\\'" . dotenv-mode))

;; 快捷键

;; (global-set-key (kbd "RET") 'newline-and-indent) ; Enter 键设置为"新其一行并做缩进"
;; (global-set-key (kbd "M-w") 'kill-region)              ; 交换 M-w 和 C-w, M-w 为剪切
;; (global-set-key (kbd "C-w") 'kill-ring-save)           ; 交换 M-w 和 C-w, C-w 为复制
;; (global-set-key (kbd "C-a") 'back-to-indentation)      ; 交换 C-a 和 M-m, C-a 为到缩进后的行首
;; (global-set-key (kbd "M-m") 'move-beginning-of-line)   ; 交换 C-a 和 M-m, M-m 为到真正的行首
;; (global-set-key (kbd "C-c /") 'comment-or-uncomment-region) ; 为选中的代码加注释/去注释, 与VSCode中一致

(global-set-key
 (kbd "C-c c c") 
 (lambda ()
   (interactive)
   (insert "#+BEGIN_COMMENT\n\n#+END_COMMENT\n")
   (forward-line -2)))

(global-set-key 
 (kbd "C-c c a") 
 (lambda ()
   (interactive)
   (insert "#+BEGIN_abstract\n\n#+END_abstract\n")
   (forward-line -2)))

(provide 'init-basic)
;;; init-basic.el ends here.
