;;; init.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; 版本和操作系统检查
(let ((minver "28.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "30.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;; (defconst *is-a-mac* (eq system-type 'darwin)) ; 判断是否处于MacOS

;; 设定源码加载路径
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;; main settings
(require 'init-basic) ; 基本设置

(require 'init-help) ; 帮助信息优化

(require 'init-theme) ; 主题

(require 'init-org) ; Org设置

(require 'init-tex) ; Tex设置

(require 'init-complete) ; 自动补全

;; (require 'init-ai) ; AI辅助

(require 'init-orgraph) ; orgraph

;; debug
(require 'init-debug)

;; End
(provide 'init)
;;; init.el ends here
