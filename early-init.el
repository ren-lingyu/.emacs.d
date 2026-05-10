;;; early-init.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; 关闭启动界面
(setq inhibit-startup-message t)

;; time zone
(setenv "TZ" "Asia/Shanghai")

;; 禁用 package.el
(setq package-enable-at-startup nil)

;; straight.el
(setq straight-use-package-by-default t)
(setq straight-repository-branch "main")
(setq straight-vc-git-default-clone-depth 1) ; 使用浅克隆
(setq straight-vc-git-default-protocol 'https)
;; (setq straight-host-usernames '((github . "git")))
;; (setq straight-check-for-modifications '(check-on-save))

;; compile warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; 初始外观
(setq frame-resize-pixelwise t)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq blink-cursor-mode nil)

(setq default-frame-alist
      '((background-color . "gray10")
        (foreground-color . "gray90")
        (fullscreen . maximized)))

(setq initial-frame-alist default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
