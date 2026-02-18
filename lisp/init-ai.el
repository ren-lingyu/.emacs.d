;;; init-ai.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; copilot
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :branch "main")
  :config
  (setq copilot-idle-delay nil)
  :hook
  (prog-mode . copilot-mode)
  (LaTeX-mode . copilot-mode)
  (python-mode . copilot-mode)
  (TeX-mode . copilot-mode)
  :bind
  (:map copilot-mode-map
    ("C-TAB" . 'copilot-accept-completion)
    ("C-<right>" . 'copilot-accept-completion-by-word)
  )
)

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request org markdown-mode)
)

;; debug code
;; 由于 `lisp-indent-offset' 的默认值是 nil，在编辑 elisp 时每敲一个字
;; 符都会跳出一个 warning，将其默认值设置为 t 以永不显示这个 warning
(setq-default copilot--indent-warning-printed-p t
              copilot-indent-offset-warning-disable t)

  ;; 文件超出 `copilot-max-char' 的时候不要弹出一个 warning 的 window
(defun my-copilot-get-source-suppress-warning (original-function &rest args)
  "Advice to suppress display-warning in copilot--get-source."
  (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
    (apply original-function args)))
(advice-add 'copilot--get-source :around #'my-copilot-get-source-suppress-warning)

(provide 'init-ai)
;;; init-ai.el ends here