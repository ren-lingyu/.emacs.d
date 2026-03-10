;;; init-ai.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; gptel
(use-package gptel
  :straight (:host github :repo "karthink/gptel")
  :config
  (setq gptel-directives
	(append gptel-directives
		'((translation . "You are a LLM working in Emacs as a specialized bilingual translation assistant specialized in academic STEM(such as phyics and mathematics) content. 
When the user provides a text input, your sole task is to perform a direct, precise, and context-free translation between Chinese and English.
The following rules must be strictly obeyed: 
1. If the input is in Chinese, translate it into English.
2. If the input is in English, translate it into Chinese.
3. Do not add any explanations, commentary, or additional text beyond the translated output.
4. Maintain the original tone (formal, casual, etc.) and intent of the input.
5. Output only the translated text without leading marks before the output message."))))
  ;; (gptel-make-ollama "Ollama"
  ;;   :host "localhost:11434"
  ;;   :stream t
  ;;   :models '(gemma3n:e4b
  ;; 	      deepseek-v3.2:cloud
  ;; 	      qwen3.5:cloud
  ;; 	      qwen3-coder-next:cloud))
  (gptel-make-gh-copilot "Copilot")
  (setq gptel-backend
	(gptel-make-ollama "Ollama"             ;Any name of your choosing
	  :host "localhost:11434"               ;Where it's running
	  :stream t                             ;Stream responses
	  :models '(gemma3n:e4b
		    deepseek-v3.2:cloud
		    qwen3.5:cloud
		    qwen3-coder-next:cloud)))
  (setq gptel-model 'deepseek-v3.2:cloud)
  )

;; ellama
(use-package ellama
  :straight (:host github :repo "s-kostyaev/ellama")
  :bind ("C-c e" . ellama)
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-hook . ellama-chat-send-last-message)
  :init (setopt ellama-auto-scroll t)
  :config
  ;; show ellama context in header line in all buffers
  (ellama-context-header-line-global-mode +1)
  ;; show ellama session id in header line in all buffers
  (ellama-session-header-line-global-mode +1)
  (setq ellama-language "中文"))

;; ;; copilot
;; (use-package copilot
;;   :straight (:host github :repo "copilot-emacs/copilot.el" :branch "main")
;;   :config
;;   (setq copilot-idle-delay nil)
;;   :hook
;;   (prog-mode . copilot-mode)
;;   (LaTeX-mode . copilot-mode)
;;   (python-mode . copilot-mode)
;;   (TeX-mode . copilot-mode)
;;   :bind
;;   (:map copilot-mode-map
;;     ("C-TAB" . 'copilot-accept-completion)
;;     ("C-<right>" . 'copilot-accept-completion-by-word)
;;   )
;; )

;; (use-package copilot-chat
;;   :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
;;   :after (request org markdown-mode)
;; )

;; ;; debug code
;; ;; 由于 `lisp-indent-offset' 的默认值是 nil，在编辑 elisp 时每敲一个字
;; ;; 符都会跳出一个 warning，将其默认值设置为 t 以永不显示这个 warning
;; (setq-default copilot--indent-warning-printed-p t
;;               copilot-indent-offset-warning-disable t)

;;   ;; 文件超出 `copilot-max-char' 的时候不要弹出一个 warning 的 window
;; (defun my-copilot-get-source-suppress-warning (original-function &rest args)
;;   "Advice to suppress display-warning in copilot--get-source."
;;   (cl-letf (((symbol-function 'display-warning) (lambda (&rest args) nil)))
;;     (apply original-function args)))
;; (advice-add 'copilot--get-source :around #'my-copilot-get-source-suppress-warning)

(provide 'init-ai)
;;; init-ai.el ends here
