;;; init-tex.el
;;; commentary:
;;; code: 

(use-package auctex
  :defer t
  :init
  (setq TeX-auto-save t ; 自动保存
        TeX-parse-self t ; 自动解析文档结构
        TeX-master nil ; 不指定主文件
        preview-image-type nil ; 禁用预览
        TeX-command-default "LaTeX"
        TeX-save-query nil
        TeX-show-compilation nil ; 禁用编译
        TeX-PDF-mode nil)
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook
    (lambda ()
      (setq TeX-electric-escape nil
            TeX-electric-sub-and-superscript nil
      ) ; 自动补全
    )
  )
  :config
  (setq TeX-auto-local (locate-user-emacs-file "cache/auctex-auto/"))
  (unless 
    (file-exists-p TeX-auto-local)
    (make-directory TeX-auto-local t)
  )
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
)

(use-package company-auctex
  :after auctex
  :hook (LaTeX-mode . company-auctex-init)
)

(use-package company-math
  :after company
  :init
  (add-to-list 'company-backends 'company-math-symbols-unicode)
  (add-to-list 'company-backends 'company-math-symbols-latex)
)

(use-package flycheck
  :hook (LaTeX-mode . flycheck-mode)
)

(use-package cdlatex
  :hook 
  (org-mode . turn-on-org-cdlatex)
  (LaTeX-mode . company-auctex-init)
)

;; 启用 org-mode 的 LaTeX 区域高亮支持
(setq org-highlight-latex-and-related '(native script))

;; 自定义 LaTeX 高亮规则
(defun my/org-latex-font-lock ()
  "Strict LaTeX syntax highlighting in org-mode."
  (font-lock-add-keywords
   nil
   '(
      ;; 数学区域 $...$ 和 \( ... \)
      ("\\$[^$ \n]+\\$" . font-lock-constant-face)
      ("\\\\(\\([^() \n]+\\)\\\\)" 1 font-lock-constant-face)
      ;; 环境名, 如 \begin{equation}
      ("\\\\\\(begin\\|end\\){\\([^}]+\\)}"
        (1 font-lock-keyword-face)
        (2 font-lock-type-face)
      )
      ;; LaTeX 命令, 如 \frac, \alpha
      ("\\\\[a-zA-Z@]+" . font-lock-keyword-face)
      ;; 匹配 ^{...} 或 ^单个字符
      ("\\^\\({[^}]*}\\|[^{ \n]\\)" . font-lock-builtin-face)
      ;; 匹配 _{...} 或 _单个字符
      ("_\\({[^}]*}\\|[^{ \n]\\)" . font-lock-builtin-face)

      ;; 自定义宏命令(严格匹配)
      ("\\\\\\(mr\\|bs\\|diagmat\\){[^{} \n]+}" . font-lock-builtin-face)
      ;; \ab(...)、\ab{...}、\ab[...]、\ab|...|、\ab<...>
      ("\\\\ab(\\([^() \n]+\\))" 1 font-lock-builtin-face)
      ("\\\\ab{\\([^{} \n]+\\)}" 1 font-lock-builtin-face)
      ("\\\\ab\\[\\([^][]+\\)\\]" 1 font-lock-builtin-face)
      ("\\\\ab|\\([^| \n]+\\)|" 1 font-lock-builtin-face)
      ("\\\\ab<\\([^<> \n]+\\)>" 1 font-lock-builtin-face)
      ;; 量子态符号(严格匹配, 避免孤立符号)
      ("\\\\bra<\\([^| \n]+\\)|" 1 font-lock-keyword-face)
      ("\\\\ket|\\([^> \n]+\\)>" 1 font-lock-keyword-face)
      ;; \braket<...|...> 和 \braket<...|...|...>
      ("\\\\braket<\\([^| \n]+\\)|\\([^> \n]+\\)>" . font-lock-keyword-face)
      ("\\\\braket<\\([^| \n]+\\)|\\([^| \n]+\\)|\\([^> \n]+\\)>" . font-lock-keyword-face)
    )
  )
)

(add-hook 'org-mode-hook #'my/org-latex-font-lock)
(add-hook 'LaTeX-mode-hook #'my/org-latex-font-lock)

;; 与latex环境有关的快捷键

(global-set-key 
  (kbd "C-c l e e") 
  (lambda ()
    (interactive)
    (insert "\\begin{equation}\n\n\\end{equation}")
    (forward-line -2)
  )
) 
(global-set-key 
  (kbd "C-c l e a") 
  (lambda ()
    (interactive)
    (insert "\\begin{align}\n\n\\end{align}")
    (forward-line -2)
  )
)
(global-set-key 
  (kbd "C-c l e s") 
  (lambda ()
    (interactive)
    (insert "\\begin{split}\n\n\\end{split}")
    (forward-line -2)
  )
)
(global-set-key
  (kbd "C-c l e l")
  (lambda ()
    (interactive)
    (insert "\\label{}\n")
    (forward-line -1)
  )
)
                              

;; The End
(provide 'init-tex)

;;; init-tex.el ends here
