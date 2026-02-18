;;; init-org-roam-citar.el -*- lexical-binding: t; -*-
;;; commentary:

;;; code:

;; 引用
(use-package citar
  :after org
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-templates
    '(
        (main . "| ${=type=:12} | ${title} | ${year issued date} | ${author editor:%etal:} |")
        (suffix . "${abstract}")
        (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal}.\n")
        (note . "Notes on ${author editor:%etal}, ${title}")
    )
  )
  (citar-format-reference
   '((author . (:style " %s" :fallback ""))
     (title . (:style " “%s”" :fallback ""))
     (year . (:style " (%s)" :fallback ""))
     (key . (:style " [%s]" :fallback ""))
     (type . (:style " [%s]" :fallback ""))
     (abstract . (:style "\n%s" :fallback ""))))
)

;; Citar 与 Embark 的集成增强
(use-package citar-embark
  :after (citar embark)
  :config
  (citar-embark-mode)  ;; 启用集成模式
)

(use-package citar-org-roam
  :after (org org-roam citar)
  :custom
  (citar-org-roam-mode t)
  :config
  ;; (advice-add 'citar-org-roam-mode-hook :after
  ;;   (lambda (&rest _)
  ;;     (display-line-numbers-mode 1)
  ;;     (font-lock-mode 1)
  ;;   )
  ;; )
  (setq citar-org-roam-subdir "literature")
  (setq citar-org-roam-template-fields
        '((:citar-title . ("title"))
          (:citar-author . ("author" "editor"))
          (:citar-year . ("year"))
          (:citar-month . ("month"))
          (:citar-doi . ("doi"))
          (:citar-isbn . ("isbn"))
          (:citar-url . ("url"))))
  (setq citar-org-roam-note-title-template "${title}")
  (setq org-roam-capture-templates
        (append org-roam-capture-templates
                '(("r" "reference" plain "%?"
                   :target
                   (file+head
                    "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/${id}.org"
                    "#+TITLE: ${note-title}\n#+AUTHOR: ${citar-author}\n#+YEAR: ${citar-year}\n#+MONTH: ${citar-month}\n#+DOI: ${citar-doi}\n#+ISBN: ${citar-isbn}\n#+URL: ${citar-url}\n#+FILETAGS: :ref:\n#+DESCRIPTION:\n\n* Citing Nodes")
                   :unnarrowed t)
                )
        )
  )
  (setq citar-org-roam-capture-template-key "r")
)

(defun my/citar-insert-then-create-note ()
  "Insert citation and then create a note for the selected entry."
  (interactive)
  (let ((refs (citar-select-refs)))
    (when refs
      ;; 插入引用
      (citar-insert-citation refs)
      ;; 创建笔记(使用第一个条目)
      (citar-create-note (car refs))))
)

(global-set-key (kbd "C-c c i") #'my/citar-insert-then-create-note)

(provide 'init-org-roam-citar)

;;; init-org-roam-citar.el ends here