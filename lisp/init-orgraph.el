;;; init-orgraph.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(defconst orgraph-directory org-directory)
(defconst texlive (expand-file-name "./config/texlive.sh" orgraph-directory))

(with-eval-after-load 'ox-latex
  (add-hook 'org-export-filter-final-output-functions #'my/insert-toc-after-abstract-or-title)
  (setq org-latex-precompile nil)
  (setq org-latex-compiler "lualatex")
  (setq org-latex-bib-compiler "biblatex")
  (setq org-latex-pdf-process
        (list
         (format "%s latexmk -f -pdf -%%latex -interaction=nonstopmode -output-directory=$(realpath %%o) $(realpath %%f)"
                 (shell-quote-argument texlive))))
  (setq org-latex-precompile-compiler-map
    `(("pdflatex" . ,(format "%s latex" (shell-quote-argument texlive))) 
      ("xelatex" . ,(format "%s xelatex -no-pdf" (shell-quote-argument texlive)))
      ("lualatex" . ,(format "%s dvilualatex" (shell-quote-argument texlive)))))
  (setq org-latex-classes
	`(("note"
           ,(mapconcat #'identity
		       '("\\documentclass[10pt,a4paper]{article}"
			 "\\usepackage{org-note}")
		       "\n")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
	  ("slide"
           ,(mapconcat #'identity
		       '("\\documentclass[8pt]{beamer}"
			 "\\usepackage{org-beamer}")
		       "\n")
           ("\\section{%s}" . "\\section*{%s}")
           ("\\subsection{%s}" . "\\subsection*{%s}")
           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
           ("\\subsubsubsection{%s}" . "\\subsubsubsection*{%s}")
           ("\\paragraph{%s}" . "\\paragraph*{%s}")
           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
  (setq org-export-headline-levels 4)
  (setq org-latex-default-class "note")
  ;; (setq org-latex-title-command "")
  (setq org-export-with-toc nil)
  (defun my/insert-toc-after-abstract-or-title (output backend info)
    (when (org-export-derived-backend-p backend 'latex)
      (if (string-match "\\\\end{abstract}" output)
          ;; 如果有 abstract 块, 在 \end{abstract} 后插入目录
          (setq output
                (replace-regexp-in-string "\\\\end{abstract}"
					  "\\\\end{abstract}\n\\\\tableofcontents\n"
					  output))
        ;; 如果没有 abstract 块, 在 \maketitle 后插入目录
        (setq output
              (replace-regexp-in-string "\\\\maketitle"
					"\\\\maketitle\n\\\\tableofcontents\n"
					output))))
    output)
  ;; 定义\label{eq:...}和\eqref{eq:...}对应的链接类型
  (org-link-set-parameters "eq"
			   :follow 
			   (lambda (path arg)
			     (let 
				 ((label (concat "\\label{eq:" path "}")))
			       (org-mark-element)
			       (goto-char (point-min))
			       (if 
				   (re-search-forward label nil t)
				   (progn
				     (beginning-of-line)
				     (recenter)
				     (message "找到公式引用: %s" label))
				 (message "未找到公式引用: %s" label))))
			   ;; 设置导出函数，导出为 \eqref{eq:...}
			   :export 
			   (lambda 
			     (path description backend info)
			     (cond
			      ;; 对于 LaTeX 导出
			      ((eq backend 'latex)
			       (format "\\eqref{eq:%s}" path))
			      ((eq backend 'html)
			       (format "<span class=\"eqref\">eq:%s</span>" 
				       (or description path)))
			      (t (or description (format "eq:%s" path)))))
			   ;; 自定义链接外观  
			   :face 
			   '(:inherit 'org-link
			     :foreground "dark red" 
			     :background "yellow"
			     :underline t)
			   :help-echo 
			   "公式引用链接. \n格式: [[eq:<label>]]. \n跳转时采用正则表达式查找当前光标所在buffer内\\label{eq:<label>}所在行. "))

(with-eval-after-load 'org-latex-preview
    (when (display-graphic-p) 
        (setq org-startup-with-latex-preview t))
	(setq org-latex-preview-mode-ignored-environments nil)
	(setq org-latex-preview-process-precompile nil)
    (setq org-latex-preview-preamble
		  (concat
			"\\documentclass{article}\n"
			"\\usepackage{xcolor}\n"
			"[PACKAGES]\n"
			"\\usepackage{org--math}\n"
			"\\pagestyle{empty}\n"))
    (setq org-latex-preview-compiler-command-map
        `(("pdflatex" . ,(format "%s latexmk -norc -latex=pdflatex" (shell-quote-argument texlive))) 
          ("xelatex" . ,(format "%s latexmk -norc -xelatex -no-pdf" (shell-quote-argument texlive)))
          ("lualatex" . ,(format "%s latexmk -norc -dvilua" (shell-quote-argument texlive)))))
    (setq org-latex-preview-process-default 'docker)
    (setq org-latex-preview-process-alist
        `((docker 
                :programs ("docker")
                :description "dvi > svg"
                :message "you need to install the programs: texlive and dvisvgm in docker image."
                :image-input-type "dvi"
                :image-output-type "svg"
                :latex-compiler ("%l -interaction=nonstopmode -outdir=%o %f")
                :image-converter 
                (,(format "%s dvisvgm --page=1- --optimize --clipjoin --relative --no-fonts --bbox=preview -o %%B-%%%%9p.svg %%f"
			  			  (shell-quote-argument texlive)))))))

(with-eval-after-load 'citar
  (setq citar-notes-paths 
	(list (expand-file-name "./literature/" org-roam-directory))
	citar-library-paths nil)
  (setq citar-bibliography 
		(list 
			(expand-file-name "./texmf/bibtex/bib/ref.bib" org-directory)
			(expand-file-name "./texmf/bibtex/bib/zotero-my-library.bib" org-directory))))

(with-eval-after-load 'org-roam-organize
  (setq org-roam-organize/tag-id-alist 
        '(("map" . "0c935953-8648-4d41-9d17-51c329f2a508")
          ("zettel" . "1c30c3be-1def-426b-9bf6-c0e53b084fbf")
          ("ref" . "dea94cb6-7409-4b2b-88d4-872fd29fadd3")
          ("idea" . "af10604e-797c-44f4-8227-f28bacb2b0f1")
          ("note" . "8b3dc8f5-d750-4cf4-8c93-1d4c8a728991")
          ("blog" . "ab4d834c-ee95-4fc8-8ea2-745b58d6c326"))))

(mapc #'require '(org org-roam org-gtd consult consult-org-roam auctex vertico orderless marginalia))

;; (setq compile-command "emacs --batch --load ~/.emacs.d/init.el --eval \"(org-publish-all t)\"")

;; 钩子
(add-hook 'before-save-hook 
	  (lambda ()
	    (let* ((post_file_list (my/blog-files)))
	      (cond ((and 
		      buffer-file-name
		      (file-in-directory-p buffer-file-name org-roam-directory)
		      (not (file-in-directory-p buffer-file-name (expand-file-name "./literature/" org-roam-directory)))
		      (not (member buffer-file-name post_file_list)))
		     (my/update-and-insert-or-not-date-in-org-file 
		      orgraph-directory
		      "<%Y-%m-%d %a %z>" 
		      t)
		     (message "Updated DATE in %s" (buffer-file-name)))
		    ((and
		      buffer-file-name
		      (file-in-directory-p buffer-file-name (expand-file-name "./permanent/" org-roam-directory)))
		     (my/update-and-insert-or-not-date-in-org-file 
		      orgraph-directory
		      "<%Y-%m-%d %a %z>"
		      nil))
		    (t nil)))))

(add-hook 'after-save-hook
	  (lambda ()
	    (let* ((post_file_list (my/blog-files))
		       (filename (buffer-file-name)))
	      (when (and
		     filename
		     (file-in-directory-p filename (expand-file-name "./permanent/" org-roam-directory))
		     (member filename post_file_list))
		(org-publish-all)
		(message "[INFO] Publish finished. ")))))

;; load init-local.el
(defun load-local ()
  (interactive)
  (load-file (expand-file-name "./config/init-local.el" orgraph-directory)))

(provide 'init-orgraph)
;;; init-orgraph.el ends here
