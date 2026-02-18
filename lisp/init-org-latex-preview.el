;;; init-org-preview.el --- Org preview configuration -*- lexical-binding: t; -*-
;;; commentary:

;;; from https://abode.karthinks.com/org-latex-preview/

;;; code:

(require 'org-latex-preview)
(setq org-latex-preview-numbered t)
(plist-put org-latex-preview-appearance-options :page-width 0.8)
(add-hook 'org-mode-hook 'org-latex-preview-mode)
(add-hook 'latex-mode-hook (lambda () (org-latex-preview-mode -1)))
(add-hook 'LaTeX-mode-hook (lambda () (org-latex-preview-mode -1)))
(setq org-latex-preview-mode-display-live t)
(setq org-latex-preview-mode-update-delay 0.25)
(defun my/org-latex-preview-uncenter (ov)
  (overlay-put ov 'before-string nil))
(defun my/org-latex-preview-recenter (ov)
  (overlay-put ov 'before-string (overlay-get ov 'justify)))
(defun my/org-latex-preview-center (ov)
  (save-excursion
    (when (derived-mode-p 'org-mode)
      (goto-char (overlay-start ov))
      (when-let* 
        (
          (elem (org-element-context))
          (
            (or 
              (eq (org-element-type elem) 'latex-environment)
              (string-match-p "^\\\\\\[" (org-element-property :value elem))
            )
          )
          (img (overlay-get ov 'display))
          (prop `(space :align-to (- center (0.55 . ,img))))
          (justify (propertize " " 'display prop 'face 'default))
        )
        (overlay-put ov 'justify justify)
        (overlay-put ov 'before-string (overlay-get ov 'justify))
      )
    )
  )
)
(define-minor-mode org-latex-preview-center-mode
  "Center equations previewed with `org-latex-preview'."
  :global nil
  (if org-latex-preview-center-mode
    (progn
      (add-hook 'org-latex-preview-overlay-open-functions
              #'my/org-latex-preview-uncenter nil :local)
      (add-hook 'org-latex-preview-overlay-close-functions
              #'my/org-latex-preview-recenter nil :local)
      (add-hook 'org-latex-preview-overlay-update-functions
              #'my/org-latex-preview-center nil :local)
    )
    (remove-hook 'org-latex-preview-overlay-close-functions
                  #'my/org-latex-preview-recenter)
    (remove-hook 'org-latex-preview-overlay-update-functions
                  #'my/org-latex-preview-center)
    (remove-hook 'org-latex-preview-overlay-open-functions
                  #'my/org-latex-preview-uncenter)
  )
)
(add-hook 'org-latex-preview-mode-hook #'org-latex-preview-center-mode)

(defun my/org-latex-preview-cleanup (exit-code process-buffer processing-info)
  "生成图片后删除中间文件."
  (when (= exit-code 0)
    (let ((texfile (plist-get processing-info :texfile)))
      (when texfile
        (dolist (ext '(".aux" ".log" ".tex" ".out" ".fls" ".fdb_latexmk" ".dvi"))
          (let ((file (concat (file-name-sans-extension texfile) ext)))
            (when (file-exists-p file)
              (delete-file file))))))))
(add-hook 'org-latex-preview-process-finish-functions #'my/org-latex-preview-cleanup)

(make-directory
 (expand-file-name "org-latex-preview/" temporary-file-directory)
 t)

(advice-add 'org-latex-preview--create-tex-file :override
	    (lambda (processing-info fragments appearance-options)
	      (let* ((header
		      (concat
		       (plist-get processing-info :latex-header)
           org-latex-preview--include-preview-string))
		     (textwidth
		      ;; We can fetch width info from APPEARANCE-OPTIONS, but it's
		      ;; possible that an old config using `org-format-latex-options'
		      ;; won't have :page-width set, and so we need a default too.
		      (let ((w (or (plist-get appearance-options :page-width) 0.6)))
			(cond
			 ((stringp w)
			  (format "\n\\setlength{\\textwidth}{%s}\n" w))
			 ((and (floatp w) (<= 0.0 w 1.0))
			  (format "\n\\setlength{\\textwidth}{%s\\paperwidth}\n" w)))))
		     (relative-file-p
		      (string-match-p "\\(?:\\\\input{\\|\\\\include{\\)[^/]" header))
		     (remote-file-p (file-remote-p default-directory))
		     (tex-temp-name
		      (expand-file-name
		       (concat "./org-latex-preview/" (make-temp-name "org-tex-") ".tex")
		       (and temporary-file-directory)))
		     (write-region-inhibit-fsync t)
		     (coding-system-for-write buffer-file-coding-system)
		     (precompile-failed-msg))
		(when (and relative-file-p remote-file-p)
      (error "Org LaTeX Preview does not currently support \\input/\\include in remote files"))
		(when org-latex-preview-process-precompile
		  (pcase (plist-get processing-info :latex-processor)
		    ("pdflatex"
		     (if-let ((format-file (org-latex-preview--precompile processing-info header
                                                              (not relative-file-p))))
			 (setq header (concat "%& " (file-name-sans-extension format-file)))
           (setq precompile-failed-msg
                 (concat 
                  (format "Precompile failed for buffer %s."
                          (plist-get processing-info :org-buffer))
                  (when-let ((filename (buffer-file-name
                                        (plist-get processing-info :org-buffer))))
                    (format " (File %s)" filename))))))
		    ((or "xelatex" "lualatex")
		     (setq precompile-failed-msg
			   (concat
			    (plist-get processing-info :latex-processor)
			    " does not support precompilation."))))
		  (when precompile-failed-msg
		    (display-warning
		     '(org latex-preview disable-local-precompile)
		     (concat
		      precompile-failed-msg
		      " Disabling LaTeX preview precompile in this buffer.\n To re-enable, run `(setq-local org-latex-preview-process-precompile t)' or reopen this buffer."))
		    (setq-local org-latex-preview-process-precompile nil)))
		(with-temp-file tex-temp-name
		  (insert header)
		  (insert textwidth
			  "\n\\begin{document}\n\n"
			  "\\setlength\\abovedisplayskip{0pt}"
			  " % Remove padding before equation environments.\n\n")
		  (dolist (fragment-info fragments)
		    (insert
		     "\n\\begin{preview}\n"
		     (plist-get fragment-info :string)
		     "\n\\end{preview}\n"))
		  (insert "\n\\end{document}\n"))
		tex-temp-name)))

(provide 'init-org-latex-preview)
;;; init-org-latex-preview.el ends here. 
