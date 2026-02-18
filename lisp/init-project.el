;;; init-project-directory.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; 检查当前路径是否在指定项目目录下
(defun my/dir-in-p (path)
  (and
    (file-directory-p path)
    (string-prefix-p 
      (file-name-as-directory (file-truename (expand-file-name path))) 
      (file-name-as-directory (file-truename (expand-file-name default-directory)))
    )
  )
)

;; 项目路径
(defvar project-directory/knowhub (expand-file-name "~/dataspace/knowhub/ProjectFiles/"))
(defvar project-directory/blog (expand-file-name "~/dataspace/blog/ProjectFiles/"))

;; 项目加载
(when 
    (my/dir-in-p project-directory/knowhub) 
    (require 'init-knowhub)
)
(when 
    (my/dir-in-p project-directory/blog) 
    (require 'init-blog)
)

(provide 'init-project)
;;; init-project.el ends here