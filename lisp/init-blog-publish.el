;;; init-blog-publish.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(unless (featurep 'org) (require 'org))
(unless (featurep 'org-roam) (require 'org-roam))
(unless (featurep 'ox-publish) (require 'ox-publish))
(unless (featurep 'ox-html) (require 'ox-html))

(use-package citeproc)

(defconst blog/beginning-year 2026)
;; (defconst org-blog-org-blog-url "https://aren-coco.com/")
;; (defconst org-blog-org-blog-url "http://localhost:8081/")

;; license strings
(defconst cc-license-generic-work-full-tool-name
  (concat 
   "This work is licensed under" "\u0020"
   "<a rel=\"license\" href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International</a>"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/cc.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/by.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/nc.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/sa.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"))

(defconst cc-license-no-geniric-work-full-tool-name
  (concat 
   ;; "<a href=\"https://aren-coco.com/\">Untitled</a>"
   "&copy;" 
   (if (= (string-to-number (format-time-string "%Y")) blog/beginning-year) 
       (number-to-string blog/beginning-year) 
     (concat (number-to-string blog/beginning-year) "-" (format-time-string "%Y"))) "\u0020"
   "<a rel=\"cc:attributionURL\" href=\"https://aren-coco.com/\">aRenCoco</a>" "\u0020" "\u00B7" "\u0020"
   "<a rel=\"license\" href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International</a>"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/cc.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/by.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/nc.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/sa.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"))

(defconst cc-license-no-genic-work-no-full-tool-name
  (concat
   ;; "<a href=\"https://aren-coco.com/\">Untitled</a>"
   "&copy;" 
   (if (= (string-to-number (format-time-string "%Y")) blog/beginning-year) 
       (number-to-string blog/beginning-year) 
     (concat (number-to-string blog/beginning-year) "-" (format-time-string "%Y"))) "\u0020"
   "<a rel=\"cc:attributionURL\" href=\"https://aren-coco.com/\">aRenCoco</a>" "\u0020" "\u00B7" "\u0020"
   "<a rel=\"license\" href=\"https://creativecommons.org/licenses/by-nc-sa/4.0/\">CC BY-NC-SA 4.0</a>"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/cc.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/by.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/nc.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"
   "<img src=\"https://mirrors.creativecommons.org/presskit/icons/sa.svg\" alt=\"\" style=\"max-width: 1em;max-height:1em;margin-left: .2em;\">"))

;; Blog configuration
(defconst org-blog-sitemap-title "Post")
(defconst org-blog-theindex-title "Index")
(defconst org-blog-sitemap-abstract "The map of blog posting, sorted by DATE and categorized by FILESTAGS. ")
(defconst org-blog-theindex-abstract "The website index, sorted by the first letter of the title. ")

(setq org-html-head 
      (concat 
       "<link rel=\"stylesheet\" href=\"https://font.aren-coco.com/MapleMono-NF-CN-Regular/result.css\"/>\n"
       "<link rel=\"stylesheet\" href=\"https://font.aren-coco.com/LXGWWenKai-Regular/result.css\"/>\n"
       "<link rel=\"stylesheet\" href=\"https://cdn.boxicons.com/3.0.8/fonts/basic/boxicons.min.css\"/>\n"
       "<link rel=\"icon\" type=\"image/svg+xml\" href=\"/favicon.svg\"/>\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/org-html-style-default.css?v=" (format-time-string "%Y%m%d%z") "\"/>\n"
       "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/org-html-style-local.css?v=" (format-time-string "%Y%m%d%z") "\"/>\n" ))

(setq org-html-home/up-format
      "<div class=\"flexbox\">\n")

(setq org-html-preamble-format
      `(("en"
	 ,(concat
	   "\n"
	   ;; "<div class=\"flexbox\">\n"
	   ;; "<div id=\"org-div-home-and-up\">\n"
	   "<header>\n"
	   "<nav>\n"
	   "<a accesskey=\"\" href=\"/index.html\">aRenCoco's Blog</a>\n"
	   "<a accesskey=\"\" href=\"/sitemap.html\">Post</a>\n"
	   "<a accesskey=\"\" href=\"/about.html\">About</a>\n"
	   "<a accesskey=\"\" href=\"/style.html\">Style</a>\n"
	   "<a accesskey=\"\" href=\"/theindex.html\">Index</a>\n"
	   "</nav>\n"
	   "</header>\n"
	   ;; "</div>\n"
	   "<hr class=\"topline\">\n"
	   "\n"))))

(setq org-html-postamble-format
      `(("en"
	 ,(concat
           "\n"
           ;; "<hr class=\"bottomline\">\n"
           "<footer>\n"
           "<p>\n"
           cc-license-no-genic-work-no-full-tool-name
           "</p>\n"
           "<p>\n"
           "Generated at <span class=\"update-time\">%T</span> by %c on WSL2." 
           "</p>\n"
           "</footer>\n"
           "</div>\n"
           "\n"))))

(add-to-list 'org-export-global-macros
  '("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@"))

(add-to-list 'org-export-global-macros
  '("filetags" . "@@html:<span class=\"filetags\" data-filetags=\"$1\"></span>@@"))

(defun org-blog-sitemap-entry-format (entry _style project)
  "Sitemap PROJECT ENTRY STYLE format that includes date."
  (let* ((file (org-publish--expand-file-name entry project))
	 ;; 获取标题
	 (parsed_title (org-publish-find-property file :title project))
	 (title (if parsed_title
		    (org-no-properties (org-element-interpret-data parsed_title))
		  (file-name-nondirectory (file-name-sans-extension file))))
	 ;; 获取日期
	 (date_prop (car (org-publish-find-property file :date project)))
	 ;; (tags (mapconcat (lambda (tag) (concat "#" tag)) (org-publish-find-property file :filetags project) " "))
	 (tags_list (org-publish-find-property file :filetags project))
	 (tags (mapconcat
		(lambda (tag) (concat "#" tag))
		(cl-remove-if
		 (lambda (tag) (member tag (list "zettel" "blog" "post")))
		 tags_list)             
		" "))
	 (date-str (if date_prop
		       (format-time-string "%Y-%m-%d %a %z" (org-timestamp-to-time date_prop))
		     "")))
    (org-publish-cache-set-file-property file :title title)
    ;; 输出 sitemap 项
    (if (and
         (member "post" tags_list)
         (not (= (length title) 0)) 
         (not (or (null (file-name-directory entry)) (string= (file-name-directory entry) "./") (string= (file-name-directory entry) "/"))))
	(format "{{{timestamp(%s)}}} [[file:%s][%s]] {{{filetags(%s)}}} " date-str entry title tags)
      (format "" entry)
      ;; (progn
      ;;   (if
      ;;     (or (null (file-name-directory entry)) (string= (file-name-directory entry) "./") (string= (file-name-directory entry) "/"))
      ;;     (format "[[file:%s][%s]]" entry title)
      ;;     (format "{{{timestamp(%s)}}} [[file:%s][%s]] {{{filetags(%s)}}} " date-str entry title tags)
      ;;     ;; (format "{{{timestamp(%s)}}} [[file:%s][%s]]" date-str entry title)
      ;;   )
      ;; )
      )))

(defun org-blog--checkout-tags (list)
  (let* ((tags_hash (make-hash-table :test 'equal)))
    (dolist (entry list)
      (when (and 
           (listp entry) 
           (stringp (car entry)))
        (let* ((str (car entry)))
          (when (string-match "{{{filetags(\\([^)]*\\))}}}" str)
            (let* ((tags_str (match-string 1 str)))
              (dolist (tag (split-string tags_str " " t))
                (puthash tag t tags_hash)))))))
    (hash-table-keys tags_hash)))

(defun org-blog-publish-sitemap-filtered (title list)
  "Generate a sitemap using a custom filter function. TITLE is the sitemap title. LIST is an internal representation of files as returned by `org-list-to-lisp'."
  (let* ((filtered_list
          (cl-remove-if
           (lambda (entry)
             (cond
              ((null entry) t)
              ((listp entry) (string= (car entry) ""))
              ((stringp entry) (string= entry "")) 
              (t nil)))
           list))
	 (blog_tags (org-blog--checkout-tags filtered_list)))
    (setq org-html-head-extra
	  (format 
           "<style>\n%s%s</style>\n"
           (concat
            "li:has(.filetags){display: none;}\n"
            ".content:has([value=\"all\"]:checked)\n"
            "li{display: list-item;}\n")
           (mapconcat
            (lambda (tag)
              (format 
               (concat
                ".content:has([value=\"%s\"]:checked)\n"
                "li:has([data-filetags~=\"%s\"]){display: list-item;}\n")
               tag 
               tag))
            blog_tags)))
    (concat 
     "#+TITLE: " title "\n"
     "#+INDEX: " title "\n"
     "#+FILETAGS: :blog:index:\n"
     "#+BEGIN_abstract\n"
     org-blog-sitemap-abstract "\n"
     "#+END_abstract\n"
     (format 
      (concat 
       "#+BEGIN_EXPORT html\n" 
       "<section class=\"filter\">\n%s\n%s</section>\n" 
       "#+END_EXPORT\n")
      (concat 
       "<label class=\"category\">\n" 
       "<input type=\"radio\" name=\"tag\" value=\"all\" checked/>\n" 
       "<span>all</span>\n" 
       "</label>\n")
      (mapconcat
       (lambda (tag)
         (format 
          (concat 
           "<label class=\"category\">\n" 
           "<input type=\"radio\" name=\"tag\" value=\"%s\"/>\n" 
           "<span>%s</span>\n" 
           "</label>\n")
          tag 
          tag))
       blog_tags "\n"))
     "\n"
     (org-list-to-org filtered_list))))

;; (advice-add 'org-publish-index-generate-theindex :after
;; 	    (lambda (project directory)
;; 	      (let ((index.org (expand-file-name "theindex.org" directory)))
;; 		(when (file-exists-p index.org)
;; 		  (with-temp-buffer
;; 		    (insert 
;; 		     (concat
;; 		      "#+TITLE: " org-blog-theindex-title "\n"
;; 		      "#+INDEX: " org-blog-theindex-title "\n"
;; 		      "#+FILETAGS: :blog:index:\n"
;; 		      "#+BEGIN_abstract\n" 
;; 		      org-blog-theindex-abstract "\n"
;; 		      "#+END_abstract\n"
;; 		      "#+INCLUDE: \"theindex.inc\"\n"))
;; 		    (write-region (point-min) (point-max) index.org))))))

(setq org-html-head-include-default-style nil)
;; (setq org-export-with-toc t)
(setq org-publish-sitemap-file-entry-format "%d-%t")
(setq org-html-metadata-timestamp-format "%Y-%m-%d %a %H:%M %z")

(defun my/blog-files ()
  "Return a list of note files containing 'blog' tag." ;
  (seq-uniq
    (seq-map
      #'car
      (org-roam-db-query [:select [nodes:file]
				  :from tags
				  :left-join nodes
				  :on (= tags:node-id nodes:id)
				  :where (like tag (quote "%\"blog\"%"))]))))

(defun my-generator/org-html-publish-to-html (blog_file_list)
  "Generate function with args PLIST, FILENAME and PUB-DIR to publish files who member a list, which is appending to BLOG_FILE_LIST, to HTML."
  (lambda (plist filename pub-dir)
    (let* ((file_name (expand-file-name filename))
	   (base_directory (expand-file-name "./roam/permanent/" org-directory))
	   (no_node_index_file_list (list (expand-file-name "sitemap.org" base_directory)
					  (expand-file-name "theindex.org" base_directory)))
	   (post_file_list (append (symbol-value blog_file_list) no_node_index_file_list)))
      (when (member file_name post_file_list)
	(org-html-publish-to-html plist filename pub-dir)))))

(defun my/blog-build-article-status (info)
  "Return HTML snippet for article timestamps."
  (let* ((input_file (file-name-nondirectory (plist-get info :input-file))))
    (if (not (member input_file (list "index.org" "sitemap.org" "theindex.org" "about.org" "style.org")))
	(let* ((spec (org-html-format-spec info))
               (date_str (format-time-string "%Y-%m-%d %z" (org-time-string-to-time (format-spec "%d" spec))))
               (lastmod_str (format-time-string "%Y-%m-%d %z" (org-time-string-to-time (format-spec "%C" spec))))
               ;; (date_str (format-spec "%d" spec))
               ;; (lastmod_str (format-spec "%C" spec))
               )
          (concat
           "<div class=\"post-status\">\n"
           (format "<span>\n<i class='bx bx-calendar'></i>\n<span>%s</span>\n</span>\n<span>\n<i class='bx bx-edit'></i>\n<span>%s</span>\n</span>\n" date_str lastmod_str)
           "</div>\n"))
      "")))

(defun my/org-html-inject-article-status (body backend info)
  "Insert publication and last-modified timestamps at the top of BODY."
  (if (eq backend 'html)
      (concat (my/blog-build-article-status info) "\n" body)
    body))

(add-to-list 'org-export-filter-body-functions #'my/org-html-inject-article-status)

;; (setq org-export-with-planning t)

(defvar my/blog-file-list nil)

(setq org-publish-project-alist
      `(("post"
	 :base-directory ,(expand-file-name "./roam/permanent/" org-directory)
	 :base-extension "org"
	 :publishing-directory ,(expand-file-name "./public/" org-directory)
	 :recursive t
	 :preparation-function ,(lambda (project) (setq my/blog-file-list (my/blog-files)))
	 :publishing-function ,(my-generator/org-html-publish-to-html 'my/blog-file-list)
	 :completion-function ,(lambda (project) (setq my/blog-file-list nil))
	 :html-link-home "/index.html"
	 :with-author "aRenCoco"
	 :with-email "aRen_Coco@outlook.com" 
	 :headline-levels 5
	 :with-toc t
	 :with-creator t
	 :with-timestamp t
	 :with-planning nil
	 :html-postamble t ; 页脚
	 :auto-preamble t
	 :section-numbers t
	 :auto-sitemap t
	 :sitemap-filename "sitemap.org" 
	 :sitemap-title ,(format "%s" org-blog-sitemap-title)
	 :sitemap-sort-files anti-chronologically
	 :sitemap-file-entry-format "%d %t"
	 :sitemap-style list
	 :sitemap-format-entry org-blog-sitemap-entry-format
	 :sitemap-function org-blog-publish-sitemap-filtered
	 :makeindex t
	 :sitemap-sort-folders: ignore)
	("index"
	 :base-directory ,(expand-file-name "./roam/permanent/" org-directory)
	 :base-extension "org"
	 :exclude ".*"
	 :include ("index.org" "about.org" "theindex.org")
	 :publishing-directory ,(expand-file-name "./public/" org-directory)
	 :recursive nil
	 :publishing-function org-html-publish-to-html
	 :completion-function ,(lambda (project) (org-publish-file (expand-file-name "theindex.org" (plist-get project :base-directory)) (cons "index" project)))
	 :html-link-home "/index.html"
	 :with-author "aRenCoco"
	 :with-email "aRen_Coco@outlook.com" 
	 :headline-levels 5
	 :with-toc nil
	 :with-creator t
	 :with-timestamp t
	 :with-planning t
	 :html-postamble t ; 页脚
	 :auto-preamble t
	 :section-numbers nil
	 :auto-sitemap nil
	 :makeindex nil)
	("static"
	 :base-directory ,(expand-file-name "./static/" org-directory)
	 :base-extension "css\\|js\\|png\\|svg\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|woff2\\|proto\\|bin"
	 :publishing-directory ,(expand-file-name "./public/" org-directory)
	 :recursive t
	 :publishing-function org-publish-attachment)
	("all"
	 :components ("post" "index" "static"))))

(provide 'init-blog-publish)
;;; init-blog-publish.el ends here. 
