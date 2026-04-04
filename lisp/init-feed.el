;;; init-feed.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

(use-package elfeed
  :straight (:host github :repo "skeeto/elfeed")
  :config
  (setq elfeed-feeds '(("https://aren-coco.com/feed.atom" blog)
                       ("https://langxubai.com/rss.xml" blog))))

(use-package elfeed-dashboard
  :disabled
  :straight (:host github :repo "manojm321/elfeed-dashboard"))

(use-package elfeed-org
  :disabled
  :straight (:host github :repo "remyhonig/elfeed-org"))

(use-package arxiv-mode
  :straight (:host github :repo "fizban007/arxiv-mode")
  :config
  (setq arxiv-use-variable-pitch t)
  (setq arxiv-pop-up-new-frame t)
  (setq arxiv-default-category "cond-mat")
  (setq arxiv-frame-alist '((name . "*arXiv*"))))

(provide 'init-feed)
;;; init-feed.el ends here.
