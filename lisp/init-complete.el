;;; init-complete.el -*- lexical-binding: t; -*-
;;; commentary:
;;; code:

;; 自动补全
(use-package company
  :hook 
  (after-init . global-company-mode)
)

;; (use-package yasnippet
;;   :straight (:host github :repo "joaotavora/yasnippet")
;;   :init (yas-global-mode 1)
;; )

(use-package aas
  :straight (:host github :repo "ymarco/auto-activating-snippets")
  :hook 
  (LaTeX-mode . aas-activate-for-major-mode)
  (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
    ;; expand unconditionally
    ";o-" "ō"
    ";i-" "ī"
    ";a-" "ā"
    ";u-" "ū"
    ";e-" "ē"
  )
  (aas-set-snippets 'org-mode
    ";abst" (lambda () (interactive) (insert "#+BEGIN_abstract\n\n#+END_abstract\n") (forward-line -2) )
    ";comm" (lambda () (interactive) (insert "#+BEGIN_COMMENT\n\n#+END_COMMENT\n") (forward-line -2) )
    ";eq" (lambda () (interactive) (insert "\\\(  \\\)") (forward-char -3) )
    ";ee" (lambda () (interactive) (insert "\\begin{equation}\n\n\\end{equation}\n") (forward-line -2) )
    ";ea" (lambda () (interactive) (insert "\\begin{align}\n\n\\end{align}\n") (forward-line -2) )
    ";es" (lambda () (interactive) (insert "\\begin{split}\n\n\\end{split}\n") (forward-line -2) ) 
  )
)

(use-package laas
  :straight (:host github :repo "tecosaur/LaTeX-auto-activating-snippets")
  :init
  (setq laas-basic-snippets
    '(
      ";;alp" "\\alpha"
      ";;bet" "\\beta"
      ";;gam" "\\gamma"
      ";;del" "\\delta"
      ";;eps" "\\epsilon"
      ";;veps" "\\varepsilon"
      ";;zet" "\\zeta"
      ";;eta" "\\eta"
      ";;the" "\\theta"
      ";;vthe" "\\vartheta"
      ";;iot" "\\iota"
      ";;kap" "\\kappa"
      ";;lam" "\\lambda"
      ";;mu" "\\mu"
      ";;nu" "\\nu"
      ";;xi" "\\xi"
      ";;pi" "\\pi"
      ";;vpi" "\\varpi"
      ";;rho" "\\rho"
      ";;vrho" "\\varrho"
      ";;sig" "\\sigma"
      ";;vsig" "\\varsigma"
      ";;tau" "\\tau"
      ";;ups" "\\upsilon"
      ";;phi" "\\phi"
      ";;vphi" "\\varphi"
      ";;chi" "\\chi"
      ";;psi" "\\psi"
      ";;ome" "\\omega"
      ";;cgam" "\\Gamma"
      ";;vcgam" "\\varGamma"
      ";;cdel" "\\Delta"
      ";;vcdel" "\\varDelta"
      ";;cthe" "\\Theta"
      ";;vcthe" "\\varTheta"
      ";;clam" "\\Lambda"
      ";;vclam" "\\varLambda"
      ";;cxi" "\\Xi"
      ";;vcxi" "\\varXi"
      ";;cpi" "\\Pi"
      ";;vcpi" "\\varPi"
      ";;csig" "\\Sigma"
      ";;vcsig" "\\varSigma"
      ";;cups" "\\Upsilon"
      ";;vcups" "\\varUpsilon"
      ";;cphi" "\\Phi"
      ";;vcphi" "\\varPhi"
      ";;cpsi" "\\Psi"
      ";;vcpsi" "\\varPsi"
      ";;come" "\\Omega"
      ";;vcome" "\\varOmega"
      ";;ale" "\\aleph"
      ";;bet" "\\beth"
      ";;dal" "\\daleth"
      ";;gim" "\\gimel"
    )
  )
  ;; (setq laas-subscript-snippets nil)
  ;; (setq laas-frac-snippet nil)
  ;; (setq laas-accent-snippets nil)
  :config
  (aas-set-snippets 'laas-mode
    ";label" (lambda () (interactive) (yas-expand-snippet "\\label{$1:$2}\n$0"))
    ";uniti" "\\mathrm{i}"
    ";hom" "\\Hom"
    ";end" "\\End"
    ";id" "\\id"
    ";ker" "\\ker"
    ";im" "\\im"
    ";tr" "\\tr"
    ";mrm" "\\mathrm"
    ";mca" "\\mathcal"
    ";mbb" "\\mathbb"
    ";mtt" "\\mathtt"
    ";mfr" "\\mathfrak"
    ";times" "\\times"
    ";sum" (lambda () (interactive) (yas-expand-snippet "\\sum_{ $1 }^{ $2 } $0"))
    ";int" (lambda () (interactive) (yas-expand-snippet "\\int_{ $1 }^{ $2 } $0"))
    ";prod" (lambda () (interactive) (yas-expand-snippet "\\prod_{ $1 }^{ $2 } $0"))
    ";otimes" (lambda () (interactive) (yas-expand-snippet "\\bigotimes_{ $1 }^{ $2 } $0"))
    ";pp" (lambda () (interactive) (yas-expand-snippet "\\ab( $1 )$0"))
    ";ss" (lambda () (interactive) (yas-expand-snippet "\\ab[ $1 ]$0"))
    ";bb" (lambda () (interactive) (yas-expand-snippet "\\ab\\\\{ $1 \\\\}$0"))
    ";aa" (lambda () (interactive) (yas-expand-snippet "\\ab< $1 >$0"))
    ";vv" (lambda () (interactive) (yas-expand-snippet "\\ab| $1 |$0"))
    ";ket" (lambda () (interactive) (yas-expand-snippet "\\ket| $1 >$0"))
    ";bra" (lambda () (interactive) (yas-expand-snippet "\\bra< $1 |$0"))
    ";bk" (lambda () (interactive) (yas-expand-snippet "\\braket< $1 | $2 >$0"))
    ";bok" (lambda () (interactive) (yas-expand-snippet "\\braket< $1 | $3 | $2 >$0"))
    ";eval" (lambda () (interactive) (yas-expand-snippet "\\eval{ $1 }_{ $2 }^{ $3 }$0"))
    ";frac" (lambda () (interactive) (yas-expand-snippet "\\frac{ $1 }{ $2 }$0"))
    ";sqrt" (lambda () (interactive) (yas-expand-snippet "\\sqrt{ $1 }$0"))
  )
  :hook
  (LaTeX-mode . laas-mode)
  (LaTeX-mode . yas-minor-mode)
  (org-mode . laas-mode)
  (org-mode . yas-minor-mode)
)

(defun load/init-complete ()
  (interactive) 
  (load-file (expand-file-name "./lisp/init-complete.el" user-emacs-directory))
)

(provide 'init-complete)
;;; init-complete.el ends here
