;;; init-help.el -*- lexical-binding: t; -*-
;;; commentary:

;; 参考了https://emacs-china.org/t/helpful-el/8153

;;; code:

(use-package helpful
  :straight (:host github :repo "Wilfred/helpful")
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h s" . helpful-symbol))
  :config
  (setq helpful-max-buffers 5)
  ;; don't pop new window
  (setq helpful-switch-buffer-function
        (lambda (buf) (if-let ((window (display-buffer-reuse-mode-window buf '((mode . helpful-mode)))))
                          ;; ensure the helpful window is selected for `helpful-update'.
                          (select-window window)
                        ;; line above returns nil if no available window is found
                        (pop-to-buffer buf))))
  (defvar help-helpful/history () "History of helpful, a list of buffers.")
  (defun help-helpful--switch-to-buffer  (buffer &optional offset)
    "Jump to last SYMBOL in helpful history, offset by OFFSET."
    (interactive)
    (require 'seq)
    (require 'cl-lib)
    (setq help-helpful/history (seq-remove (lambda (buf) (not (buffer-live-p buf))) help-helpful/history))
    (cl-labels ((find-index (elt lst)
                  (let ((idx 0)
                        (len (length lst)))
                    (while (and (not (eq elt (nth idx lst)))
                                (not (eq idx len)))
                      (setq idx (1+ idx)))
                    (if (eq idx len)
                        nil
                      idx))))
      (let ((idx (+ (or offset 0) (find-index buffer help-helpful/history))))
	(if (or (>= idx (length help-helpful/history))
		(< idx 0))
            (message "No further history.")
          (switch-to-buffer (nth idx help-helpful/history))))))
  (defun help-helpful--update (oldfunc)
    "Insert back/forward buttons."
    (funcall oldfunc)
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (insert-text-button "Back"
                          'action (lambda (&rest _)
                                    (interactive)
                                    (help-helpful--switch-to-buffer  (current-buffer) 1)))
      (insert " | ")
      (insert-text-button "Forward"
                          'action (lambda (&rest _)
                                    (interactive)
                                    (help-helpful--switch-to-buffer  (current-buffer)  -1)))
      (insert "\n\n")))
  (advice-add #'helpful-update :around #'help-helpful--update)
  (advice-add #'helpful--buffer :around (lambda (oldfunc &rest _)
                                          (let ((buf (apply oldfunc _)))
                                            (push buf help-helpful/history)
                                            buf))))

(provide 'init-help)
;;; init-basic.el ends here.
