;; _python.el -- Custom python configuration

;;; Commentary: Adapted from kpurdon

;; Author: Charles DeVore
;;
;; This file is not part of GNU Emacs.

;;; Code:

(elpy-enable)

(setq py-shell-name "python3")

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

(setq python-shell-completion-native-enable nil)

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).
(require 'py-autopep8)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(setq py-autopep8-options '("--max-line-length=100"))
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(define-key global-map (kbd "RET") 'newline-and-indent)


;; Set tab width to four spaces
;; (add-hook 'python-mode-hook
;;       (lambda ()
;;         (setq indent-tabs-mode t)
;;         (setq tab-width 4)
;;         (setq python-indent 4)))


;; Add support for EIN which provides notebook support
;; (require 'ein)


(provide '_python)

;;; _python.el ends here
