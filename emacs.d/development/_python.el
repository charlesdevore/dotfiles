;; _python.el -- Custom python configuration

;;; Commentary: Adapted from kpurdon

;; Author: Charles DeVore
;;
;; This file is not part of GNU Emacs.

;;; Code:

;; Define and install python development packages
(setq python-development-packages
  '(elpy
    py-autopep8
    blacken
    conda
    anaconda-mode
    company-anaconda
    )
  )
(mapc 'install-package-if-not-installed python-development-packages)


;; (elpy-enable)

(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Add Company Jedi support
;; (require 'company-jedi)
;; (defun my/python-mode-company-jedi-hook ()
;;   (add-to-list 'company-backends 'company-jedi))
;; (add-hook 'python-mode-hook 'my/python-mode-company-jedi-hook)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

;; (use-package company-jedi
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends 'company-jedi))

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home "/opt/miniconda3/")
  (setq conda-env-home-directory "/opt/miniconda3/"))

(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))

;; (add-hook 'conda-postactivate-hook 'jedi:stop-server)
;; (add-hook 'conda-postdeactivate-hook 'jedi:stop-server)

;; (defvar py-shell-name "python3")

;; (defvar python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i --simple-prompt")

;; (defvar python-shell-completion-native-enable nil)

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).

;; (require 'py-autopep8)
;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;; (setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
;; (setq py-autopep8-options '("--max-line-length=100"))

;; (define-key global-map (kbd "RET") 'newline-and-indent)


;; Set tab width to four spaces
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent 4)))


;; Add support for EIN which provides notebook support
;; (require 'ein)


(provide '_python)

;;; _python.el ends here
