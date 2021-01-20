;; _python.el -- Custom python configuration

;;; Commentary: Adapted from kpurdon

;; Author: Charles DeVore
;;
;; This file is not part of GNU Emacs.

;;; Code:

;; Define and install python development packages
(defvar python-development-packages
  '(elpy
    ;; blacken
    python-black
    conda
    anaconda-mode
    company-anaconda
    py-isort
    )
  )
(mapc 'install-package-if-not-installed python-development-packages)


(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; Sort the import statements before saving
(add-hook 'before-save-hook 'py-isort-before-save)

(use-package conda
  :ensure t
  :init
  (defvar conda-anaconda-home "/opt/miniconda3/")
  (defvar conda-env-home-directory "/opt/miniconda3/")
  (defvar conda-env-autoactivate-mode t))

(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))


;; Enbable Black
;; (require 'blacken)
;; (add-hook 'python-mode-hook 'blacken-mode)
(use-package python-black
  :demand t
  :after python)
(add-hook 'python-mode-hook 'python-black-on-save-mode)


;; Set tab width to four spaces
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)))

;; Turn on the fill column indicator for Python
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (set-fill-column 80)))

;; Set the python shells
(add-hook 'python-mode-hook
          (lambda ()
            (setq python-shell-interpreter "python")
            (setq python-shell-interpreter-args "-i")))



;; Add support for EIN which provides notebook support
;; (require 'ein)


;;; _python.el ends here
