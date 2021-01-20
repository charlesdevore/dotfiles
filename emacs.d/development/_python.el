;; _python.el -- Custom python configuration

;;; Commentary: Adapted from kpurdon

;; Author: Charles DeVore
;;
;; This file is not part of GNU Emacs.

;;; Code:

;; Define and install python development packages
(defvar python-development-packages
  '(elpy
    py-autopep8
    blacken
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

;; Add autopep8 on save
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)

(use-package conda
  :ensure t
  :init
  (defvar conda-anaconda-home "/opt/miniconda3/")
  (defvar conda-env-home-directory "/opt/miniconda3/"))

(eval-after-load "company"
  '(add-to-list 'company-backends '(company-anaconda :with company-capf)))


;; (defvar python-shell-completion-native-enable nil)


;; Enbable Blacken
(require 'blacken)
(add-hook 'python-mode-hook 'blacken-mode)

;; enable autopep8 formatting on save
;; ignoring:
;; - E501 - Try to make lines fit within --max-line-length characters.
;; - W293 - Remove trailing whitespace on blank line.
;; - W391 - Remove trailing blank lines.
;; - W690 - Fix various deprecated code (via lib2to3).

(require 'py-autopep8)
(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
(setq py-autopep8-options '("--ignore=E501,W293,W391,W690"))
(setq py-autopep8-options '("--max-line-length=80"))


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
