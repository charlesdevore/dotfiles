;;; development.el --- Load All Development Configuration

;;; Author: Charles DeVore

;;; Commentary:

;;; Code:


;; Define and install general development packages
(defvar development-packages
  '(flycheck
    company
    )
  )
(mapc 'install-package-if-not-installed development-packages)

(require 'use-package)

;; Setup flycheck general settings
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

(add-hook 'after-init-hook 'global-company-mode)

;; customize company-mode
(require 'company)
(global-company-mode)
(setq company-require-match nil)
;; (setq company-abort (kbd "C-a"))
(setq company-idle-delay 0)
(setq company-echo-delay 0)
(setq company-minimum-prefix-length 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require '_python)
;; (require '_golang)
;; (require '_markdown)
;; (require '_web)
;; (require '_json)

(provide 'development)

;;; development.el ends here
