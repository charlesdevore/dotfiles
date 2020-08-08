;;; development.el --- Load All Development Configuration

;;; Commentary:
;;; Author: Charles DeVore
;;;
;;; This file is not part of GNU Emacs.

;;; Code:

(require 'rainbow-delimiters)

;; (global-flycheck-mode)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'subword-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

(require '_python)
;; (require '_golang)
;; (require '_markdown)
;; (require '_web)
;; (require '_json)

(provide 'development)

;;; development.el ends here
