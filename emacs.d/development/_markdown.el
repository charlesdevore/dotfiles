;; _markdown.el -- custom markdown configuration

;;; Commentary:

;; Author: Charles DeVore
;;
;; This file is not part of GNU Emacs.

;;; Code:

;; Define and install markdown packages
(defvar markdown-development-packages
  '(markdown-mode+
    markdown-preview-mode
    )
  )
(mapc 'install-package-if-not-installed markdown-development-packages)


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc -t html --mathjax"
              markdown-enable-math t)
  )


;;; _markdown.el ends here
