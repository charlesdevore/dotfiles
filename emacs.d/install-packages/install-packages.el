;;; install-packages.el --- Emacs Package Installation

;; Author: Charles DeVore
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

(require 'package)

(defvar my-packages
  '(better-defaults
    company
    company-go
    cyberpunk-theme
    elpy
    exec-path-from-shell
    flycheck
    go-add-tags
    go-eldoc
    go-guru
    go-mode
    json-mode
    magit
    markdown-mode
    py-autopep8
    rainbow-delimiters
    web-mode))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))


(when (not package-archive-contents)
    (package-refresh-contents))
(package-initialize)

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(provide 'install-packages)

;;; install-packages.el ends here
