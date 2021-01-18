;;; development.el --- Load All Development Configuration

;;; Commentary:
;;; Author: Charles DeVore
;;;
;;; This file is not part of GNU Emacs.

;;; Code:

;; Define and install general development packages 
(defvar development-packages
  '(flycheck
    company
    )
  )
(mapc 'install-package-if-not-installed development-packages)

;;; init.el -- Custom Emacs Configuration

;;; Author: Charles DeVore

;;; Commentary:

;;; Code:


;; MELPA Support
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

;; Refresh the packages if there are no archives
(when (not package-archive-contents)
  (package-refresh-contents)
  )


;; Set the default directory
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; Define a function to help install packages
(defun install-package-if-not-installed (package)
  "Install PACKAGE if not already installed.

If not installed, install using default package install.  If
installed, do nothing."
  (interactive)
  (print package)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package))
  )

;; Set packages to load for the defaults and themes
(defvar theme-default-packages
  '(better-defaults
    spacemacs-theme
    use-package
    )
  )
(mapc 'install-package-if-not-installed theme-default-packages)

;; Use the better defaults
(require 'better-defaults)


;; Set the default and alternate theme
(defvar default-theme 'spacemacs-dark)
(defvar alternate-theme 'spacemacs-light)
(load-theme alternate-theme t)
(load-theme default-theme t)
(defun toggle-theme ()
  "Toggle the theme from light to dark."
  (interactive)
  (if (eq (car custom-enabled-themes) alternate-theme)
      (disable-theme alternate-theme)
    (enable-theme alternate-theme)
    )
  )
;; Add a keyboard shortcut to toggle between the two themes
(global-set-key [f5] 'toggle-theme)

;; Remove the startup message
(setq inhibit-startup-message t)

;; Set the line number format on the left side of the buffer
(defvar linum-format "%4d \u2502 ")
(global-linum-mode t)


;; (windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Git support
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)

;; Add a helper function to comment or uncomment a line or region
(defun comment-or-uncomment-region-or-line ()
    "Comment/uncomment the region or current line."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; Disable flashing yellow exclamation mark
(setq visible-bell nil)

;; Use aspell for spell-checking
(defvar ispell-program-name "/usr/local/bin/aspell")

;;; init.el ends here
(require 'use-package)

;; Setup flycheck general settings
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  )

;; (add-hook 'after-init-hook 'global-company-mode)

;; customize company-mode
;; (setq company-require-match nil)
;; (setq company-abort (kbd "C-a"))
;; (setq company-idle-delay 0)
;; (setq company-echo-delay 0)
;; (setq company-minimum-prefix-length 1)

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
