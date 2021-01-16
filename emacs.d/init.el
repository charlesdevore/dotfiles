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


;; Set packages to load for the defaults and themes
(defvar theme-default-packages
  '(better-defaults
    spacemacs-theme
    )
  )

;; If the package isn't installed, install it
(defun install-package-if-not-installed (package)
  (interactive)
  (unless (package-installed-p package)
    (package-install package))
  )
(mapc 'install-package-if-not-installed theme-default-packages)


;; Set the default and alternate theme. Add a keyboard shortcut to toggle between the two themes
(setq default-theme 'spacemacs-dark)
(setq alternate-theme 'spacemacs-light)
(load-theme alternate-theme t)
(load-theme default-theme t)
(defun toggle-theme ()
  (interactive)
  (if (eq (car custom-enabled-themes) alternate-theme)
      (disable-theme alternate-theme)
    (enable-theme alternate-theme)
    )
  )
(global-set-key [f5] 'toggle-theme)



(require 'install-packages)
(require 'better-defaults)

(setq inhibit-startup-message t
      linum-format "%4d \u2502 ")

(global-linum-mode t)

;; Add MATLAB support
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)


;; modify ibuffer-formats to set name column wider
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 40 40 :left :elide) " " filename)
        (mark " "
              (name 16 -1) " " filename)))

;; (windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Git support
(require 'magit)
(global-set-key (kbd "C-c g") 'magit-status)


;; (add-hook 'after-init-hook 'global-company-mode)

;; customize company-mode
;; (setq company-require-match nil)
;; (setq company-abort (kbd "C-a"))
;; (setq company-idle-delay 0)
;; (setq company-echo-delay 0)
;; (setq company-minimum-prefix-length 1)

(require 'development)

;; Add keybindings for launching a python interactive session and a shell
(define-key global-map (kbd "C-c p") 'run-python)
(define-key global-map (kbd "C-c s") 'shell)


(defun comment-or-uncomment-region-or-line ()
    "Comment/uncomment the region or current line."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(global-set-key (kbd "M-;") 'comment-or-uncomment-region-or-line)

;; Maximize the screen on startup
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable flashing yellow exclamation mark
(setq visible-bell nil)

;; Use aspell for spell-checking
(setq ispell-program-name "/usr/local/bin/aspell")

;;; init.el ends here
