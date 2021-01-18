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
  "Take the package name as input. If not installed,
install using default package install. If installed,
 do nothing."
  (interactive)
  (unless (package-installed-p package)
    (package-install package))
  )

;; Set packages to load for the defaults and themes
(defvar theme-default-packages
  '(better-defaults
    spacemacs-theme
    )
  )
(mapc 'install-package-if-not-installed theme-default-packages)

;; Use the better defaults
(require 'better-defaults)


;; Set the default and alternate theme
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
;; Add a keyboard shortcut to toggle between the two themes
(global-set-key [f5] 'toggle-theme)

;; Remove the startup message
(setq inhibit-startup-message t)

;; Set the line number format on the left side of the buffer
(setq linum-format "%4d \u2502 ")
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
(setq ispell-program-name "/usr/local/bin/aspell")


;; Set the individual development language settings in an external
;; file and directory.
(require 'development)


;;; init.el ends here
