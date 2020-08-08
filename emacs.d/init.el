;;; init.el -- Custom Emacs Configuration

;;; Author: Charles DeVore

;;; Commentary:

;;; Code:

(package-initialize)
;; (when (not package-archive-contents)
;;   (package-refresh-contents))
;; (package-initialize)

(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

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

(load-theme 'cyberpunk t)
;; (windmove-default-keybindings)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Git support
;;(require 'magit)
;;(global-set-key (kbd "C-c g") 'magit-status)


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
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; init.el ends here
