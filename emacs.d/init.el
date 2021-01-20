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
    fill-column-indicator
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

;; Prefer splitting window vertically
(setq split-width-threshold 160
      split-height-threshold 80)
(defun my-split-window-sensibly (&optional window)
    "replacement `split-window-sensibly' function which prefers vertical splits"
    (interactive)
    (let ((window (or window (selected-window))))
        (or (and (window-splittable-p window t)
                 (with-selected-window window
                     (split-window-right)))
            (and (window-splittable-p window)
                 (with-selected-window window
                     (split-window-below))))))

(setq split-window-preferred-function #'my-split-window-sensibly)


;; Set the line number format on the left side of the buffer
(defvar linum-format "%4d \u2502 ")
(global-linum-mode t)

;; Show the column number in the mini-buffer
(setq column-number-mode t)

;; Add the fill-column indicator
(require 'fill-column-indicator)

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


;; Set the individual development language settings in an external
;; file and directory.
(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (print fullpath)
        (load (substring fullpath 0 -3)))))))
(load-directory "~/.emacs.d/development")




;;; init.el ends here
