;;; startup customization of UI

(defvar user-cask-file "~/.emacs.d/Cask"
  "Cask file for emacs configuration")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; when emacs is built without X frontend some features like
;; toolbar or scrollbar are unavailable; that causes
;; initialization to fail.
(mapc
 (lambda (feature)
   (when (fboundp feature)
     (funcall feature 0)))
 '(tool-bar-mode scroll-bar-mode menu-bar-mode))

(setq inhibit-splash-screen t)

;; Custom theme
(load-theme 'material t)

;; Detach the customization file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Add line numbers
(global-linum-mode t)
(setq linum-format "%4d ")

;;; custom keybindings
(global-set-key [(meta /)] 'redo)
(global-set-key (kbd "<C-tab>") 'complete-symbol)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c f u i") 'find-user-init-file)
(global-set-key (kbd "C-c f u c") 'find-user-cask-file)
(global-set-key (kbd "C-c m s") 'magit-status)

;;; Set man page width
(setenv "MANWIDTH" "72")

(mouse-avoidance-mode 'animate)

;; Follow links to VCS-controlled source files
(setq vc-follow-symlinks t)

;;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

;; utility function to quickly open init file
(defun find-user-init-file (other-window-p)
  (interactive "P")
  (let ((find-function (if other-window-p
			   #'find-file-other-window
  			   #'find-file)))
    (funcall find-function user-init-file)))

;; utility function to quickly open cask file
(defun find-user-cask-file (other-window-p)
  (interactive "P")
  (let ((find-function (if other-window-p
			   #'find-file-other-window
  			   #'find-file)))
    (funcall find-function user-cask-file)))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Elixir Lang
(add-hook 'elixir-mode-hook 'alchemist-mode)
(add-hook 'alchemist-mode-hook 'company-mode)
(add-hook 'alchemist-iex-mode-hook 'company-mode)

;; Haskell
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(add-hook 'haskell-mode-hook 'company-mode)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(setq shm-program-name "~/.local/bin/structured-haskell-mode")

;; Elisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
