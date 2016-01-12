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

;; Get rid of annoying backup files in-place
(setq backup-directory-alist `(("." . "~/.emacs.d/backup")))

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
(global-set-key (kbd "C-c t l d r") 'tldr)
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

;; Javascript
(setq flycheck-disabled-checkers '(javascript-jshint))
(setq flycheck-checkers '(javascript-eslint))

;; Org Mode
(eval-after-load 'org
  '(setq org-default-notes-file (concat org-directory "/notes.org")))
(define-key global-map "\C-cc" 'org-capture)

;; Use Ido
(setq ido-auto-merge-work-directories-length -1)
(ido-mode 1)
(ido-everywhere t)

;; Handle whitespace
(setq whitespace-line-column 120)
(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun nameless/file-make-executable (file)
  "Make file FILE executable"
  (interactive "fSelect file: ")
  (let* ((file-modes (file-modes file))
	 (new-modes (logior #o100 file-modes)))
    (set-file-modes file new-modes)
    (message "Set mode 0%s (octal) for file `%s'" new-modes file)))

(defun nameless/file-make-executable-if-shebang ()
  "Make the file associated with the current buffer executable if it has shebang"
  (and buffer-file-name
       (save-excursion
	 (save-restriction
	   (widen)
	   (goto-char (point-min))
	   (when (and (looking-at auto-mode-interpreter-regexp)
		      (not (file-executable-p buffer-file-name)))
	     (nameless/file-make-executable buffer-file-name))))))

(defun nameless/set-auto-mode ()
  "Call `set-auto-mode' if `major-mode' is fundamental-mode"
  (interactive)
  (when (equal major-mode 'fundamental-mode)
    (set-auto-mode t)))

;; Make files with shebang executable
(add-hook 'after-save-hook 'nameless/file-make-executable-if-shebang)

;; Select auto mode for new files
(add-hook 'after-save-hook 'nameless/set-auto-mode)
