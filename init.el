;;; startup customization of UI

;; package.el initialization

(require 'package) ;; You might already have this line
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;; when emacs is built without X frontend some features like
;; toolbar or scrollbar are unavailable; that causes
;; initialization to fail. 
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(menu-bar-mode 0)

(setq inhibit-splash-screen t)

;;; Add line numbers
(global-linum-mode t)
(setq linum-format "%4d ")

;;; custom keybindings
(global-set-key [(meta /)] 'redo)
(global-set-key (kbd "<C-tab>") 'complete-symbol)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; use fuzzy completion by default
(setq ac-use-fuzzy t)

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline 1)
  (c-set-style "linux")
  (setq	c-basic-offset 4)
  (local-set-key [return] 'reindent-then-newline-and-indent))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; Set man page width
(setenv "MANWIDTH" "72")

(mouse-avoidance-mode 'animate)

;;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

(provide 'init)
