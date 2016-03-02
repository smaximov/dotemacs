(require 'package)
(setf package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; Packages configuration
(use-package diminish
  :ensure t
  :demand t)

(use-package async
  :ensure t
  :demand t)

(use-package material-theme
  :if window-system
  :ensure t
  :demand t
  :config
  (load-theme 'material t))

(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status))

(use-package dash
  :ensure t
  :demand t)

(use-package f
  :ensure t
  :demand t)

(use-package s
  :ensure t
  :demand t)

(use-package projectile
  :ensure t
  :init
  (add-hook 'after-init-hook #'projectile-global-mode))

(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  :config
  (setf whitespace-line-column 120))

(use-package exec-path-from-shell
  :if window-system
  :config
  (exec-path-from-shell-initialize))

(use-package ido
  :ensure t
  :config
  (setf ido-auto-merge-work-directories-length -1)
  (ido-mode 1)
  (ido-everywhere t))

(use-package tldr
  :ensure t
  :bind ("C-c t l d r" . tldr))

(use-package elixir-mode
  :ensure t)

(use-package alchemist
  :ensure t
  :after (elixir-mode company-mode)
  :init
  (add-hook 'elixir-mode-hook #'alchemist-mode))

(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setf flycheck-disabled-checkers '(javascript-jshint)))

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package flycheck-haskell
  :ensure t
  :after (flycheck haskell-mode)
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package toml-mode
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :bind ([C-tab] . company-indent-or-complete-common)
  :config
  (setf company-tooltip-align-annotations t))

(use-package bookmark+
  :ensure t)

(use-package org
  :ensure t
  :after htmlize
  :config
  (setf org-default-notes-file (f-join org-directory "notes.org")
        org-src-fontify-natively t)
  :bind ("C-c c" . org-capture))

(use-package htmlize
  :ensure t
  :defer t)

(use-package emr
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'emr-initialize)
  :bind ([M-return] . emr-show-refactor-menu))

(use-package json-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :init
  (add-hook 'js2-mode-hook #'electric-pair-mode)
  :config
  (setf js2-basic-offset 2
        js2-strict-missing-semi-warning nil))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" "\\.markdown$"))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-b" . helm-mini)
         ("M-s o" . helm-occur)))

(use-package eldoc
  :ensure t
  :diminish eldoc-mode
  :after dash
  :init
  (--each '(emacs-lisp-mode-hook ielm-mode-hook)
    (add-hook it #'eldoc-mode)))

(use-package paredit
  :ensure t
  :after (dash eldoc)
  :init
  (--each '(emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook ielm-mode-hook
                                 lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook)
    (add-hook it #'enable-paredit-mode))
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(use-package clojure-mode
  :ensure t
  :after (eldoc paredit)
  :init
  (--each (list #'enable-paredit-mode #'eldoc-mode)
    (add-hook 'clojure-mode-hook it)))

(use-package cider
  :ensure t
  :after (clojure-mode eldoc paredit)
  :init
  (--each (list #'enable-paredit-mode #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook it))
  :config
  (setf cider-lein-command (f-full "~/bin/lein")))

(use-package scss-mode
  :ensure t
  :after whitespace
  :init
  (add-hook 'scss-mode-hook #'whitespace-mode)
  :config
  (setf css-indent-offset 2))

(use-package rust-mode
  :ensure t
  :bind (:map rust-mode-map
              ("C-c f c" . nameless/find-cargo-file)))

(use-package cargo
  :ensure t
  :diminish cargo-minor-mode
  :after rust-mode
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(use-package racer
  :ensure t
  :diminish racer-mode
  :after (rust-mode f eldoc)
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :config
  (setf racer-rust-src-path (f-full "~/src/rust/src")))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'after-init-hook #'yas-global-mode))

(use-package rvm
  :ensure t
  :init
  (add-hook 'after-init-hook #'rvm-use-default))

;;; Custom commands
(defun nameless/dispatch-by-prefix-arg (prefix-present-fun prefix-absent-fun &rest args)
  "Choose function based on the presence of prefix argument."
  (apply (if current-prefix-arg prefix-present-fun prefix-absent-fun) args))

(defun nameless/find-scratch-buffer ()
  "Switch to the scratch buffer (create one if necessary).

With prefix argument, switch to the scratch buffer in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'switch-to-buffer-other-window #'switch-to-buffer
                                   "*scratch*"))

(defun nameless/find-user-init-file ()
  "Find user's initialization file.

With prefix argument, open the file in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                   user-init-file))

(defun nameless/find-user-cask-file ()
  "Find user's cask file.

With prefix argument, open the file in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                   user-cask-file))

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
                      (not (f-executable? buffer-file-name)))
             (nameless/file-make-executable buffer-file-name))))))

(defun nameless/set-auto-mode ()
  "Call `set-auto-mode' if `major-mode' is fundamental-mode"
  (interactive)
  (when (equal major-mode 'fundamental-mode)
    (set-auto-mode t)))

(defun nameless/narrow-to-region-in-indirect-buffer (start end name)
  "Narrow to the current region in indirect buffer

NAME is the name of new buffer. It's also used to determine
suitable major mode according to `auto-mode-alist'"
  (interactive "r\nsBuffer name: ")
  (deactivate-mark)
  (let ((buffer (clone-indirect-buffer name nil))
        (switch (if current-prefix-arg #'switch-to-buffer-other-window #'switch-to-buffer)))
    (with-current-buffer buffer
      (funcall switch buffer)
      (narrow-to-region start end)
      ;; Try to guess major mode from indirect buffer's file name
      (when name
        (let ((mode (assoc-default name auto-mode-alist 'string-match)))
          (when (and mode
                     (not (equal mode major-mode)))
            (funcall mode)))))))

(defun nameless/find-cargo-file ()
  "Find project's Cargo.toml file.

With prefix argument, find the file in other window."
  (interactive)
  (-if-let (crate-root (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                       (f-expand "Cargo.toml" crate-root))
    (error "No `Cargo.toml` found")))

;;; Other configuration

;; when emacs is built without X frontend some features like
;; toolbar or scrollbar are unavailable; that causes
;; initialization to fail. To avoid that, check if the feature is present first
(--each '(tool-bar-mode scroll-bar-mode menu-bar-mode)
  (when (fboundp it)
    (funcall it 0)))

;; Start Emacs maximized
(toggle-frame-maximized)

;; Indent using spaces
(setq-default indent-tabs-mode nil)

;; Don't show splash screen at startup
(setf inhibit-splash-screen t)

;; Get rid of annoying backup files stored in-place
(setf backup-directory-alist `(("." . "~/.emacs.d/backup")))

;; Display current column position of the cursor
(add-hook 'after-init-hook #'column-number-mode)

;; Track recent files
(add-hook 'after-init-hook #'recentf-mode)

;; Detach the customization file
(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; custom keybindings
(global-set-key (kbd "C-c f u i") #'nameless/find-user-init-file)
(global-set-key (kbd "C-c f u c") #'nameless/find-user-cask-file)
(global-set-key (kbd "C-c f s") #'nameless/find-scratch-buffer)
(global-set-key (kbd "C-x n i") #'nameless/narrow-to-region-in-indirect-buffer)

;; Follow links to VCS-controlled source files
(setf vc-follow-symlinks t)

;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

;; Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)

;; Make files with shebang executable
(add-hook 'after-save-hook #'nameless/file-make-executable-if-shebang)

;; Select auto mode for new files
(add-hook 'after-save-hook #'nameless/set-auto-mode)

(put 'narrow-to-region 'disabled nil)

;; It's not like we are 800x600 nowadays
(setf fill-column 120)

;; Highlight matching parentheses
(setf show-paren-delay 0
      show-paren-style 'mixed)
(add-hook 'after-init-hook #'show-paren-mode)

;; Handle whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
