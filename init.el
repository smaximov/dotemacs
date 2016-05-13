;;; init.el --- Emacs configuration

;;; Commentary:

;; See https://github.com/smaximov/dotemacs

;;; Code:
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

;;; Macros and utilities

(defmacro with-daemon (&rest body)
  "Execute BODY immediately when Emacs started in non-daemon mode.
If Emacs started as daemon, delay the execution of BODY until a new
frame is created."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (when (window-system frame)
                       ,@body))))
    (when (window-system)
      ,@body)))

;;; Packages configuration
(use-package diminish
  :ensure t
  :demand t)

(use-package async
  :ensure t
  :demand t)

(use-package org-page
  :load-path "lib/org-page"
  :config
  (setf op/site-preview-directory "/tmp/org-page-preview"
        op/repository-directory "~/src/maximov.space"

        op/site-domain "https://maximov.space"
        op/site-main-title "Untitled"
        op/site-sub-title "Emacs, Programming, and Anything"

        op/highlight-render 'htmlize

        op/theme 'mdo
        op/theme-root-directory "~/src/maximov.space/themes"

        op/personal-disqus-shortname "smaximov"
        op/personal-github-link "https://github.com/smaximov"
        op/personal-google-analytics-id "UA-74709646-1"))

(use-package material-theme
  :ensure t
  :demand t
  :config
  (with-daemon
   (load-theme 'material t)))

(use-package dockerfile-mode
  :ensure t)

(use-package tern
  :ensure t
  :after js2
  :diminish tern-mode
  :init
  (--each '(tern-mode eldoc-mode)
    (add-hook 'js-mode-hook it)))

(use-package company-tern
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package magit
  :ensure t
  :bind ("C-c m s" . magit-status)
  :config
  (setf git-commit-summary-max-length 100))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml$")

(use-package dash
  :ensure t
  :demand t)

(use-package mustache-mode
  :ensure t)

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

(use-package helm-projectile
  :ensure t
  :after (helm projectile)
  :bind ([remap projectile-switch-project] . helm-projectile-switch-project))

(use-package whitespace
  :ensure t
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'markdown-mode-hook #'whitespace-mode)
  :config
  (setf whitespace-line-column 120))

(use-package exec-path-from-shell
  :ensure t
  :demand t
  :config
  (add-to-list 'exec-path-from-shell-variables "CARGO_HOME")
  (add-to-list 'exec-path-from-shell-variables "RUSTUP_HOME")
  (with-daemon
   (exec-path-from-shell-initialize)))

(use-package tldr
  :ensure t
  :bind ("C-c t l d r" . tldr))

(use-package elixir-mode
  :ensure t)

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
  :mode "\\.cargo/config$"
  :init
  (add-hook 'toml-mode-hook (lambda ()
                              (let* ((path (buffer-file-name))
                                     (file (and path
                                                (f-filename path)))
                                     (is-cargo.toml (and file
                                                    (s-equals? "Cargo.toml" file))))
                                (when is-cargo.toml
                                        (cargo-minor-mode)))))
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :bind ([C-tab] . company-indent-or-complete-common)
  :config
  (setf company-tooltip-align-annotations t
        company-idle-delay 0.05
        company-minimum-prefix-length 1))

(use-package bookmark+
  :ensure t)

(use-package org
  :ensure t
  :after htmlize
  :config
  (setf org-default-notes-file (f-join org-directory "notes.org")
        org-src-fontify-natively t)
  :bind (("C-c c" . org-capture)
         ("C-c l" . org-store-link)))

(use-package htmlize
  :ensure t
  :defer t)

(use-package emr
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'emr-initialize)
  :bind ([M-return] . emr-show-refactor-menu))

(use-package json-mode
  :ensure t
  :config
  (setf js-indent-level 2))

(use-package js2-mode
  :ensure t
  :mode "\\.js$"
  :init
  (add-hook 'js2-mode-hook #'electric-pair-mode)
  :config
  (setf js2-strict-missing-semi-warning nil
        js2-basic-offset 2))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md$" "\\.markdown$")
  :config
  (setf markdown-command "kramdown"))

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  :bind (([remap execute-extended-command] . helm-M-x) ; M-x
         ([remap yank-pop] . helm-show-kill-ring)      ; M-y
         ([remap list-buffers] . helm-mini)            ; C-x C-b
         ([remap switch-to-buffer] . helm-mini)        ; C-x b
         ([remap find-file] . helm-find-files)         ; C-x C-f
         ([remap occur] . helm-occur)))                ; M-s o

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
  :preface
  ;; Get rid of "assignment to free variable `css-indent-offset'" warning
  (defvar css-indent-offset)
  :init
  (add-hook 'scss-mode-hook #'whitespace-mode)
  (add-hook 'scss-mode-hook (lambda ()
                              (setf comment-start "//"
                                    comment-end "")))
  :config
  (setf css-indent-offset 2)
  (setf require-final-newline t))

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook #'electric-pair-mode))

(use-package cargo
  :load-path "lib/cargo"
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
  (setf racer-rust-src-path (f-full "~/src/rust/src")
        racer-cmd (executable-find "racer")))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'after-init-hook #'yas-global-mode)
  :config
  (setf yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package rvm
  :ensure t
  :init
  (add-hook 'after-init-hook #'rvm-use-default))

(use-package paren
  :ensure t
  :init
  (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setf show-paren-delay 0
        show-paren-style 'mixed))

(use-package navigation
  :load-path "lib"
  :demand t
  :bind (("C-c f u i" . nameless/find-user-init-file)
         ("C-c f s" . nameless/find-scratch-buffer)
         ("C-c f d f" . nameless/find-dominating-file)))

(use-package file-helpers
  :load-path "lib"
  :init
  (add-hook 'after-save-hook #'nameless/file-make-executable-if-shebang)
  (add-hook 'after-save-hook #'nameless/set-auto-mode))

(use-package term
  :ensure t
  :after navigation
  :init
  (add-hook 'term-mode-hook (lambda ()
                              (setf yas-dont-activate-functions t)))
  :bind
  (("C-x t" . ansi-term)
   ("C-x C-t" . nameless/find-term-buffer)))

(use-package inf-ruby
  :ensure t
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :config
  (setf projectile-switch-project-action 'neotree-projectile-action))

(use-package multiple-cursors
  :ensure t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package phi-search
  :ensure t
  :init
  (require 'phi-replace)
  :bind
  (([remap isearch-forward] . phi-search)
   ([remap isearch-backward] . phi-search-backward)
   ([remap query-replace] . phi-replace)))

(use-package darkroom
  :ensure t)

(use-package sh-mode
  :ensure sh-script
  :mode "\\.zsh$"
  :mode "\\.zsh-theme$")

;;; Custom commands

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

;; Follow links to VCS-controlled source files
(setf vc-follow-symlinks t)

;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

;; Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)

(put 'narrow-to-region 'disabled nil)

;; It's not like we are 800x600 nowadays
(setq-default fill-column 120)

;; Handle whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Credentials
(setf user-full-name "smaximov"
      user-mail-address "s.b.maximov@gmail.com")

(provide 'init)
;;; init.el ends here
