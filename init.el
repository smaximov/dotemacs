;;; init.el --- Emacs configuration; -*- lexical-binding: t -*-

;;; Commentary:

;; See https://github.com/smaximov/dotemacs

;;; Code:
(require 'package)
(setf package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap req-package
(add-to-list 'load-path (concat user-emacs-directory "/lib/req-package"))

(require 'req-package)

;; Save PID file when running as a daemon
(when (daemonp)
  (require 'server)
  (defvar emacs-pid-dir (or (getenv "XDG_RUNTIME_DIR")
                            server-socket-dir
                            temporary-file-directory))
  (defvar emacs-pid-file (format "%s/%s" emacs-pid-dir "emacs.pid"))

  ;; write PID file on startup
  (add-hook 'emacs-startup-hook
            (lambda ()
              (make-directory emacs-pid-dir t)
              (with-temp-file emacs-pid-file
                (insert (number-to-string (emacs-pid))))))

  ;; remove PID file on exit
  (add-hook 'kill-emacs-hook
            (lambda ()
              (when (file-exists-p emacs-pid-file)
                (delete-file emacs-pid-file)))))

;;; Macros and utilities

(defmacro with-daemon (&rest body)
  "Execute BODY immediately when Emacs started in non-daemon mode.
If Emacs started as daemon, delay the execution of BODY until a new
frame is created."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     ;; (when (window-system frame)
                     ;;   ,@body)
                     ,@body
                     )))
    (when (window-system)
      ,@body)))

;;; Packages configuration
(req-package diminish)

(req-package async)

(req-package dash)

(req-package simple-httpd)

(req-package git)

(req-package mustache)

(req-package org-page :loader :path
  :load-path "lib/org-page"
  :if (file-exists-p (format "%s/lib/org-page/org-page.el" user-emacs-directory))
  :require dash ht simple-httpd git mustache
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

(req-package material-theme
  :config
  (with-daemon
   (load-theme 'material t)))

(req-package dockerfile-mode)

(req-package tern
  :require js2-mode dash
  :diminish tern-mode
  :init
  (--each '(tern-mode eldoc-mode)
    (add-hook 'js-mode-hook it)))

(req-package company-tern
  :require company
  :config
  (add-to-list 'company-backends 'company-tern))

(req-package editorconfig
  :init
  (add-hook 'after-init-hook #'editorconfig-mode))

(req-package magit
  :bind ("C-c m s" . magit-status)
  :if (version<= "24.4" emacs-version)
  :init
  (add-hook 'after-init-hook #'global-magit-file-mode)
  :config
  (setf git-commit-summary-max-length 100))

(req-package yaml-mode
  :mode "\\.yml$")

(req-package mustache-mode)

(req-package f)

(req-package s)

(req-package projectile
  :init
  (add-hook 'after-init-hook #'projectile-global-mode))

(req-package helm-projectile
  :require projectile
  :bind ([remap projectile-switch-project] . helm-projectile-switch-project))

(req-package whitespace
  :require diminish
  :diminish whitespace-mode
  :init
  (add-hook 'prog-mode-hook #'whitespace-mode)
  (add-hook 'markdown-mode-hook #'whitespace-mode)
  :config
  (setf whitespace-line-column 120))

(req-package exec-path-from-shell
  :config
  (add-to-list 'exec-path-from-shell-variables "CARGO_HOME")
  (add-to-list 'exec-path-from-shell-variables "RUSTUP_HOME")
  (with-daemon
   (exec-path-from-shell-initialize)))

(req-package tldr
  :bind ("C-c t l d r" . tldr))

(req-package elixir-mode)

(req-package flycheck
  :init
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setf flycheck-disabled-checkers '(javascript-jshint)))

(req-package haskell-mode
  :init
  (add-hook 'haskell-mode-hook #'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(req-package flycheck-haskell
  :require flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(req-package toml-mode
  :require f
  :mode "\\.cargo/config$"
  :init
  (add-hook 'toml-mode-hook (lambda ()
                              (let* ((path (buffer-file-name))
                                     (file (and path
                                                (f-filename path)))
                                     (is-cargo.toml (and file
                                                    (s-equals? "Cargo.toml" file))))
                                (when is-cargo.toml
                                        (cargo-minor-mode))))))

(req-package company
  :require diminish
  :diminish company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  :bind ([C-tab] . company-indent-or-complete-common)
  :config
  (setf company-tooltip-align-annotations t
        company-idle-delay 0.05
        company-minimum-prefix-length 1))

(req-package bookmark+)

(req-package org
  :require htmlize f
  :config
  (setf org-directory "~/share/owncloud/org"
        org-default-notes-file (f-join org-directory "notes.org")
        org-agenda-files `(,org-default-notes-file)
        org-src-fontify-natively t
        org-enforce-todo-checkbox-dependencies t
        org-enforce-todo-dependencies t
        org-agenda-dim-blocked-tasks t
        org-archive-location (f-join org-directory "archive.org::* From %s")
        org-log-done 'time

        ;; Don't align a node's content with the headline
        org-adapt-indentation nil

        ;; Custom agenda views
        org-agenda-custom-commands '(("h" "Agenda and home-related chores"
                                      ((agenda)
                                       (tags-todo "home"
                                                  ((org-agenda-sorting-strategy '(priority-down)))))))

        org-goto-auto-isearch nil)
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c f a" . org-cycle-agenda-files)
         ("C-c l" . org-store-link)))

(req-package htmlize)

(req-package emr
  :init
  (add-hook 'prog-mode-hook #'emr-initialize)
  :bind ([M-return] . emr-show-refactor-menu))

(req-package json-mode
  :config
  (setf js-indent-level 2))

(req-package js2-mode
  :mode "\\.js$"
  :init
  (add-hook 'js2-mode-hook #'electric-pair-mode)
  :config
  (setf js2-strict-missing-semi-warning nil
        js2-basic-offset 2
        js2-strict-trailing-comma-warning nil))

(req-package markdown-mode
  :mode ("\\.md$" "\\.markdown$")
  :config
  (setf markdown-command "kramdown"))

(req-package helm
  :init
  (require 'helm-config)
  :bind (([remap execute-extended-command] . helm-M-x) ; M-x
         ([remap yank-pop] . helm-show-kill-ring)      ; M-y
         ([remap list-buffers] . helm-mini)            ; C-x C-b
         ([remap switch-to-buffer] . helm-mini)        ; C-x b
         ([remap find-file] . helm-find-files)         ; C-x C-f
         ([remap occur] . helm-occur)))                ; M-s o

(req-package eldoc
  :require diminish dash
  :diminish eldoc-mode
  :init
  (--each '(emacs-lisp-mode-hook ielm-mode-hook)
    (add-hook it #'eldoc-mode)))

(req-package paredit
  :require dash eldoc
  :init
  (--each '(emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook ielm-mode-hook
                                 lisp-mode-hook lisp-interaction-mode-hook scheme-mode-hook)
    (add-hook it #'enable-paredit-mode))
  :config
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(req-package clojure-mode
  :require eldoc paredit
  :init
  (--each (list #'enable-paredit-mode #'eldoc-mode)
    (add-hook 'clojure-mode-hook it)))

(req-package cider
  :require eldoc paredit f
  :init
  (--each (list #'enable-paredit-mode #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook it))
  :config
  (setf cider-lein-command (f-full "~/bin/lein")))

(req-package scss-mode
  :require whitespace
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

(req-package rust-mode
  :init
  (add-hook 'rust-mode-hook #'electric-pair-mode))

(req-package cargo
  :require rust-mode diminish
  :diminish cargo-minor-mode
  :init
  (add-hook 'rust-mode-hook #'cargo-minor-mode))

(req-package racer
  :require rust-mode f eldoc
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  :config
  (setf racer-rust-src-path (f-full "~/src/rust/src")
        racer-cmd (executable-find "racer")))

(req-package flycheck-rust
  :require flycheck
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(req-package yasnippet
  :init
  (add-hook 'after-init-hook #'yas-global-mode)
  :config
  (setf yas-snippet-dirs '("~/.emacs.d/snippets")))

(req-package rvm
  :init
  (add-hook 'after-init-hook #'rvm-use-default))

(req-package paren
  :init
  (add-hook 'after-init-hook #'show-paren-mode)
  :config
  (setf show-paren-delay 0
        show-paren-style 'mixed))

(req-package navigation :loader :path
  :load-path "lib"
  :bind (("C-c f u i" . nameless/find-user-init-file)
         ("C-c f s" . nameless/find-scratch-buffer)
         ("C-c f d f" . nameless/find-dominating-file)
         ("C-x C-t" . nameless/find-term-buffer)))

(req-package file-helpers :loader :path
  :load-path "lib"
  :init
  (add-hook 'after-save-hook #'nameless/file-make-executable-if-shebang)
  (add-hook 'after-save-hook #'nameless/set-auto-mode))

(req-package term :loader :built-in
  :init
  (add-hook 'term-mode-hook (lambda ()
                              (setf yas-dont-activate-functions t)))
  :bind
  (("C-x t" . ansi-term)))

(req-package inf-ruby
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(req-package neotree
  :require projectile
  :bind (([f8] . neotree-toggle))
  :config
  (setf projectile-switch-project-action 'neotree-projectile-action))

(req-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(req-package phi-search
  :init
  (require 'phi-replace)
  :bind
  (([remap isearch-forward] . phi-search)
   ([remap isearch-backward] . phi-search-backward)
   ([remap query-replace] . phi-replace)))

(req-package sh-script :loader :built-in
  :ensure sh-script
  :mode "\\.zsh$"
  :mode "\\.zsh-theme$")

(req-package helm-descbinds
  :init
  (add-hook 'after-init-hook #'helm-descbinds-mode))

(req-package asm-mode :loader :built-in
  :config
  (setf tab-stop-list (number-sequence 4 200 4)))

(req-package cask-mode
  :require paredit
  :init
  (add-hook 'cask-mode-hook #'enable-paredit-mode))

(req-package lua-mode)

(req-package-finish)

;;; Custom commands

;;; Other configuration

;; when emacs is built without X frontend some features like
;; toolbar or scrollbar are unavailable; that causes
;; initialization to fail. To avoid that, check if the feature is present first
(--each '(tool-bar-mode scroll-bar-mode menu-bar-mode)
  (when (fboundp it)
    (funcall it 0)))

;; Start Emacs maximized
(with-daemon
 (toggle-frame-maximized))

;; Indent using spaces
(setq-default indent-tabs-mode nil)

;; Don't show splash screen at startup
(setf inhibit-splash-screen t)

(defconst nameless/backup-dir "~/.emacs.d/backup/"
  "Directory to put backup files.")

(defconst nameless/auto-save-dir "~/.emacs.d/autosave/"
  "Directory to put auto-save files.")

(make-directory nameless/backup-dir t)
(make-directory nameless/auto-save-dir t)

;; Get rid of annoying backup & autosave files stored in-place
(setf backup-directory-alist `(("." . ,nameless/backup-dir))
      auto-save-file-name-transforms `((".*" ,nameless/auto-save-dir t)))

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

;; Visual line mode
(setf visual-line-fringe-indicators
      (or (cdr (assoc 'continuation fringe-indicator-alist))
          visual-line-fringe-indicators))
(add-hook 'compilation-mode-hook #'visual-line-mode)
(add-hook 'flycheck-error-list-mode-hook #'visual-line-mode)

;; Use UTF-8 by default
(prefer-coding-system 'utf-8)

;; Cyrillic support
(setf default-input-method 'russian-computer)

(provide 'init)
;;; init.el ends here
