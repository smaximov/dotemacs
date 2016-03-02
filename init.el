;; Use cask for Emacs configuration
(defvar user-cask-file "~/.emacs.d/Cask"
  "Cask file for emacs configuration")

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(pallet-mode t)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;; startup customization of UI

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

;; Custom theme
(load-theme 'material t)

;; Detach the customization file
(setf custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; custom keybindings
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
(global-set-key (kbd "C-c f u i") #'nameless/find-user-init-file)
(global-set-key (kbd "C-c f u c") #'nameless/find-user-cask-file)
(global-set-key (kbd "C-c f s") #'nameless/find-scratch-buffer)
(global-set-key (kbd "C-c m s") #'magit-status)
(global-set-key (kbd "C-c t l d r") #'tldr)
(global-set-key (kbd "C-x n i") #'nameless/narrow-to-region-in-indirect-buffer)

;; Follow links to VCS-controlled source files
(setf vc-follow-symlinks t)

;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

;; Choose function to apply depending on the presence of prefix argument
(defun nameless/dispatch-by-prefix-arg (prefix-present-fun prefix-absent-fun &rest args)
  "Choose function based on the presence of prefix argument."
  (apply (if current-prefix-arg prefix-present-fun prefix-absent-fun) args))

;; Switch to scratch buffer
(defun nameless/find-scratch-buffer ()
  "Switch to the scratch buffer (create one if necessary).

With prefix argument, switch to the scratch buffer in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'switch-to-buffer-other-window #'switch-to-buffer
                                   "*scratch*"))

;; Utility function to quickly open user's init file
(defun nameless/find-user-init-file ()
  "Find user's initialization file.

With prefix argument, open the file in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                   user-init-file))

;; Utility function to quickly open user's cask file
(defun nameless/find-user-cask-file ()
  "Find user's cask file.

With prefix argument, open the file in other window."
  (interactive)
  (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                   user-cask-file))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Elixir Lang
(add-hook 'elixir-mode-hook #'alchemist-mode)
(add-hook 'alchemist-mode-hook #'company-mode)
(add-hook 'alchemist-iex-mode-hook #'company-mode)

;; Haskell
(add-hook 'haskell-mode-hook #'haskell-indentation-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))
(add-hook 'haskell-mode-hook #'company-mode)
(add-hook 'haskell-mode-hook #'interactive-haskell-mode)

(add-hook 'prog-mode-hook #'emr-initialize)
(define-key prog-mode-map (kbd "M-RET") #'emr-show-refactor-menu)

;; Various Lisps
(--each '(emacs-lisp-mode-hook ielm-mode-hook cider-mode-hook clojure-mode-hook cider-repl-mode-hook)
  (add-hook it #'company-mode)
  (add-hook it #'eldoc-mode))

;; Paredit
(--each '(emacs-lisp-mode-hook eval-expression-minibuffer-setup-hook
                               ielm-mode-hook lisp-mode-hook lisp-interaction-mode-hook
                               scheme-mode-hook clojure-mode-hook
                               cider-repl-mode-hook)
  (add-hook it #'enable-paredit-mode))

;; Clojure
(setf cider-lein-command (f-full "~/bin/lein"))

;; Emacs Lisp
(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'eval-buffer)

(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; Javascript
(setf flycheck-disabled-checkers '(javascript-jshint))
(setf js-indent-level 2)
(add-hook 'js-mode-hook #'electric-pair-mode)

;; Org Mode
(require 'org)
(setf org-default-notes-file (f-join org-directory "notes.org")
      org-src-fontify-natively t)
(define-key global-map "\C-cc" 'org-capture)

;; Use Ido
(setf ido-auto-merge-work-directories-length -1)
(ido-mode 1)
(ido-everywhere t)

;; Handle whitespace
(setf whitespace-line-column 120)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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

;; Make files with shebang executable
(add-hook 'after-save-hook #'nameless/file-make-executable-if-shebang)

;; Select auto mode for new files
(add-hook 'after-save-hook #'nameless/set-auto-mode)

(defun nameless/convert-lordown ()
  (interactive)
  (call-process-region (point-min) (point-max) "lordown" t t))

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-e") #'nameless/convert-lordown)))
(put 'narrow-to-region 'disabled nil)

;; Narrow to region in indirect buffer
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

;; Rust customization
(setf racer-rust-src-path (f-full "~/src/rust/src"))

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
(add-hook 'rust-mode-hook #'cargo-minor-mode)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(setf company-tooltip-align-annotations t)

(defun nameless/rust-mode-hook ()
  (local-set-key (kbd "C-c f c") #'nameless/find-cargo-file))

(defun nameless/find-cargo-file ()
  "Find project's Cargo.toml file.

With prefix argument, find the file in other window."
  (interactive)
  (-if-let (crate-root (locate-dominating-file (buffer-file-name) "Cargo.toml"))
      (nameless/dispatch-by-prefix-arg #'find-file-other-window #'find-file
                                       (f-expand "Cargo.toml" crate-root))
    (error "No `Cargo.toml` found")))

(add-hook 'rust-mode-hook #'nameless/rust-mode-hook)

;; It's not like we are 800x600 nowadays
(setf fill-column 120)

;; Highlight matching parentheses
(setf show-paren-delay 0
      show-paren-style 'mixed)
(add-hook 'after-init-hook #'show-paren-mode)

;; Yasnippet
(add-hook 'after-init-hook #'yas-global-mode)

;; Ruby
(add-hook 'after-init-hook #'rvm-use-default)

;; SCSS
(setf css-indent-offset 2)
(add-hook 'scss-mode-hook #'whitespace-mode)

;; Projectile
(add-hook 'after-init-hook #'projectile-global-mode)

;; Env variables from shell
(let ((-compare-fn #'eq))
  (when (-contains? '(ns x) window-system)
    (exec-path-from-shell-initialize)))

;; Helm
(require 'helm-config)
(require 'helm)

(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") #'helm-mini)
(global-set-key (kbd "M-s o") #'helm-occur)

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
