;;; init.el --- Emacs configuration; -*- lexical-binding: t -*-

;;; Commentary:

;; See https://github.com/smaximov/dotemacs

;;; Code:

;; Tweak GC
(setf gc-cons-threshold 50000000)

;; Initialize Emacs package system
(require 'package)
(setf package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/"))
      package-archive-priorities '(("gnu" . 1)
                                   ("melpa" . 2)
                                   ("melpa-stable" . 0)))
(package-initialize)

;;; Various definitions
(defconst nameless/backup-dir "~/.emacs.d/backup/"
  "Directory to put backup files.")

(defconst nameless/auto-save-dir "~/.emacs.d/autosave/"
  "Directory to put auto-save files.")

(defconst nameless/user-config-directory
  (expand-file-name "init.d" user-emacs-directory)
  "Directory where `load-dir' will search for elisp files.")

(defun nameless:split-list (list elem &optional test)
  "Split LIST into two lists at ELEM.

TEST is a predicate which specifies how to compare two elements.
Default is `eql'.

The function returns a cons (BEFORE-ELEM . AFTER-ELEM), where
BEFORE-ELEM is a list of items before the first occurence of ELEM
and AFTER-ELEM is a list of items after the first occurence of ELEM,
if any."
  (unless test (setf test #'eql))
  (let ((before-elem nil)
        (after-elem nil)
        (elem-found nil))
    (dolist (item list)
      (if (funcall test item elem)
          (if elem-found
              (push item after-elem)
            (setf elem-found t))
        (push item (if elem-found after-elem before-elem))))
    (cons (reverse before-elem) (reverse after-elem))))

;;; Macros and utilities
(defmacro with-daemon (&rest body)
  "Execute BODY immediately when Emacs started in non-daemon mode.
If Emacs started as daemon, delay the execution of BODY until a new
frame is created.

You can separate daemon code from non-daemon using :otherwise keyword
in BODY."
  (let* ((split (nameless:split-list body :otherwise #'eq))
         (daemon-body (car split))
         (otherwise-body (or (cdr split) daemon-body)))
    `(if (daemonp)
         (add-hook 'after-make-frame-functions
                   (lambda (frame)
                     (with-selected-frame frame
                       ,@daemon-body)))
       ,@otherwise-body)))

;;; Package configuration

(defun require-package (package)
  "Install package PACKAGE unless already installed."
  (unless (package-installed-p package)
    (message "Package %s is not available, installing..." package)
    (package-refresh-contents)
    (package-install package))
  (require package))

;; Bootstrap use-package
(require-package 'use-package)

(setf use-package-verbose t)

(use-package req-package
  :ensure t
  :pin melpa-stable)

(req-package load-dir
  :ensure t
  :pin gnu
  :force t
  :config
  (setf load-dir-recursive t)
  (load-dir-one nameless/user-config-directory)
  (req-package-finish))

;;; Configuration

;; When emacs is built without X frontend some features like
;; toolbar or scrollbar are unavailable; that causes
;; initialization to fail. To avoid that, check if the feature is present first
(--each '(tool-bar-mode scroll-bar-mode menu-bar-mode)
  (when (fboundp it)
    (funcall it 0)))

;; Diminish
(require 'diminish)
(diminish 'auto-revert-mode)

;; Emacs package header skeleton
(define-auto-insert '(emacs-lisp-mode . "Emacs Package Skeleton")
  '("Short description: "
    ";;; " (file-name-nondirectory buffer-file-name) " --- " str " -*- lexical-binding: t -*-" ?\n ?\n
    ";;; Commentary:" ?\n ?\n
    ";;; Code:" ?\n ?\n
    _ ?\n ?\n
    "(provide '" (file-name-base) ")" ?\n
    ";;; " (file-name-nondirectory buffer-file-name) " ends here"))

;; Start Emacs maximized
(with-daemon
 (when (window-system)
  (toggle-frame-maximized)))

;; Indent using spaces
(setq-default indent-tabs-mode nil)

;; Don't show splash screen at startup
(validate-setq inhibit-splash-screen t)

(make-directory nameless/backup-dir t)
(make-directory nameless/auto-save-dir t)

;; Get rid of annoying backup & autosave files stored in-place
(validate-setq backup-directory-alist `(("." . ,nameless/backup-dir))
               auto-save-file-name-transforms `((".*" ,nameless/auto-save-dir t)))

;; Display current column position of the cursor
(add-hook 'after-init-hook #'column-number-mode)

;; Track recent files
(add-hook 'after-init-hook #'recentf-mode)

;; Detach the customization file
(validate-setq custom-file "~/.emacs.d/custom.el")
(unless (f-exists? custom-file)
  (f-touch custom-file))
(load custom-file t t)

;; Follow links to VCS-controlled source files
(validate-setq vc-follow-symlinks t)

(defconst nameless:font-families '("Iosevka Type" "Fira Mono" "Anonymous Pro")
  "List of preferred font families ordered by priority.")

;; Enable first available font fron `nameless:font-families'
(when (display-graphic-p)
  (when-let* ((family (cl-find-if (lambda (family) (member family (font-family-list)))
                                  nameless:font-families)))
    (set-face-attribute 'default nil
                        :family family :slant 'normal :weight 'normal :height 140 :width 'normal)
    family))

(define-key indent-rigidly-map (kbd "C-f") #'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "C-b") #'indent-rigidly-left)

(put 'narrow-to-region 'disabled nil)

;; Handle whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(require 'hl-line)
(global-hl-line-mode)

;; Credentials
(validate-setq user-full-name "Sergei Maximov"
               user-mail-address "s.b.maximov@gmail.com")

;; Visual line mode
(validate-setq visual-line-fringe-indicators
               (or (cdr (assoc 'continuation fringe-indicator-alist))
                   visual-line-fringe-indicators))
(add-hook 'compilation-mode-hook #'visual-line-mode)
(add-hook 'flycheck-error-list-mode-hook #'visual-line-mode)

;; Use UTF-8 by default
(prefer-coding-system 'utf-8)

;; Cyrillic support
(validate-setq default-input-method 'russian-computer)

(defun delete-whitespace-till-next-word ()
  "Delete all white space from point to the next word."
  (interactive)
  (if (looking-at "[ \t\n]+")
      (replace-match "")
    (ding)))

(global-set-key (kbd "M-\\") #'delete-whitespace-till-next-word)

(validate-setq split-height-threshold nil)
(validate-setq split-width-threshold 160)

(global-set-key (kbd "C-,") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-.") (lambda () (interactive) (other-window +1)))

(require 'epa)

;; Use minibuffer to ask for the GPG passphrase
(validate-setq epa-pinentry-mode 'loopback)

(defun create-lex-comparator-with-ext (ext)
  "Create pathnames comparator which prefers pathnames which have EXT extension."
  (lambda (a b)
    (let ((a-base (file-name-sans-extension a))
          (b-base (file-name-sans-extension b)))
      (cond
       ((string-lessp a-base b-base)
        t)
       ((string-lessp b-base a-base)
        nil)
       (t
        (string-equal (file-name-extension a) ext))))))

(setf auth-sources (sort auth-sources (create-lex-comparator-with-ext "gpg")))

(defun auto-insert-save-excursion-advice (original-fun &rest args)
  "Preserve buffer position when calling `auto-insert'."
  (if (= (point) (point-min))
      (apply original-fun args)
    (save-excursion (apply original-fun args))))

(advice-add 'auto-insert :around #'auto-insert-save-excursion-advice)

(provide 'init)
;;; init.el ends here
