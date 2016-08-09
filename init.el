;;; init.el --- Emacs configuration; -*- lexical-binding: t -*-

;;; Commentary:

;; See https://github.com/smaximov/dotemacs

;;; Code:

;; Save PID file when running as a daemon
(require 'server)

(when (daemonp)
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

;; Initialize Emacs package system
(require 'package)
(setf package-enable-at-startup nil
      package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; Various definitions
(defconst nameless/backup-dir "~/.emacs.d/backup/"
  "Directory to put backup files.")

(defconst nameless/auto-save-dir "~/.emacs.d/autosave/"
  "Directory to put auto-save files.")

(defconst nameless/user-config-directory
  (expand-file-name "init.d" user-emacs-directory)
  "Directory where `load-dir' will search for elisp files.")

;;; Macros and utilities
(defmacro with-daemon (&rest body)
  "Execute BODY immediately when Emacs started in non-daemon mode.
If Emacs started as daemon, delay the execution of BODY until a new
frame is created."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     ,@body)))
    (when (window-system)
      ,@body)))

;;; Package configuration

;; Bootstrap req-package
(add-to-list 'load-path (expand-file-name "lib/req-package"
                                          user-emacs-directory))
(require 'req-package)

(req-package load-dir
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

;; Start Emacs maximized
(with-daemon
 (toggle-frame-maximized))

;; Indent using spaces
(setq-default indent-tabs-mode nil)

;; Don't show splash screen at startup
(setf inhibit-splash-screen t)

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
(unless (f-exists? custom-file)
  (f-touch custom-file))
(load custom-file t t)

;; Follow links to VCS-controlled source files
(setf vc-follow-symlinks t)

;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

(put 'narrow-to-region 'disabled nil)

;; It's not like we are 800x600 nowadays
(setq-default fill-column 120)

;; Handle whitespace
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Credentials
(setf user-full-name "Sergei Maximov"
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

;; FIXME: a dirty hack to make Emacs split windows vertically
;; by default when the server is started by systemd. I should
;; investigate the cause of this issue when I have some time.
;; It can be related to the environment the server is started in.
(setf split-height-threshold 200)

(provide 'init)
;;; init.el ends here
