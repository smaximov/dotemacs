;;; Package --- summary

;;; Commentary:

;;; Code:

;;; startup customization of UI
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(setq inhibit-splash-screen t)

;;; Add line numbers
(global-linum-mode t)
(setq linum-format "%4d ")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;; GitHub's gists support
(require 'gist)

;;; markdown mode
(add-to-list 'auto-mode-alist
	     '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist
	     '("\\.md$" . markdown-mode))

;;; custom markdown mode hook
(defun my-markdown-mode-hook ()
  (setq markdown-command-needs-filename t) ; markdown.py reads from file
  (setq markdown-command "markdown.py"))   ; custom stylesheet

(add-hook 'markdown-mode-hook 'my-markdown-mode-hook)

;;; add 'redo' functionality
(require 'redo+)

;;; custom keybindings
(global-set-key [(meta /)] 'redo)
(global-set-key [f5] 'compile)
(global-set-key [f1] 'man)
(global-set-key (kbd "<C-tab>") 'complete-symbol)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;; enable fuzzy matching
(require 'fuzzy)

;;; auto completion
(require 'auto-complete)
(global-auto-complete-mode)
(require 'auto-complete-config)
(ac-config-default)

(add-to-list 'ac-modes 'enh-ruby-mode)

;;; use fuzzy completion by default
(setq ac-use-fuzzy t)


;;; haskell setup
(require 'ghci-completion)
;;; Haskell mode customization
(defun nameless/haskell-mode-hook ()
  "Custom haskell-mode hook."
 (local-set-key [return] 'newline-and-indent))

(add-hook 'haskell-mode-hook 'nameless/haskell-mode-hook)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(setq haskell-font-lock-symbols t)

(add-hook 'inferior-haskell-mode-hook 'turn-on-ghci-completion)

(add-to-list 'auto-mode-alist
             '("\\.hs$" . haskell-mode))

(eval-after-load "haskell-mode"
  '(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;;; which function mode - displays current function
;;; name in the mode line
(which-function-mode 1)
(eval-after-load "which-funct"
  '(add-to-list 'which-func-modes 'haskell-mode))

;;; Common Lisp setup
(add-to-list 'load-path "~/.emacs.d/slime")
(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

(require 'slime-autoloads)
(setq slime-net-coding-system 'utf-8-unix)
(setq slime-complete-symbol*-fancy t)
(slime-setup '(slime-fancy slime-repl))

(defun my-paredit-mode-hook ()
  "Customize paredit-mode for Emacs."
  (paredit-mode t)
  (show-paren-mode t)
  (local-set-key (kbd "M-)") 'paredit-close-round-and-newline))

(add-hook 'emacs-lisp-mode-hook 'my-paredit-mode-hook)
(add-hook 'lisp-mode-hook 'my-paredit-mode-hook)
(add-hook 'lisp-interaction-mode-hook 'my-paredit-mode-hook)
(add-hook 'slime-repl-mode-hook 'my-paredit-mode-hook)
;;; (add-hook 'slime-mode-hook 'slime-autostart)

;;; browse url with opera by default
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "opera"
;;       browse-url-generic-args '("-newpage"))

(require 'auto-indent-mode)

;;; ECB configuration
(require 'ecb)
(require 'ecb-autoloads)

(setf ecb-layout-name "leftright2")
(setf ecb-show-sources-in-directories-buffer 'always)
(setf ecb-compile-window-height 12)

;;; activate and deactivate ecb
(global-set-key (kbd "C-x C-;") 'ecb-activate)
(global-set-key (kbd "C-x C-'") 'ecb-deactivate)
;;; show/hide ecb window
(global-set-key (kbd "C-;") 'ecb-show-ecb-windows)
(global-set-key (kbd "C-'") 'ecb-hide-ecb-windows)
;;; quick navigation between ecb windows
(global-set-key (kbd "C-)") 'ecb-goto-window-edit1)
(global-set-key (kbd "C-!") 'ecb-goto-window-directories)
(global-set-key (kbd "C-@") 'ecb-goto-window-sources)
(global-set-key (kbd "C-#") 'ecb-goto-window-methods)
(global-set-key (kbd "C-$") 'ecb-goto-window-compilation)

(defun my-c-mode-common-hook ()
  (c-toggle-auto-newline 1)
  (c-set-style "linux")
  (setq	c-basic-offset 4)
  (local-set-key [return] 'reindent-then-newline-and-indent))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;; Set man page width
(setenv "MANWIDTH" "72")

;;; go customization
(defun my-go-mode-hook ()
  (local-set-key [return] 'reindent-then-newline-and-indent))

(add-hook 'go-mode-hook 'my-go-mode-hook)


(require 'yaml-mode)

(mouse-avoidance-mode 'animate)

;;; LaTeX
(setf TeX-auto-save t
      TeX-parse-self t
      TeX-master nil)

(defun reverse-input-method (input-method)
  "Build the reverse mapping of single letters from INPUT-METHOD."
  (interactive
   (list (read-input-method-name "Use input method (default current): ")))
  (if (and input-method (symbolp input-method))
      (setq input-method (symbol-name input-method)))
  (let ((current current-input-method)
        (modifiers '(nil (control) (meta) (control meta))))
    (when input-method
      (activate-input-method input-method))
    (when (and current-input-method quail-keyboard-layout)
      (dolist (map (cdr (quail-map)))
        (let* ((to (car map))
               (from (quail-get-translation
                      (cadr map) (char-to-string to) 1)))
          (when (and (characterp from) (characterp to))
            (dolist (mod modifiers)
              (define-key local-function-key-map
                (vector (append mod (list from)))
                (vector (append mod (list to)))))))))
    (when input-method
      (activate-input-method current))))

(reverse-input-method  "russian-computer")

;;; Customize fonts
(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Terminus-12"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t)
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(auto-indent-next-pair-timer-geo-mean (quote ((default 0.0005 0))))
 '(auto-indent-next-pair-timer-interval (quote ((default 0.0005))))
 '(background-color "#002b36")
 '(background-mode dark)
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes (quote ("d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ecb-layout-window-sizes (quote (("leftright2" (ecb-directories-buffer-name 0.18269230769230768 . 0.6438356164383562) (ecb-sources-buffer-name 0.18269230769230768 . 0.3424657534246575) (ecb-methods-buffer-name 0.16346153846153846 . 0.6438356164383562) (ecb-history-buffer-name 0.16346153846153846 . 0.3424657534246575)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/" "/"))))
 '(explicit-shell-file-name nil)
 '(fci-rule-color "#073642")
 '(foreground-color "#839496")
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p t)
 '(js2-global-externs nil)
 '(js3-auto-indent-p t)
 '(js3-consistent-level-indent-inner-bracket t)
 '(js3-enter-indents-newline t)
 '(js3-expr-indent-offset 2)
 '(js3-indent-on-enter-key t)
 '(js3-lazy-commas t)
 '(js3-lazy-dots t)
 '(js3-lazy-operators t)
 '(js3-lazy-semicolons t)
 '(js3-paren-indent-offset 2)
 '(js3-square-indent-offset 2)
 '(magit-use-overlays nil)
 '(openwith-associations (quote (("\\.pdf\\'" "evince" (file)) ("\\.mp3\\'" "xmms" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\|mkv\\|mp4\\)\\'" "mplayer2" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "eog" (file)))))
 '(safe-local-variable-values (quote ((encoding . utf-8) (ruby-compilation-executable . "ruby") (ruby-compilation-executable . "ruby1.8") (ruby-compilation-executable . "ruby1.9") (ruby-compilation-executable . "rbx") (ruby-compilation-executable . "jruby") (whitespace-line-column . 80) (lexical-binding . t))))
 '(slime-complete-symbol*-fancy t t)
 '(slime-lisp-implementations (quote ((sbcl ("/usr/bin/sbcl")) (clojure ("/usr/bin/clojure")))) t)
 '(slime-net-coding-system (quote utf-8-unix) t)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#dc322f") (40 . "#cb4b16") (60 . "#b58900") (80 . "#859900") (100 . "#2aa198") (120 . "#268bd2") (140 . "#d33682") (160 . "#6c71c4") (180 . "#dc322f") (200 . "#cb4b16") (220 . "#b58900") (240 . "#859900") (260 . "#2aa198") (280 . "#268bd2") (300 . "#d33682") (320 . "#6c71c4") (340 . "#dc322f") (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil))

;;; Python configuration
(setq jedi:setup-keys t
      jedi:complete-on-dot t)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setf python-python-command "ipython")

;;; Erlang
(add-to-list 'auto-mode-alist
             '("\\.erl$" . erlang-mode))
(add-to-list 'auto-mode-alist
             '("\\.hrl$" . erlang-mode))
(add-to-list 'auto-mode-alist
             '("Emakefile$" . erlang-mode))
(setq erlang-root-dir "/usr/lib64/erlang")

(defun nameless/erlang-mode-hook ()
  ;; Erlang skeletons
  ;; define skeletons keymap
  (define-prefix-command 'erlang-skel-map)
  (local-set-key (kbd "C-c s") 'erlang-skel-map)

  ;; define keys
  (define-key 'erlang-skel-map (kbd "h") 'tempo-template-erlang-small-header)
  (define-key 'erlang-skel-map (kbd "v") 'tempo-template-erlang-supervisor)
  (define-key 'erlang-skel-map (kbd "e") 'tempo-template-erlang-gen-event)
  (define-key 'erlang-skel-map (kbd "s") 'tempo-template-erlang-generic-server)
  (define-key 'erlang-skel-map (kbd "f") 'tempo-template-erlang-gen-fsm)

  (local-set-key (kbd "C-c M-m") 'erlang-man-function)
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; tag error_logger:warning_*() function messages as warnings by
  ;; default; set Erlang node name to "emacs"
  (setq inferior-erlang-machine-options '("+W" "w" "-sname" "emacs")))

(add-hook 'erlang-mode-hook 'nameless/erlang-mode-hook)


;;; Powerline
(require 'powerline)
(powerline-default-theme)

;;; CoffeeScript
(defun nameless/coffee-mode-hook ()
  (setq coffee-tab-width 2))

(add-hook 'coffee-mode-hook 'nameless/coffee-mode-hook)

;;; Octave mode
(defun nameless/octave-mode-hook ()
  (when (eq window-system 'x)
    (font-lock-mode 1)))

(add-hook 'octave-mode-hook 'nameless/octave-mode-hook)

(add-to-list 'auto-mode-alist
             '("\\.m$" . octave-mode))

;;; Irony mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/irony-mode/elisp"))

(require 'yasnippet)
;; (require 'irony)
;; (irony-enable 'ac)

;; (add-hook 'c++-mode-hook 'irony-mode)
;; (add-hook 'c-mode-hook 'irony-mode)

;;; CMake configuration
(require 'cmake-mode)
(add-to-list 'auto-mode-alist
             '("CMakeLists\\.txt$" . cmake-mode))
(add-to-list 'auto-mode-alist
             '("\\.cmake" . cmake-mode))

;;; Scala-mode configuration
(require 'scala-mode2)
(setf scala-indent:default-run-on-strategy scala-indent:eager-strategy)
(add-to-list 'load-path "~/.emacs.d/scala/ensime/elisp/")
(require 'ensime)

(defun nameless/scala-mode-hook ()
  (local-set-key (kbd "RET") 'reindent-then-newline-and-indent))

(add-hook 'scala-mode-hook 'nameless/scala-mode-hook)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(el-get 'sync)

;;; scss-mode
(require 'scss-mode)
(setf scss-compile-at-save nil)

;;; Javascript
(require 'nodejs-repl)
(require 'js2-mode)

(defun nameless/js-mode-hook ()
  (local-set-key [return] 'reindent-then-newline-and-indent)
  (c-toggle-auto-newline 1))

(add-hook 'js-mode-hook 'nameless/js-mode-hook)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;;; Java configuration
(defun nameless/java-mode-hook ()
  (c-set-style "java"))

(add-hook 'java-mode-hook 'nameless/java-mode-hook)

(require 'load-dir)

(setf load-dirs t)
(setf load-dir-recursive t)
(load-dirs)

;;; Flyckeck
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init)
;;; init.el ends here

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

;;; Helm
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
