(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" default)))
 '(flycheck-command-wrapper-function
   (lambda
     (command)
     (let
         ((bundler
           (executable-find "bundle"))
          (cmd
           (file-name-nondirectory
            (car command))))
       (if
           (string-equal cmd "rubocop")
           (append
            (list bundler "exec" "rubocop")
            (cdr command))
         command))))
 '(package-selected-packages
   (quote
    (direnv nix-mode dockerfile-mode flycheck-crystal crystal-mode toml-mode racer cargo racket-mode zpresent rubocop erlang erlang-mode prog-mode s yasnippet yard-mode yaml-mode vue-mode validate smartparens slim-mode scss-mode rvm rust-mode rspec-mode robe rjsx-mode req-package rake powerline phi-search multiple-cursors material-theme magithub load-dir json-mode ivy-pages htmlize highlight-indentation flycheck-rust flycheck-credo exec-path-from-shell emr emmet-mode el-get editorconfig diminish counsel-projectile circe cask-mode cask alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
