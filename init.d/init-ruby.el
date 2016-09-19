;;; init-ruby.el --- `ruby-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package inf-ruby
  :require ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  ;; may as well switch default implementaion to "ruby" ("irb")
  (setf inf-ruby-default-implementation "pry"))

(req-package ruby-mode :loader :built-in
  :require smartparens autoinsert
  :mode "^Gemfile$"
  :mode "^Rakefile$"
  :init
  (add-hook 'ruby-mode-hook #'smartparens-strict-mode)
  :config
  (define-auto-insert '(ruby-mode . "Ruby skeleton")
    '(nil
      "# frozen_string_literal: true" ?\n
      _)))

(req-package rvm
  :require exec-path-from-shell
  :init
  (add-hook 'after-init-hook #'rvm-use-default))

(req-package robe
  :require company ruby-mode rvm inf-ruby diminish
  :diminish robe-mode
  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  :config
  (push 'company-robe company-backends)

  (advice-add 'inf-ruby-console-auto
              :before #'rvm-activate-corresponding-ruby))

(req-package yard-mode
  :require ruby-mode diminish
  :diminish yard-mode
  :init
  (add-hook 'ruby-mode-hook #'yard-mode))

(req-package rake
  :require ruby-mode ivy
  :config
  (setf rake-completion-system 'ivy-completing-read)
  :bind (("C-c . t"  . rake)
         ("C-c . r" . rake-rerun)
         ("C-c . c" . rake-regenerate-cache)))

;; https://github.com/bbatsov/projectile/issues/991#issuecomment-248026667
(setq projectile-rails-keymap-prefix (kbd "C-c C-r"))

(req-package projectile-rails
  :loader :path
  :load-path "lib/projectile-rails"
  :init
  (add-hook 'projectile-mode-hook #'projectile-rails-on)
  :config
  (add-hook 'projectile-rails-server-mode-hook
            (lambda ()
              (setq-local compilation-scroll-output t)))
  (setf projectile-rails-expand-snippet nil))

(req-package rspec-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
