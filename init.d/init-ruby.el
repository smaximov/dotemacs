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
  :require company ruby-mode rvm inf-ruby
  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  :config
  (push 'company-robe company-backends)

  (advice-add 'inf-ruby-console-auto
              :before #'rvm-activate-corresponding-ruby))

(req-package yard-mode
  :require ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'yard-mode))

(req-package rake
  :require ruby-mode helm
  :config
  (setf rake-completion-system 'helm)
  :bind (("C-c . t"  . rake)
         ("C-c . r" . rake-rerun)))

(req-package rspec-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
