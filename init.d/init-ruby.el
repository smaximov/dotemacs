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
  :require smartparens
  :mode "^Gemfile$"
  :mode "^Rakefile$"
  :init
  (add-hook 'ruby-mode-hook #'smartparens-strict-mode))

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

(req-package rspec-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
