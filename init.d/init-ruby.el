;;; init-ruby.el --- `ruby-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package inf-ruby
  :require ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(req-package ruby-mode :loader :built-in
  :mode "^Gemfile$"
  :mode "^Rakefile$")

(req-package rvm
  :require exec-path-from-shell
  :init
  (add-hook 'after-init-hook #'rvm-use-default))

(req-package robe
  :require company ruby-mode
  :init
  (add-hook 'ruby-mode-hook #'robe-mode)
  :config
  (push 'company-robe company-backends))

(req-package rspec-mode)

(provide 'init-ruby)
;;; init-ruby.el ends here
