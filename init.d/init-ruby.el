;;; init-ruby.el --- Ruby, Rails, and supporting tools -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package inf-ruby
  :ensure t
  :pin melpa-stable
  :require ruby-mode validate
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  ;; may as well switch default implementaion to "ruby" (== "irb")
  (validate-setq inf-ruby-default-implementation "pry"
                 ruby-insert-encoding-magic-comment nil))

(req-package mmm-erb
  :mode  ("\\.ejs$" . html-erb-mode)
  :mode ("\\.erb$" . html-erb-mode))

(req-package ruby-mode ; built-in
  :ensure t
  :require smartparens autoinsert
  :mode "^Gemfile$"
  :mode "^Rakefile$"
  :hook (ruby-mode . smartparens-mode)
  :init
  (define-auto-insert '(ruby-mode . "Ruby skeleton")
    '(nil
      "# frozen_string_literal: true" ?\n
      _)))

(req-package robe
  :ensure t
  :pin melpa-stable
  :require company ruby-mode inf-ruby diminish
  :diminish robe-mode
  :hook (ruby-mode . robe-mode)
  :config
  (push 'company-robe company-backends))

(req-package yard-mode
  :ensure t
  :pin melpa
  :require ruby-mode diminish
  :diminish yard-mode
  :hook ruby-mode)

(req-package rake
  :ensure t
  :pin melpa-stable
  :require ruby-mode ivy
  :config
  (setf rake-completion-system 'ivy-completing-read)
  :bind (("C-c . t"  . rake)
         ("C-c . r" . rake-rerun)
         ("C-c . c" . rake-regenerate-cache)))

(req-package rspec-mode
  :ensure t
  :pin melpa-stable
  :require inf-ruby
  :hook (after-init . inf-ruby-switch-setup))

(req-package rubocop-toggle-cops
  :load-path "lib"
  :require flycheck ruby-mode
  :bind (:map ruby-mode-map
              ("C-c :" . rubocop-toggle-cops)))

(req-package rubocop
  :ensure t
  :require ruby-mode diminish
  :diminish rubocop-mode
  :pin melpa-stable
  :hook (ruby-mode . rubocop-mode))

(provide 'init-ruby)
;;; init-ruby.el ends here
