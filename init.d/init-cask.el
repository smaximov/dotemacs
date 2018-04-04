;;; init-cask.el --- `cask-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package cask-mode
  :ensure t
  :pin melpa-stable
  :require paredit
  :hook (cask-mode . enable-paredit-mode))

(req-package cask
  :ensure t
  :pin melpa-stable
  :require dash f
  :config
  ;; TODO refactor this mess
  (defun cask-update-load-path ()
    (interactive)
    (-when-let* ((project-path (and buffer-file-name
                                    (locate-dominating-file buffer-file-name cask-filename)))
                 (bundle (cask-setup project-path))
                 (elisp-files (--map (f-canonical (f-expand it project-path))
                                     (--filter (equal "el" (f-ext it))
                                               (cask-files bundle)))))
      (let* ((included-p (-contains? elisp-files buffer-file-name))
             (elisp-dirs (-distinct (cons (f-dirname buffer-file-name)
                                          (--map (f-dirname it) elisp-files))))
             (deps (append (when included-p
                             (cask-development-dependencies bundle t))
                           (cask-runtime-dependencies bundle t)))
             (deps-dirs (-non-nil (--map (cask-dependency-path bundle
                                                               (cask-dependency-name it))
                                         deps)))
             (extra-paths (append elisp-dirs deps-dirs))
             (new-paths (append extra-paths (default-value 'load-path))))
        (setq-local load-path new-paths)
        (when (boundp 'flycheck-emacs-lisp-load-path)
          (setq-local flycheck-emacs-lisp-load-path new-paths)))))
  (add-hook 'emacs-lisp-mode-hook #'cask-update-load-path))

(provide 'init-cask)
;;; init-cask.el ends here
