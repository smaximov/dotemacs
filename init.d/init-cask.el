;;; init-cask.el --- `cask-mode' configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package cask-mode
  :require paredit
  :init
  (add-hook 'cask-mode-hook #'enable-paredit-mode))

(req-package cask
  :require dash f
  :config
  (defun cask-update-load-path ()
    (interactive)
    (-when-let* ((project-path (and buffer-file-name
                                    (locate-dominating-file buffer-file-name cask-filename)))
                 (bundle (cask-setup project-path))
                 (elisp-files (--map (f-canonical (f-expand it project-path))
                                     (--filter (equal "el" (f-ext it))
                                               (cask-files bundle)))))
      (when (-contains? elisp-files buffer-file-name)
        (let* ((elisp-dirs (-distinct (--map (f-dirname it) elisp-files)))
               (deps-dirs (-non-nil (--map (cask-dependency-path bundle
                                                                 (cask-dependency-name it))
                                           (cask-runtime-dependencies bundle t))))
               (extra-paths (append elisp-dirs deps-dirs))
               (new-paths (append extra-paths (default-value 'load-path))))
          (set (make-local-variable 'load-path) new-paths)
          (when (boundp 'flycheck-emacs-lisp-load-path)
            (set (make-local-variable 'flycheck-emacs-lisp-load-path)
                 new-paths))))))
  (add-hook 'emacs-lisp-mode-hook #'cask-update-load-path))

(provide 'init-cask)
;;; init-cask.el ends here
