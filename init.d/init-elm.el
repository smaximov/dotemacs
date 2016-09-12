;;; init-elm.el --- a function language for the Web -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'req-package)

(req-package elm-mode
  :require company f dash json
  :preface
  (defconst nameless:elm-package "elm-package.json")

  (defun nameless:parse-elm-package-src (package-file)
    "Parse `src' array of PACKAGE-FILE."
    (with-temp-buffer
      (insert-file-contents package-file)
      (goto-char (point-min))
      (let ((json-object-type 'hash-table)
            (json-array-type 'list))
        (gethash "source-directories" (json-read)))))

  (defun nameless:resolve-relative-path (file directories)
    "Resolve FILE relative to DIRECTORIES."
    (catch 'done
      (--each directories
        (when (f-ancestor-of? it file)
          (throw 'done (f-relative file it))))))

  (defun nameless:elm-buffer-module-name ()
    "Return the current Elm buffer's corresponding module name."
    (when buffer-file-name
      (-if-let* ((project-root (locate-dominating-file buffer-file-name
                                                       nameless:elm-package))
                 (package-file (f-join project-root nameless:elm-package))
                 (source-dirs (--map (f-join project-root it)
                                     (nameless:parse-elm-package-src package-file)))
                 (relative-path (nameless:resolve-relative-path buffer-file-name
                                                                source-dirs)))
          (replace-regexp-in-string (regexp-quote (f-path-separator)) "."
                                    (f-no-ext relative-path)
                                    'case-sensitive 'literal)
        (f-base buffer-file-name))))
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook #'electric-pair-local-mode)
  (add-to-list 'company-backends #'company-elm))

(req-package flycheck-elm
  :require flycheck
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-elm-setup))

(provide 'init-elm)
;;; init-elm.el ends here
