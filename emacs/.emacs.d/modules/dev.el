;;; dev.el --- Dev configs
;;; Commentary:
;;; Code:
(use-package eglot
  :pin elpa
  :ensure t
  :defer t
  :config
  (fset #'jsonrpc--log-event #'ignore)
  :custom
  (read-process-output-max (* 1024 1024))
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown)
  (eglot-send-changes-idle-time 0.1)
  :hook ((c-ts-mode-hook . eglot-ensure)
	 (css-ts-mode-hook . eglot-ensure)
	 (html-mode-hook . eglot-ensure)
	 (js-base-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . prettier-mode)
	 (css-ts-mode-hook . prettier-mode)
	 (json-ts-mode-hook . prettier-mode)
	 (go-ts-mode-hook . eglot-ensure)
	 (latex-mode-hook . eglot-ensure)
	 (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode))
  :bind (("C-c l b" . eglot-format-buffer)
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l e" . eglot-reconnect)
	 ("C-c l r" . eglot-rename)))

(use-package yaml-pro
  :ensure t
  :defer t)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-lint
  :ensure t
  :defer t)

(use-package web-mode
  :ensure t
  :defer t)

(use-package prettier
  :ensure t
  :defer t
  :diminish)

(use-package css-in-js-mode
  :straight '(css-in-js-mode :type git :host github :repo "orzechowskid/tree-sitter-css-in-js"))

(use-package dape
  :ensure t
  :defer t
  :hook
  ((kill-emacs . dape-breakpoint-save)
   (after-init . dape-breakpoint-load))
  :config
  (setq dape-buffer-window-arrangement 'right)
  (dape-breakpoint-global-mode)
  (add-hook 'dape-stopped-hook 'dape-info)
  (add-hook 'dape-stopped-hook 'dape-repl)
  (add-hook 'dape-compile-hook 'kill-buffer)
  (add-hook 'dape-start-hook (lambda () (save-some-buffers t))))

(use-package tree-sitter
  :ensure t
  :defer t
  :diminish " tree")

(use-package tree-sitter-langs
  :ensure t
  :defer t)

(use-package treesit
  :commands (treesit-install-language-grammar gui/treesit-install-all-languages)
  :init
  (setq treesit-language-source-alist
     '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
       (c . ("https://github.com/tree-sitter/tree-sitter-c"))
       (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
       (css . ("https://github.com/tree-sitter/tree-sitter-css"))
       (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
       (go . ("https://github.com/tree-sitter/tree-sitter-go"))
       (html . ("https://github.com/tree-sitter/tree-sitter-html"))
       (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
       (json . ("https://github.com/tree-sitter/tree-sitter-json"))
       (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
       (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
       (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))
  :mode
  ("\\.ts\\(x\\)?\\'" . tsx-ts-mode)
  ("\\.go\\'" . go-ts-mode)
  :config
  (defun gui/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75)))))

(use-package editorconfig
  :ensure t
  :defer t
  :diminish
  :config
  (editorconfig-mode 1)
  :hook
  (prog-mode-hook . editorconfig-apply))

(use-package smartparens
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode-hook . smartparens-mode))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
  :config (add-to-list 'copilot-major-mode-alist '("u"))
  :hook (prog-mode-hook . copilot-mode)
  :bind (:map copilot-completion-map
	 ("[tab]" . copilot-accept-completion)
	 ("TAB" . copilot-accept-completion))
  :custom
  (copilot-node-executable "/Users/i568723/.nvm/versions/node/v20.17.0/bin/node"))

(use-package copilot-chat
  :straight (:host github :repo "chep/copilot-chat.el" :files ("*.el"))
  :after (request))

(use-package symbol-overlay
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind (("M-i" . symbol-overlay-put)
	 ("M-n" . symbol-overlay-switch-forward)
	 ("M-p" . symbol-overlay-switch-backward)))

(use-package flycheck
  :ensure t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  :hook
  (after-init-hook . global-flycheck-mode))

(use-package flymake
  :ensure t)

(provide 'dev)
;;; dev.el ends here
