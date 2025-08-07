;;; dev.el --- Dev configs
;;; Commentary:
;;; Code:
(defun gui/advice-compilation-filter (f proc string)
  "Advice for `compilation-filter' to use `xterm-color' for ANSI color codes."
  (funcall f proc (xterm-color-filter string)))

(defun gui/symbol-overlay-dwim ()
  "DWIM symbol overlay.
If invoked on an already highlighted symbol, remove it.
If not, add it to highlight list."
  (interactive)
  (let ((overlays (symbol-overlay-get-list 0 (thing-at-point 'symbol))))
    (if overlays
	(symbol-overlay-remove-all))
    (symbol-overlay-put)))

(defun gui/eglot-capf-with-yasnippet ()
  "Use `eglot-completion-at-point' with `yasnippet-capf'."
  (setq-local completion-at-point-functions
              (list 
	           (cape-capf-super
		        #'eglot-completion-at-point
		        #'yasnippet-capf))))

(use-package eglot
  :pin elpa
  :ensure t
  :defer t
  :config
  (fset #'jsonrpc--log-event #'ignore)
  (add-to-list 'eglot-server-programs
               `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode)
		         .
		         ("~/.local/state/fnm_multishells/73526_1753288954055/bin/typescript-language-server" "--stdio"
                  :initializationOptions
                  (:preferences
                   (:includeInlayParameterNameHints "all"
		                                            :includeInlayParameterNameHintsWhenArgumentMatchesName t
		                                            :includeInlayFunctionParameterTypeHints t)))))
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
         (typescript-ts-mode-hook . eglot-ensure)
	     (css-ts-mode-hook . prettier-mode)
	     (json-ts-mode-hook . prettier-mode)
	     (go-ts-mode-hook . eglot-ensure)
	     (latex-mode-hook . eglot-ensure)
	     (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode)
	     (eglot-managed-mode-hook . eglot-inlay-hints-mode)
         (before-save-hook . eglot-format-buffer)
         (eglot-managed-mode-hook . gui/eglot-capf-with-yasnippet))
  :bind (("C-c l b" . eglot-format-buffer)
	     ("C-c l a" . eglot-code-actions)
	     ("C-c l e" . eglot-reconnect)
	     ("C-c l r" . eglot-rename)))

(use-package yasnippet
  :ensure t
  :diminish
  :config
  (yas-global-mode 1)
  (setq yas-triggers-in-field t)
  :hook
  (prog-mode-hook . yas-minor-mode))

(use-package yasnippet-capf
  :ensure t
  :after yasnippet
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package conf-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . conf-mode))

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
  :defer t
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

(use-package cider
  :ensure t
  :defer t)

(use-package clojure-ts-mode
  :ensure t
  :defer t)

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
  (copilot-node-executable "/Users/i568723/.local/state/fnm_multishells/73376_1753288905522/bin/node"))

(use-package copilot-chat
  :defer t
  :ensure t)

(use-package symbol-overlay
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode-hook . symbol-overlay-mode)
  :bind (("M-i" . gui/symbol-overlay-dwim)
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

(use-package xterm-color
  :ensure t
  :custom
  (compilation-environment '("TERM=ansi"))
  (comint-output-filter-functions
   (remove 'ansi-color-process-output
           comint-output-filter-functions))
  (compilation-scroll-output t)
  :config
  (advice-add 'compilation-filter :around #'gui/advice-compilation-filter)
  :hook
  (shell-command-mode-hook . (lambda ()
                       (font-lock-mode -1)
                       (make-local-variable 'font-lock-function)
                       (setq font-lock-function (lambda (_) nil))
                       (add-hook 'comint-preoutput-filter-functions
                                 'xterm-color-filter nil t))))

(provide 'dev)
;;; dev.el ends here
