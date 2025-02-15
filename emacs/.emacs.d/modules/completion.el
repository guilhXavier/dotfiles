;;; completion.el --- Completion configs
;;; Commentary:
;;; Code:
(use-package vertico
  :ensure t
  :pin elpa
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
	       '(jinx grid (vertico-grid-annotate . 20))))

(use-package consult
  :ensure t
  :bind
  (("C-x b" . consult-buffer)
  ("C-s" . consult-line)
  ("C-c g" . consult-ripgrep)
  ("C-c o" . consult-outline)
  :map isearch-mode-map
  ("M-s e" . consult-isearch-history)
  ("M-s l" . consult-line)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :defer t
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(partial-completion orderless flex))
  (completion-category-defaults nil)
  (read-file-name-completion-ignore-case t)
  (completion-category-overrides '((file (styles partial-completion))
				   (minibuffer (initial orderless)))))
(use-package savehist
  :ensure t
  :defer 2
  :config
  (savehist-mode))

(use-package marginalia
  :pin melpa
  :ensure t
  :custom (marginalia-annotators '(marginalia-annotators-light))
  :init
  (marginalia-mode))

(use-package corfu
  :pin elpa
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)
  (corfu-preview-current nil)
  (corfu-preselect-first nil)
  (corfu-on-exact-match nil)
  (corfu-echo-documentation nil)
  (corfu-scroll-margin 5)
  :init
  (global-corfu-mode))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(provide 'completion)
;;; completion.el ends here
