;;; theming.el --- Theming configs
;;; Commentary:
;;; Code:
(use-package hide-mode-line
  :ensure t
  :hook ((text-mode-hook . hide-mode-line-mode)
	 (prog-mode-hook . hide-mode-line-mode)))

(use-package dash
  :ensure t)

(mini-echo-define-segment "persp"
  "Get the current perspective list"
  :setup (persp-init-frame)
  :fetch (mini-echo-segment--extract (frame-parameter nil 'persp--modestring)))

(use-package mini-echo
  :ensure t
  :config
  (mini-echo-mode)
  :custom
  (mini-echo-persistent-rule '(:long ("major-mode" "shrink-path" "vcs" "flymake" "persp")
			       :short ("buffer-name" "flymake"))))
(use-package nerd-icons
  :ensure t
  :defer t)

(use-package nerd-icons-completion
  :ensure t
  :defer t
  :after marginalia
  :hook (marginalia-mode-hook . nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :defer t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . nerd-icons-dired-mode))

(provide 'theming)
;;; theming.el ends here
