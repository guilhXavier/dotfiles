;;; base.el --- Base Emacs configs
;;; Commentary:
;;; Code:
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))


(use-package emacs
  :custom
  (ring-bell-function 'ignore)
  (browse-url-new-window-flag t)
  (diary-file "~/Notes/diary")
  (vc-follow-symlinks t)
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "Guimacs")
  (inhibit-startup-screen t)
  (inhibit-startup-echo-area-message t)
  (confirm-kill-processes nil)
  (use-short-answers t)
  (read-process-output-max (* 1024 1024))
  (mac-command-modified 'meta)
  (create-lockfiles nil)
  (insert-directory-program "gls")
  (warning-minimum-level :error)
  (cursor-type 'bar)
  (frame-title-format '("%b"))
  (fill-column 80)
  (auto-revert-avoid-polling t)
  (auto-revert-interval 5)
  (auto-revert-check-vc-info t)
  (setence-end-double-space nil)
  (make-backup-file-name-function 'bedrock--backup-file-name)
  (enable-recursive-minibuffers t)
  (completion--cycle-threshold 1)
  (completions-detailed t)
  (tab-always-indent 'complete)
  (completion-auto-help 'always)
  (completions-max-height 20)
  (completions-detailed t)
  (completions-format 'one-column)
  (completions-group t)
  (completion-auto-select 'second-tab)
  (x-underline-at-descent-line nil)
  (show-trailing-whitespace nil)
  (display-line-numbers-width 3)
  :config
  (set-frame-font "Input Mono")
  (blink-cursor-mode -1)
  (cua-mode 1)
  (windmove-default-keybindings 'control)
  (savehist-mode)
  (global-auto-revert-mode)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode t)
  (delete-selection-mode t)
  (global-hl-line-mode 1)
  (pixel-scroll-precision-mode 1)
  (add-to-list 'display-buffer-alist
               '("\\`\\*\\(Warnings\\|Compile-Log\\|Fancy Diary Entries\\)\\*\\'"
		 (display-buffer-no-window)
		 (allow-no-window . t)))
  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)
  :bind
  ("C-x C-r" . pop-global-mark)
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (before-save-hook . delete-trailing-whitespace)
  (prog-mode-hook . electric-pair-mode)
  (text-mode-hook . auto-fill-mode)
  (flymake-after-save-hook . eglot-format-buffer))

(use-package winum
  :ensure t
  :custom
  (winum-auto-setup-mode-line nil)
  :config
  (winum-mode 1)
  (add-to-list 'pulsar-pulse-functions 'winum-select-window-1)
  (add-to-list 'pulsar-pulse-functions 'winum-select-window-2)
  (add-to-list 'pulsar-pulse-functions 'winum-select-window-3)
  (add-to-list 'pulsar-pulse-functions 'winum-select-window-4)
  (add-to-list 'golden-ratio-extra-commands 'winum-select-window-1)
  (add-to-list 'golden-ratio-extra-commands 'winum-select-window-2)
  (add-to-list 'golden-ratio-extra-commands 'winum-select-window-3)
  (add-to-list 'golden-ratio-extra-commands 'winum-select-window-4)
  :bind (:map winum-keymap
	      ("M-1" . winum-select-window-1)
	      ("M-2" . winum-select-window-2)
	      ("M-3" . winum-select-window-3)
	      ("M-4" . winum-select-window-4)))

(use-package modus-themes
  :ensure t
  :custom
  (modus-themes-completions '((matches . (extrabold underline))
			      (selection . (semibold italic))))
  :config
  (load-theme 'modus-vivendi t))

(use-package compile
  :bind
  ("C-x c" . compile)
  :custom
  (compilation-scroll-output t)
  (compilation-auto-jump-to-first-error t)
  (compilation-max-output-line-length nil))

(use-package delsel
  :ensure nil
  :hook (after-init-hook . delete-selection-mode))

(use-package eww
  :ensure nil
  :hook (eww-after-render-hook . eww-readable))

(use-package gcmh
  :ensure t
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold (* 32 1024 1024))
  (gc-cons-percentage 0.8))

(use-package dired
  :ensure nil
  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls"))
  :custom
  (dired-use-ls-dired t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t)
  (dired-listing-switches "-alv --group-directories-first")
  :hook ((dired-mode-hook . (lambda () (dired-hide-details-mode 1)))
	 (dired-mode-hook . dired-omit-mode)
	 (dired-mode-hook . nerd-icons-dired-mode)))

(use-package exec-path-from-shell
  :ensure t
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package eldoc
  :ensure t
  :hook (emacs-startup-hook . global-eldoc-mode))

(use-package eldoc-box
  :ensure t
  :hook
  (prog-mode-hook . eldoc-box-hover-at-point-mode))

(use-package project
  :ensure nil
  :config
  (setq project-vc-ignores '("target/" "bin/" "obj/")
	project-vc-extra-root-markers '("README.org"
					"README.md")))

(use-package helpful
  :ensure t
  :bind
  ("C-c C-d" . helpful-at-point)
  ("C-h f" . helpful-callable)
  ("C-h v" . helpful-variable)
  ("C-h k" . helpful-key)
  ("C-h x" . helpful-command))

(use-package pulsar
  :ensure t
  :init
  (pulsar-global-mode 1)
  :bind
  ("C-c h p" . pulsar-pulse-line)
  ("C-c h h" . pulsar-highlight-line)
  :custom
  (pulsar-pulse t)
  (pulsar-delay 0.055)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-magenta)
  (pulsar-highlight-face 'pulsar-yellow))

(use-package ansi-color
  :hook
  (compilation-filter-hook . ansi-color-compilation-filter))

(use-package fzf
  :ensure t
  :defer t
  :bind (("C-c f f" . fzf)
	 ("C-c f r" . fzf-grep)
	 ("C-c f g" . fzf-git-files))
  :custom
  (fzf/args "--ansi --color dark --print-query --margin=1,0 --no-hscroll")
  (fzf/executable "fzf")
  (fzf/git-grep-args "-i --line-number --color=always %s")
  (fzf/position-bottom t)
  (fzf/window-height 15)
  (fzf/grep-command "rg --color=always --line-number"))

(use-package deadgrep
  :ensure t
  :defer t
  :bind ("M-s o" . deadgrep))

(use-package wgrep
  :ensure t
  :custom
  (wgrep-auto-save-buffer t))

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c a l" . avy-goto-line)
	 ("C-c a w" . avy-goto-word-0)
	 ("C-c a t" . avy-goto-char-timer)))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind ("C-." . embark-act)
  :init
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package perspective
  :ensure t
  :demand t
  :custom
  (persp-modestring-dividers '("(" ") " ","))
  (persp-mode-prefix-key (kbd "C-c p"))
  :config
  (persp-mode))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(use-package transpose-frame
  :ensure t
  :defer t
  :commands (transpose-frame
	     flip-frame
	     flop-frame
	     rotate-frame
	     rotate-frame-clockwise
	     rotate-frame-anticlockwise)
  :bind (("C-c r" . rotate-frame-clockwise)))

(use-package magit
  :ensure t
  :custom
  (magit-process-finish-apply-ansi-colors t)
  (magit-define-global-key-bindings 'recommended)
  (magit-refresh-status-buffer nil)
  (auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (magit-diff-highlight-indentation nil)
  (magit-diff-highlight-trailing nil)
  (magit-diff-paint-whitespace nil)
  (magit-diff-highlight-hunk-body nil)
  (magit-diff-refine-hunk nil)
  :config
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  :bind-keymap
  ("C-c g" . magit-mode-map))

(use-package undo-fu
  :ensure t
  :defer t
  :diminish
  :bind
  ("s-z" . undo-fu-only-undo)
  ("s-Z" . undo-fu-only-redo))

(use-package expand-region
  :ensure t
  :defer t
  :diminish
  :bind ("C-=" . er/expand-region))

(use-package git-gutter
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode-hook . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t)

(provide 'base)
;;; base.el ends here
