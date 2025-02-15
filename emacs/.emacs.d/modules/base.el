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
  (completion-styles '(basic initials substring))
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
  (load-theme 'modus-vivendi)
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
  :hook
  (prog-mode-hook . display-line-numbers-mode)
  (text-mode-hook . auto-fill-mode)
  (flymake-after-save-hook . eglot-format-buffer))

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1))

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
  :defer t
  :hook (after-init-hook . exec-path-from-shell-initialize))

(use-package eldoc
  :diminish eldoc-mode)

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
  :defer t
  :bind (("C-c a l" . avy-goto-line)
	 ("C-c a w" . avy-goto-word-0)
	 ("C-c a t" . avy-goto-char-timer)))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c e" . embark-act))
  :init
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

(use-package perspective
  :ensure t
  :defer t
  :bind
  ("C-x C-b" . persp-list-buffers)
  :custom
  (persp-modestring-dividers '("(" ") " ","))
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package golden-ratio
  :ensure t
  :init
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

(provide 'base)
;;; base.el ends here
