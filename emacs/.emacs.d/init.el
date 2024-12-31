;;; init.el --- Guimacs config -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal Emacs configuration.

;;; Code:
;;;; * Bootstrap
(require 'use-package)

(require 'ls-lisp)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package use-package
  :custom
  (use-package-hook-name-suffix nil)
  (use-package-compute-statistic t))

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(use-package diminish :ensure t :after use-package)

;;;; * Guimacs
(defun gui/quit-emacs ()
  "Quit Emacs interactively."
  (interactive)
  (save-buffers-kill-terminal t))

(define-minor-mode gui/init-mode
  "Minor mode for the init buffer."
  :init-value t
  :lighter " Init"
  (if gui/init-mode
      (progn
       (message "Init mode enabled")
       (outline-hide-sublevels 2))
    (message "Init mode disabled")))

(defun gui/enable-init-mode-for-init-file ()
  "Enable `gui/init-mode' for the init file."
  (when (string-match "\\(early-\\)?init\\.el" (buffer-name))
    (gui/init-mode 1)))

(defun gui/move-to-point-max ()
  "Move the point to the end of the buffer."
  (goto-char (point-max)))

(defun gui/switch-focus-copilot-chat-buffer ()
  "Switch focus to the Copilot chat buffer."
  (pop-to-buffer "*Copilot-chat*"))

(defun gui/copilot-chat ()
  "Start the Copilot chat in a new perspective."
  (interactive)
  (persp-switch "copilot")
  (copilot-chat-display)
  (advice-add 'gui/move-to-point-max :after #'copilot-chat--write-buffer)
  (advice-add 'copilot-chat-prompt-send :after #'gui/switch-focus-copilot-chat-buffer)
  (if (get-buffer copilot-chat--prompt-buffer)
      (pop-to-buffer copilot-chat--prompt-buffer)
    (message "Copilot chat prompt not available")))

(defun gui/get-enabled-theme ()
  "Get the currently loaded theme"
  (symbol-name (car custom-enabled-themes)))

(defconst gui/buffer-id-dark-bg-alist '((:modified . rainbow-6)
					(:readonly . yellow-intense)
					(:default . fg-main))
  "Alist for foreground buffer name colors while in a dark mode.")

(defconst gui/buffer-id-light-bg-alist '((:modified . keybind)
					 (:readonly . bg-graph-green-1)
					 (:default . fg-main))
  "Alist for foreground buffer name colors while in light mode.")

(defun gui/resolve-color-map ()
  "Resolve the correct color map for the buffer name."
  (let ((colors (if (string= (gui/get-enabled-theme) "modus-vivendi")
		    gui/buffer-id-dark-bg-alist
		  gui/buffer-id-light-bg-alist)))
    (gui/resolve-buffer-id-color colors)))


(defun gui/resolve-buffer-id-color (color-map)
  "Resolve the correct color for the buffer name."
  (cond (buffer-read-only (modus-themes-get-color-value (cdr (assoc :readonly color-map))))
	((buffer-modified-p (window-buffer)) (modus-themes-get-color-value (cdr (assoc :modified color-map))))
	(t (modus-themes-get-color-value 'fg-main))))

(defun gui/update-modeline-color ()
  "Update the modeline color based on buffer modification status."
  (let ((color (gui/resolve-color-map)))
    (when (not(minibuffer-window-active-p (frame-selected-window)))
      (face-remap-add-relative 'mode-line-buffer-id :foreground color))))

(defun gui/fzf-find-file (&optional dir)
  "Find a file using fzf in a project root."
  (interactive)
  (let ((default-directory (or dir (project-root (project-current t)))))
    (fzf-find-file-in-dir default-directory)))

(defun gui/fzf-grep ()
  "Grep using fzf in a project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (fzf-grep-in-dir default-directory)))

(defun gui/diary-cyclic-entries (n year month day)
  "Return a cyclic diary entry that repeats every N days starting from YEAR, MONTH, DAY, but only on weekdays."
  (let ((date (calendar-absolute-from-gregorian (list month day year))))
    (if (and (zerop (% (- (calendar-absolute-from-gregorian date)
                          (calendar-absolute-from-gregorian (calendar-current-date)))
                       n))
             (not (member (calendar-day-of-week date) '(0 6))))
        "Weekday appointment at 10:00 AM")))

;;;; * Emacs defaults
(use-package emacs
  :custom
  (browse-url-new-window-flag t)
  (diary-file "~/Notes/diary")
  (vc-follow-symlinks t)
  :init
  (add-to-list 'display-buffer-alist
             '("\\`\\*\\(Warnings\\|Compile-Log\\)\\*\\'"
               (display-buffer-no-window)
               (allow-no-window . t)))
  (set-face-attribute 'default nil :family "Input Mono" :height 120 :weight 'regular)
  (set-face-attribute 'fixed-pitch nil :family "Input Mono" :height 120 :weight 'regular)
  (set-face-attribute 'variable-pitch nil :family "Input Serif" :height 120 :weight 'medium)
  (setq initial-major-mode 'fundamental-mode
	initial-scratch-message "Welcome to Guimacs"
	initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
	inhibit-startup-screen t
	inhibit-startup-echo-area-message t
	confirm-kill-processes nil
	use-short-answers t
	read-process-output-max (* 1024 1024)
	mac-command-modifier 'meta
	create-lockfiles nil
	display-time-24hr-format t
	display-time-day-and-date t
  	ring-bell-function 'ignore
	insert-directory-program "gls"
	appt-message-warning-time 60
	appt-display-interval 15
	warning-minimum-level :error
	tab-bar-tab-name-function (lambda () (buffer-name)))
  (setq-default cursor-type 'bar
		frame-title-format '("%b")
		fill-column 80)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (show-paren-mode t)
  (delete-selection-mode t)
  (put 'narrow-to-region 'disabled nil)
  (global-hl-line-mode 1)
  (column-number-mode 1)
  (pixel-scroll-precision-mode 1)
  (display-time)
  (appt-activate t)
  (if (eq system-type 'darwin)
      (setq appt-disp-window-function (lambda (remaining new-time msg) (org-show-notification msg))))
  (dolist (mapping '((python-mode . python-ts-mode)
		     (css-mode . css-ts-mode)
		     (js-mode . js-ts-mode)
		     (javascript-mode . js-ts-mode)
		     (typescript-mode . tsx-ts-mode)
		     (js-json-mode . json-ts-mode)
		     (sh-mode . bash-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :custom
  (column-number-mode nil)
  :bind ("C-c C-q" . info-apropos)
  :hook
  (text-mode-hook . auto-fill-mode)
  (flymake-after-save-hook . eglot-format-buffer)
  (find-file-hook . gui/enable-init-mode-for-init-file))

(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold (* 32 1024 1024))
  (gc-cons-percentage 0.8))

(use-package elec-pair
  :ensure t
  :defer t
  :config
  (defun gui-electric-pair-local-text-mode ()
    "Advise and wrap electric pairs in text mode"
    (add-function :before-until electric-pair-inhibit-predicate
		  (lambda (c) (eq c ?<)))
    (electric-pair-local-mode))
  :hook ((prog-mode-hook . electric-pair-local-mode)
	 (text-mode-hook . gui-electric-pair-local-text-mode)))

(use-package dired
  :ensure nil
  :config
  (when (and (eq system-type 'darwin) (executable-find "gls"))
    (setq insert-directory-program "gls"))
  :custom
  (dired-use-ls-dired t)
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

(use-package autorevert
  :defer 2
  :diminish auto-revert-mode)

(use-package vterm
  :ensure t
  :defer t)

(use-package multi-vterm
  :ensure t
  :defer t)

(use-package vertico
  :ensure t
  :pin elpa
  :init
  (vertico-mode)
  :config
  (vertico-multiform-mode 1)
  (add-to-list 'vertico-multiform-categories
	       '(jinx grid (vertico-grid-annotate . 20))))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :defer t
  :bind (:map vertico-map
	      ("RET" . vertico-directory-enter)
	      ("DEL" . vertico-directory-delete-char)
	      ("M-DEL" . vertico-directory-delete-word)))

(use-package project
  :ensure nil
  :config
  (setq project-vc-ignores '("target/" "bin/" "obj/")
	project-vc-extra-root-markers '("README.org"
					"README.md")))

;;;; * Benchmark
(use-package benchmark-init
  :ensure t
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
	  (lambda () (message "loaded in %s" (emacs-init-time))))

;;;; * Theming
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :custom
  (dashboard-startup-banner "~/SAPDevelop/dotfiles/emacs/.emacs.d/static/inter-removebg.png")
  (dashboard-center-content t)
  (dashboard-force-refresh t)
  (dashboard-week-agenda t)
  (dashboard-vertically-center-content t)
  (dashboard-items '())
  (dashboard-banner-logo-title "Welcome to Guimacs!"))

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

(use-package modus-themes
  :pin melpa
  :ensure t
  :init
  (setq modus-themes-org-blocks 'tinted-background
	modus-themes-italic-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-variable-pitch-ui nil
	modus-themes-common-palette-overrides '((bg-mode-line-active bg-graph-red-0)
						(bg-mode-line-inactive bg-dim)
						(border-mode-line-inactive bg-inactive)
						(fringe subtle)
						(bg-paren-match bg-yellow-intense)
						(custom-set-faces
						 '(mode-line ((t :family "Iosevka Comfy Wide" :height 100 :weight 'regular))))))
  (setq modus-themes-headings
	(quote ((1 . (overline variable-pitch 1.4))
		(2 . (overline variable-pitch 1.25))
		(3 . (overline 1.1))
		(t . (monochrome)))))
  :hook
  (first-change-hook . gui/update-modeline-color)
  (find-file-hook . gui/update-modeline-color)
  (after-save-hook . gui/update-modeline-color)
  (read-only-mode-hook . gui/update-modeline-color)
  (window-configuration-change-hook . gui/update-modeline-color)
  (post-command-hook . gui/update-modeline-color))


(use-package solar
  :custom
  (calendar-latitude -29.8)
  (calendar-longitude -51.16))

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '((:sunrise . modus-operandi)
			   (:sunset . modus-vivendi)))
  (circadian-setup))

(use-package emojify
  :ensure t
  :defer t)

;;;; * Completion & Navigation
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

(use-package which-key
  :ensure t
  :defer 4
  :diminish which-key-mode
  :config
  (which-key-mode 1))

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

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package treemacs
  :ensure t
  :defer t
  :custom
  (treemacs-no-png-images t)
  (treemacs-width 24)
  :bind ("C-c t" . treemacs))

(use-package fzf
  :ensure t
  :defer t
  :bind (("C-c f f" . fzf)
	 ("C-c f b" . fzf-switch-buffer)
	 ("C-c f r" . fzf-grep))
  :custom
  (fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll")
  (fzf/executable "fzf")
  (fzf/position-bottom t)
  (fzf/window-height 15)
  (fzf/grep-command "rg --no-heading -nH"))

(use-package deadgrep
  :ensure t
  :defer t
  :bind ("M-s o" . deadgrep))

(use-package avy
  :ensure t
  :defer t
  :bind (("M-g l" . avy-goto-line)
	 ("M-g e" . avy-goto-word-0)
	 ("M-g w" . avy-goto-word-1)
	 ("M-g t" . avy-goto-char-timer)))

(use-package outline
  :diminish outline-minor-mode
  :hook (prog-mode-hook . outline-minor-mode)
  :bind
  (:map outline-minor-mode-map
	("C-c s" . outline-toggle-children)
	("C-c e" . outline-show-all)))

(use-package casual-calc
  :ensure nil
  :bind (:map
	 calc-mode-map
	 ("C-o" . casual-calc-tmenu)
	 :map
	 calc-alg-map
	 ("C-o" . casual-calc-tmenu))
  :after (calc))
(use-package casual-isearch
  :ensure t
  :bind (:map isearch-mode-map
	      ("C-o" . casual-isearch-tmenu)))

(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map
	      ("C-o" . casual-dired-tmenu)
	      ("s" . casual-dired-sort-by-tmenu)
	      ("/" . casual-dired-search-replace-tmenu)))
(use-package casual-info
  :ensure t
  :bind (:map Info-mode-map
	      ("C-o" . casual-info-tmenu)))
(use-package ibuffer
  :ensure t
  :hook (ibuffer-mode . ibuffer-auto-mode))
(use-package casual-ibuffer
  :ensure t
  :bind (:map ibuffer-mode-map
	      ("C-o" . casual-ibuffer-tmenu)
	      ("F" . casual-ibuffer-filter-tmenu)
	      ("s" . casual-ibuffer-sortby-tmenu))
  :after (ibuffer))
(use-package bookmark
  :ensure nil
  :defer t)
(use-package casual-bookmarks
  :ensure t
  :bind (:map bookmark-bmenu-mode-map
              ("C-o" . casual-bookmarks-tmenu)
              ("S" . casual-bookmarks-sortby-tmenu)
              ("J" . bookmark-jump))
  :after (bookmark))
(use-package casual-avy
  :ensure t
  :bind ("M-g" . casual-avy-tmenu))
(use-package casual-symbol-overlay
  :ensure nil
  :bind (:map symbol-overlay-map
              ("C-o" . casual-symbol-overlay-tmenu))
  :after (symbol-overlay))

(use-package golden-ratio
  :ensure t
  :init
  (golden-ratio-mode 1))

;;;; * Windows & Movement
(use-package windmove
  :ensure nil
  :defer t
  :bind (("C-c <up>" . windmove-up)
	 ("C-c <right>" . windmove-right)
	 ("C-c <down>" . windmove-down)
	 ("C-c <left>" . windmove-left)))

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

;;;; * Spell checking
(use-package jinx
  :ensure t
  :defer t
  :hook ((text-mode-hook prog-mode-hook) . jinx-mode)
  :bind
  (("C-x C-;" . jinx-languages)
   ("M-$" . jinx-correct)))

(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :defer t
  :custom
  (flycheck-check-syntax-automatically '(mode-enabled save))
  :hook ((prog-mode-hook text-mode-hook) . flycheck-mode))

(use-package flymake
  :ensure t
  :defer t)

(use-package langtool
  :ensure t
  :defer t
  :custom
  (langtool-language-tool-jar "~/LanguageTool-6.4-stable/languagetool-commandline.jar")
  (langtool-java-classpath "~/LanguageTool-6.4-stable:/~/LanguageTool-6.4-stable/*"))

;;;; * Org-mode
(use-package org
  :ensure nil
  :diminish "Org"
  :custom
  (org-imenu-depth 7)
  (org-agenda-to-appt t)
  (org-fontify-done-headline nil)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line nil)
  (org-fontify-whole-block-delimiter-line t)
  (org-confirm-babel-evaluate nil)
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-preserve-indentation t)
  (org-highlight-lated-and-related '(latex))
  (line-spacing 3)
  (org-directory "~/Notes")
  (org-agenda-files '("~/Notes"))
  (org-startup-folded t)
  (org-startup-indented t)
  (org-default-notes-file (concat org-directory "/capture.org"))
  (org-clock-persist 'history)
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p@/!)" "HOLD(h)"
				 "|"
				 "DONE(d/!)" "FAIL(f@/!)")))
  :config
  (add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))
  (org-clock-persistence-insinuate)
  :hook
  (org-mode-hook . (lambda () (variable-pitch-mode 1)))
  (org-agenda-after-show-hook . (lambda () (org-agenda-to-appt t)))
  (after-save-hook . (lambda () (when (eq major-mode 'org-mode) (org-agenda-to-appt t)))))

(use-package org-alert
  :ensure t
  :custom
  (alert-default-style 'osx-notifier))

(use-package org-view-mode
  :ensure t
  :defer t)

(use-package org-special-block-extras
  :ensure t
  :defer t
  :hook (org-mode-hook . org-special-block-extras-mode))

(use-package org-superstar
  :ensure t
  :defer t
  :custom
  (org-superstar-headline-bullets-list  '("α" "β" "γ" "δ" "ε"))
  :hook (org-mode-hook . org-superstar-mode))

(use-package shackle
  :ensure t
  :defer t
  :hook (org-mode-hook . shackle-mode)
  :config
  (setq shackle-rules
	'((pdf-view-mode :align right))))

(use-package deft
  :ensure t
  :defer t
  :bind ("<f8>" . deft)
  :commands deft
  :custom
  (deft-recursive t)
  (deft-use-filename-as-title t)
  (deft-directory "~/Notes")
  (deft-extensions '("md" "org")))

(use-package imenu-list
  :ensure t
  :defer t
  :bind ("C-é" . imenu-list-smart-toggle))

(use-package ox-reveal
  :ensure t
  :defer 5)

(use-package denote
  :ensure t
  :defer t
  :config
  (denote-rename-buffer-mode 1)
  :custom
  (denote-directory (expand-file-name "~/Notes"))
  (denote-save-buffers nil)
  (denote-known-keyword '("emacs" "philosophy" "javascript" "politics" "economics"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-date-prompt-use-org-read-date t)
  (denote-backlinks-show-context t)
  :bind (("C-c n n" . denote) ; new
	 ("C-c n c" . denote-region) ; contents
	 ("C-c n f" . denote-type) ; file
	 ("C-c n d" . denote-date) ; date
	 ("C-c n z" . denote-signature) ; zettelkasten
	 ;; Backlinks
	 ("C-c n i" . denote-link) ; insert
	 ("C-c n I" . denote-add-links) ; multi-links
	 ("C-c n b" . denote-backlinks)
	 :map dired-mode-map
	 ("C-c C-d C-i" . denote-link-dired-marked-notes)
	 ("C-c C-d C-r" . denote-dired-rename-files)))

(use-package hl-todo
  :ensure t
  :defer t
  :hook (text-mode-hook . hl-todo-mode))

(use-package calfw
  :ensure t)

(use-package calfw-org
  :ensure t
  :bind
  ("C-c c" . cfw:open-org-calendar))

(use-package brazil-holidays
  :straight (:type git :host github :repo "luksamuk/brazil-holidays")
  :custom
  (calendar-holidays holiday-brazil-holidays))
;;;; * Focus
(use-package olivetti
  :ensure t
  :demand t
  :diminish
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

;;;; * Reading
(use-package pdf-tools
  :ensure t
  :defer t
  :magic ("%PDF" . pdf-view-mode)
  :hook (TeX-after-compilation-finished-hook . TeX-revert-document-buffer)
  :defines pdf-annot-activate-created-annotations
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.05)
  (pdf-annot-activate-created-annotations t)
  :config
  (pdf-tools-install :no-query)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward)
	      ("C-r" . isearch-backward)))

;;;; * Markdown
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :custom
  (markdown-fontify-code-blocks-natively t)
  (markdown-command "pandoc")
  :defines markdown-mode-map
  :bind (:map markdown-mode-map
	      ("C-c <left>" . nil)
	      ("C-c <right>" . nil)
	      ("C-c <up>" . nil)
	      ("C-c <down>" . nil)))

;;;; * Exporting text files
(use-package pandoc-mode
  :ensure t
  :defer t)

;;;; * Version control
(use-package magit
  :ensure t
  :pin melpa
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode-hook . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t)

;;;; * Editing
(use-package iedit
  :ensure t
  :defer t
  :bind ("C-:" . iedit-mode))

(use-package rect
  :ensure nil
  :defer t
  :bind ("C-x <SPC>" . rectangle-mark-mode))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :ensure t
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

;;;; * Eglot
(use-package eglot
  :pin elpa
  :ensure t
  :defer t
  :custom
  (read-process-output-max (* 1024 1024))
  (eldoc-echo-area-use-multiline-p)
  (eglot-autoshutdown)
  :hook ((c-ts-mode-hook . eglot-ensure)
	 (css-ts-mode-hook . eglot-ensure)
	 (html-mode-hook . eglot-ensure)
	 (js-base-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . eglot-ensure)
	 (tsx-ts-mode-hook . prettier-mode)
	 (css-ts-mode-hook . prettier-mode)
	 (json-ts-mode-hook . prettier-mode)
	 (go-ts-mode-hook . eglot-ensure)
	 (latex-mode-hook . eglot-ensure))
  :bind (("C-c l b" . eglot-format-buffer)
	 ("C-c l a" . eglot-code-actions)
	 ("C-c l e" . eglot-reconnect)
	 ("C-c l r" . eglot-rename)))

(use-package yaml-pro
  :ensure t
  :defer t)
(use-package elisp-mode
  :config
  :diminish "EL")

(use-package buttercup
  :ensure t
  :defer t)

(use-package package-lint
  :ensure t
  :defer t)

(use-package elisp-lint
  :ensure t
  :defer t)

(use-package xr
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

;;;; * Org-mode Babel
(use-package ob-shell
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh

   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-emacs-lisp
  :defer t
  :commands (org-babel-execute:elisp
	     org-babel-expand-body:elisp
	     org-babel-execute:emacs-lisp
	     org-babel-expand-body:emacs-lisp))

(use-package ob-js
  :ensure nil
  :defer t
  :commands (org-babel-execute:js))

(use-package ob-makefile
  :defer t)

(use-package verb
  :ensure t
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
;;;; * Quality of Life
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package multiple-cursors
  :ensure t
  :defer t
  :bind
  (("C-<" . mc/mark-all-like-this-dwim)
   ("C->" . mc/mark-all-dwim)))

(use-package undo-fu
  :ensure t
  :defer t
  :diminish
  :bind
  ("C-z" . undo-fu-only-undo)
  ("C-S-z" . undo-fu-only-redo))

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

(use-package cmake-ts-mode
  :ensure t
  :defer t)

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :defer t
  :diminish
  :hook (prog-mode-hook . smartparens-mode))

(use-package colorful-mode
  :ensure t
  :hook (prog-mode . text-mode))

(use-package pulsic
  :init
  (unless (package-installed-p 'pulsic)
    (package-vc-install
     '(pulsic :vc-backend Git
              :url "https://github.com/ichernyshovvv/pulsic.el")))
  :config
  (pulsic-mode 1)
  :custom
  (pulsic-duration 0.5)
  (pulsic-predicate
   (lambda ()
     (not
      (or (memq last-command
                '( chloe-clock-in-dwim chloe-clock-in
                   indent-for-tab-command))
          (derived-mode-p 'telega-chat-mode 'enlight-mode)
          (minibufferp))))))

(use-package spacious-padding
  :ensure t
  :config (spacious-padding-mode 1))
;;;; * Docker
(use-package docker
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :defer t)
(use-package kubernetes
  :ensure t
  :defer t
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency 3600
	kubernetes-redraw-frequency 3600))
;;;; * Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-agenda-prefix-format " %i %-12:c %s ")
 '(dashboard-agenda-sort-strategy '(time-up))
 '(dashboard-projects-switch-function 'project-switch-project)
 '(mode-line-format
   '("%e" mode-line-front-space (:propertize ("" "λ:") display (min-width (6.0)))
     mode-line-frame-identification mode-line-buffer-identification "    "
     (project-mode-line project-mode-line-format) (vc-mode vc-mode) "  "
     mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(org-agenda-include-diary t)
 '(org-agenda-start-on-weekday 0)
 '(org-habit-show-all-today nil)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc
	     ol-mhe ol-rmail ol-w3m))
 '(package-vc-selected-packages
   '((pulsic :vc-backend Git :url "https://github.com/ichernyshovvv/pulsic.el")))
 '(project-switch-commands
   '((gui/fzf-find-file "Find file" 102) (gui/fzf-grep "Find regexp" 114)
     (project-find-dir "Find directory" nil) (magit-project-status "Magit" 109)))
 '(sudoku-level 'easy))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
