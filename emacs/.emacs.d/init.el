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

(use-package diminish :ensure t :after use-package)

;;;; * Guimacs
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
  (advice-add 'gui/move-to-point-max :after #'copilot-chat--write-buffer))

(defun gui/fzf-find-file (&optional dir)
  "Find a file using fzf in a project root."
  (interactive)
  (let ((default-directory (or dir (project-root (project-current t)))))
    (fzf-git-files)))

(defun gui/fzf-grep ()
  "Grep using fzf in a project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (fzf-git-grep)))

(load-file (expand-file-name "modules/base.el" user-emacs-directory))

(load-file (expand-file-name "modules/theming.el" user-emacs-directory))

(load-file (expand-file-name "modules/completion.el" user-emacs-directory))

(mini-echo-define-segment "persp"
  "Get the current perspective list"
  :setup (persp-init-frame (selected-frame))
  :fetch (mini-echo-segment--extract (frame-parameter nil 'persp--modestring)))

(use-package mini-echo
  :after perspective
  :ensure t
  :config
  (mini-echo-mode)
  :custom  (mini-echo-persistent-rule '(:long ("major-mode" "shrink-path" "vcs" "flymake" "persp")
					      :short ("buffer-name" "flymake"))))

(load-file (expand-file-name "modules/gorg.el" user-emacs-directory))

(load-file (expand-file-name "modules/dev.el" user-emacs-directory))

;;;; * Custom variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(copilot-enable-predicates nil)
 '(copilot-log-max 1000)
 '(custom-safe-themes
   '("2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" default))
 '(dashboard-agenda-prefix-format " %i %-12:c %s ")
 '(dashboard-agenda-sort-strategy '(time-up))
 '(dashboard-projects-switch-function 'project-switch-project)
 '(exec-path
   '("/usr/bin" "/bin" "/usr/sbin" "/sbin" "/opt/homebrew/Cellar/emacs-plus@29/29.4/libexec/emacs/29.4/aarch64-apple-darwin23.6.0" "/opt/homebrew/bin/"))
 '(mode-line-format
   '("%e" mode-line-front-space
     (:propertize
      ("" "λ:")
      display
      (min-width
       (6.0)))
     mode-line-frame-identification mode-line-buffer-identification "    "
     (project-mode-line project-mode-line-format)
     (vc-mode vc-mode)
     "  " mode-line-modes mode-line-misc-info mode-line-end-spaces))
 '(org-agenda-include-diary t)
 '(org-agenda-start-on-weekday 0)
 '(org-habit-show-all-today nil)
 '(org-modules
   '(ol-bbdb ol-bibtex ol-docview ol-doi ol-eww ol-gnus org-habit ol-info ol-irc ol-mhe ol-rmail ol-w3m))
 '(package-vc-selected-packages
   '((pulsic :vc-backend Git :url "https://github.com/ichernyshovvv/pulsic.el")))
 '(project-switch-commands
   '((gui/fzf-find-file "Find file" 102)
     (gui/fzf-grep "Find regexp" 114)
     (project-find-dir "Find directory" nil)
     (magit-project-status "Magit" 109)))
 '(spacious-padding-widths
   '(:internal-border-width 15 :header-line-width 4 :mode-line-width 6 :tab-width 4 :right-divider-width 1 :scroll-bar-width 8 :fringe-width 8))
 '(sudoku-level 'easy))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mini-echo-project ((t (:inherit mini-echo-char-info)))))
