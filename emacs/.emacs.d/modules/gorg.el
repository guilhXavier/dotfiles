;;; gorg.el --- Org configs
;;; Commentary:
;;; Code:
(use-package org
  :ensure nil
  :config
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)
  :custom
  (org-export-with-smart-quotes t)
  (org-directory "~/Beorg/")
  (org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p@/!)" "HOLD(h)"
				 "|"
				 "DONE(d/!)" "FAIL(f@/!)"))
  (org-agenda-files '("inbox.org" "work.org"))
  (org-tag-list '(
		  ;; locale
		  (:startgroup)
		  ("home" . ?h)
		  ("work" . ?w)
		  ("school" . ?s)
		  (:endgroup)
		  (:newline)
		  ("one-shot" . ?o)
		  ("project" . ?j)
		  ("tiny" . ?t)
		  (:endgroup)
		  ("meta")
		  ("review")
		  ("reading")))
  (org-refile-targets 'FIXME)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-capture-templates '(("c" "Default Capture" entry (file "inbox.org")
			    "* TODO %?\n%U\n%i")
			   ;; Capture and keep an org-link to the thing we're currently working with
			   ("r" "Capture with Reference" entry (file "inbox.org")
			    "* TODO %?\n%U\n%i\n%a")
			   ;; Define a section
			   ("w" "Work")
			   ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
			    "** TODO %?\n%U\n%i\n%a")
			   ("wr" "Work report" entry (file+headline "work.org" "Reports")
			    "** TODO %?\n%U\n%i\n%a")))
  (org-agenda-custom-commands '(("n" "Agenda and All Todos"
				 ((agenda)
				  (todo)))
				("w" "Work" agenda ""
				 ((org-agenda-files '("work.org"))))))
  (org-startup-indented t)
  (dictionary-server "dict.org")
  (dictionary-use-single-buffer t)
  :hook (org-mode-hook . visual-line-mode)
  :bind (:map global-map
	      ("C-c l s" . org-store-link)
	      ("C-c l i" . org-insert-link-global))))

(use-package hl-todo
  :ensure t
  :defer t
  :hook (text-mode-hook . hl-todo-mode))

(use-package jinx
  :ensure t
  :defer t
  :config
  (add-to-list 'vertico-multiform-categories
	       '(jinx grid (vertico-grid-annotate . 20)))
  :custom
  (jinx-camel-modes '(prog-mode))
  (jinx-delay 0.01)
  :hook (emacs-startup-hook . global-jinx-mode)
  :bind (("C-x C-;" . jinx-languages)
         ("M-$" . jinx-correct)))

(use-package langtool
  :ensure t
  :defer t
  :custom
  (langtool-language-tool-jar "~/LanguageTool-6.4-stable/languagetool-commandline.jar")
  (langtool-java-classpath "~/LanguageTool-6.4-stable:/~/LanguageTool-6.4-stable/*"))

(use-package olivetti
  :ensure t
  :demand t
  :diminish
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

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

(provide 'gorg)
;;; gorg.el ends here
