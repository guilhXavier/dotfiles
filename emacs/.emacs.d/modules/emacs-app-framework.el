;;; emacs-app-framework.el --- EAF Config
;;; Commentary:
;;; Code:

(use-package eaf
  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser))

(require 'eaf-browser)
(require 'eaf-pdf-viewer)

(provide 'emacs-app-framework)
;;; emacs-app-framework.el ends here
