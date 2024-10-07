;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs early init file.

;;; Code:
(defconst emacs-start-time (current-time))

(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      package-enable-at-startup t
      package-quickstart t
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      load-prefer-newer t
      native-comp-async-report-warning-errors nil)


(defvar file-name-handler-alist-old file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist file-name-handler-alist-old)))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mode-line-format . 0) default-frame-alist)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(provide 'early-init)

;;; early-init.el ends here
