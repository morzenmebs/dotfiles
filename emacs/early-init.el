;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded before init.el.  Used for performance tweaks and
;; disabling package.el (we use Guix for package management).

;;; Code:

;; Disable package.el - Guix manages our packages
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs
(setq inhibit-redisplay t
      inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq inhibit-redisplay nil
                  inhibit-message nil)
            (redisplay)))

;; Increase gc threshold during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Reset gc after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)  ; 16mb
                  gc-cons-percentage 0.1)))

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)

;; Disable UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Frame settings
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)))

(provide 'early-init)
;;; early-init.el ends here
