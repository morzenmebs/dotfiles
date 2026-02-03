;;; init.el --- Main configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Vanilla-leaning Emacs config for Guix + EXWM.
;; Packages managed by Guix, not package.el.

;;; Code:

;;;; ---- Load Path (Guix packages) ----

(dolist (dir '("~/.guix-home/profile/share/emacs/site-lisp/"
               "~/.guix-profile/share/emacs/site-lisp/"))
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (normal-top-level-add-subdirs-to-load-path))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;;;; ---- Vanilla-Leaning Spine ----

;; Startup/performance
(setq inhibit-startup-screen t
      ring-bell-function #'ignore
      read-process-output-max (* 1024 1024))

;; Custom file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file nil 'nomessage))

;; UI - keep menu bar (useful when learning), no toolbar
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode 1)

;; Core editing
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq-default indent-tabs-mode nil)

;; Word wrap text modes
(add-hook 'text-mode-hook #'visual-line-mode)

;; Clipboard: Emacs <-> X apps
(setq select-enable-clipboard t
      select-enable-primary nil)

;; Quality of life
(recentf-mode 1)
(global-auto-revert-mode 1)
(setq require-final-newline t)
(winner-mode 1)
(windmove-default-keybindings)  ; Shift+arrows move between windows

;; Backups out of the way
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory))))
(make-directory (expand-file-name "backups" user-emacs-directory) t)

;;;; ---- Per-Window Tabs (tab-line) ----

(global-tab-line-mode 1)
(setq tab-line-tabs-function #'tab-line-tabs-window-buffers
      tab-line-switch-cycling t
      tab-line-close-button-show t
      tab-line-new-button-show nil)

(defun my/tab-new ()
  "Switch buffer (shows as new tab)."
  (interactive)
  (call-interactively #'switch-to-buffer))

(global-set-key (kbd "C-x t n") #'tab-line-switch-to-next-tab)
(global-set-key (kbd "C-x t p") #'tab-line-switch-to-prev-tab)
(global-set-key (kbd "C-x t k") #'tab-line-close-tab)
(global-set-key (kbd "C-x t t") #'my/tab-new)

;;;; ---- Init File Management ----

(defun open-init ()
  "Open init.el."
  (interactive)
  (find-file "~/dotfiles/emacs/init.el"))

;; (defun reload-init ()
;;   "Reload init.el."
;;   (interactive)
;;   (load-file user-init-file)
;;   (message "init reloaded"))

(global-set-key (kbd "C-c e i") #'open-init)
;; (global-set-key (kbd "C-c e r") #'reload-init)

(defun my/guix-home-reconfigure-and-logout ()
  "Run `guix home reconfigure ~/dotfiles/guix/home.scm` then exit Emacs/EXWM."
  (interactive)
  (save-some-buffers t)
  (let* ((home-scm (expand-file-name "~/dotfiles/guix/home.scm"))
         (cmd (format "guix home reconfigure %s" (shell-quote-argument home-scm)))
         (buf (get-buffer-create "*guix-home-reconfigure*"))
         (proc (start-process-shell-command "guix-home-reconfigure" buf cmd)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (format "$ %s\n\n" cmd))
      (read-only-mode 1)
      (display-buffer buf))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (memq (process-status p) '(exit signal))
         (let ((code (process-exit-status p)))
           (if (= code 0)
               (progn
                 (message "Guix Home reconfigure succeeded; logging out.")
                 (save-buffers-kill-emacs))
             (message "Guix Home reconfigure failed (exit %d). See *guix-home-reconfigure*." code))))))))

(global-set-key (kbd "C-c e r") #'my/guix-home-reconfigure-and-logout)

;;;; ---- Dired Sidebar ----

(defun my/dired-sidebar ()
  "Open project root in side window."
  (interactive)
  (let ((buf (dired-noselect (or (when-let ((pr (project-current nil)))
                                   (project-root pr))
                                 default-directory))))
    (display-buffer-in-side-window
     buf '((side . left) (window-width . 0.25)))))

(global-set-key (kbd "C-c d") #'my/dired-sidebar)

;;;; ---- Dired click behavior ----

(with-eval-after-load 'dired
  ;; Silence the “disabled command” prompt for alternate-file.
  (put 'dired-find-alternate-file 'disabled nil)

  (defun my/dired-mouse-1-smart (event)
    "In Dired: click dirs => reuse buffer; click files => open normally (same window)."
    (interactive "e")
    (mouse-set-point event)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))

  ;; Remap whatever Dired uses under the hood for mouse visits.
  (define-key dired-mode-map [remap dired-mouse-find-file] #'my/dired-mouse-1-smart)
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window] #'my/dired-mouse-1-smart)

  ;; And also bind mouse-1 directly (covers cases without remap).
  (define-key dired-mode-map [mouse-1] #'my/dired-mouse-1-smart))

;;;; ---- Org Mode Keys ----

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;;; ---- use-package (for config organization) ----

(require 'use-package)
(setq use-package-always-ensure nil)  ; Guix handles packages

(require 'which-key)
(which-key-mode 1)

;;;; ---- Load Modules ----

;; Theming (your my-terza theme, no-bold enforcement)
(require 'init-theming)

;; EXWM (only when running as window manager)
;; (when (and (eq window-system 'x)
;;            (or (member "--fullscreen" command-line-args)
;;                (getenv "EXWM_RUNNING")))
;;   (setenv "EXWM_RUNNING" "1")
;;   (require 'init-exwm))
(with-eval-after-load 'exwm
  (require 'init-exwm))

;;;; ---- User Customizations ----

;; Put personal additions in ~/.config/emacs/user.el
(let ((user-custom (expand-file-name "user.el" user-emacs-directory)))
  (when (file-exists-p user-custom)
    (load user-custom)))

(provide 'init)
;;; init.el ends here
