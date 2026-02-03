;;; init-exwm.el --- EXWM configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Mouse-friendly EXWM setup with minimal keybinding interference.
;; Super (Win) key used for WM actions to avoid shadowing app keys.

;;; Code:

(require 'exwm)
(require 'exwm-randr)
(require 'exwm-systemtray)

;;;; ---- Basic Settings ----

(setq exwm-workspace-number 4)

;; Buffer naming: use window class/title
(add-hook 'exwm-update-class-hook
          (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;;;; ---- Input Configuration ----

;; MINIMAL prefix keys - let most keys through to apps
;; Only intercept what's essential for Emacs operation
(setq exwm-input-prefix-keys
      '(?\C-x     ; Emacs prefix
        ?\C-c     ; mode-specific prefix
        ?\C-g     ; quit
        ?\M-x     ; execute command
        ?\M-:))   ; eval

;; C-q sends next key literally to app
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Global keys (Super/Win key for WM, minimal interference)
(setq exwm-input-global-keys
      `(;; Essential
        ([?\s-r] . exwm-reset)              ; back to line-mode
        ([?\s-g] . keyboard-quit)           ; quit
        
        ;; Workspace switching (s-1 through s-4)
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda () (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 1 4))
        
        ;; App launchers
        (,(kbd "s-RET") . (lambda () (interactive)
                          (start-process "" nil "xfce4-terminal")))
        ([?\s-b] . (lambda () (interactive)
                     (start-process "" nil "firefox")))
        (,(kbd "s-SPC") . (lambda () (interactive)
                         (start-process "" nil "dmenu_run")))
        
        ;; Window management
        ([?\s-f] . exwm-layout-toggle-fullscreen)
        ([?\s-t] . exwm-floating-toggle-floating)
        ([?\s-q] . (lambda () (interactive) (kill-buffer)))
        
        ;; Move buffer to other window (useful!)
        ([?\s-o] . other-window)
        ([?\s-m] . (lambda () (interactive)
                     (let ((buf (current-buffer)))
                       (other-window 1)
                       (switch-to-buffer buf))))))

(require 'exwm-input)
(dolist (pair exwm-input-global-keys)
  (exwm-input-set-key (car pair) (cdr pair)))

;;;; ---- Mouse Configuration ----

;; Move/resize floating windows with mouse
(setq exwm-floating-border-width 2
      exwm-floating-border-color "#45243b")

;; Mouse follows focus
(setq focus-follows-mouse nil
      mouse-autoselect-window nil)

;;;; ---- Simulation Keys ----

;; Minimal - only what you actually want translated
;; Since you're mouse-heavy and don't rely on Emacs keys,
;; keeping this minimal avoids unexpected behavior
(setq exwm-input-simulation-keys
      '(;; Just clipboard, really
        ([?\C-w] . [?\C-x])   ; cut
        ([?\M-w] . [?\C-c])   ; copy  
        ([?\C-y] . [?\C-v]))) ; paste

;;;; ---- System Tray ----

;; (exwm-systemtray-mode 1)

;;;; ---- Desktop Environment (brightness/volume keys) ----

(use-package desktop-environment
  :after exwm
  :config
  (desktop-environment-mode)
  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")
  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-"))

;;;; ---- Window Manipulation Helpers ----

(defun my/exwm-move-buffer-to-window (n)
  "Move current buffer to window N (1-indexed), stay in original window."
  (interactive "nMove to window: ")
  (let ((buf (current-buffer))
        (windows (window-list)))
    (when (<= n (length windows))
      (set-window-buffer (nth (1- n) windows) buf))))

(defun my/exwm-swap-buffers ()
  "Swap current buffer with buffer in other window."
  (interactive)
  (let* ((this-win (selected-window))
         (other-win (next-window))
         (this-buf (window-buffer this-win))
         (other-buf (window-buffer other-win)))
    (set-window-buffer this-win other-buf)
    (set-window-buffer other-win this-buf)))

(global-set-key (kbd "s-S") #'my/exwm-swap-buffers)

;;;; ---- Screenshot helpers ----

(defun my/screenshot-region-to-clipboard ()
  (interactive)
  (start-process-shell-command
   "shot-region-clip" nil
   "maim -s | copyq copy image/png -"))

(defun my/screenshot-full-to-clipboard ()
  (interactive)
  (start-process-shell-command
   "shot-full-clip" nil
   "maim | copyq copy image/png -"))

(with-eval-after-load 'desktop-environment
  (define-key desktop-environment-mode-map (kbd "<print>") nil)
  (define-key desktop-environment-mode-map (kbd "S-<print>") nil))

(with-eval-after-load 'exwm
  ;; Print      -> region to clipboard
  (exwm-input-set-key (kbd "<print>") #'my/screenshot-region-to-clipboard)

  ;; M-Print    -> full screen to clipboard
  (exwm-input-set-key (kbd "M-<Sys_Req>") #'my/screenshot-full-to-clipboard))

;;;; ---- Views system ----

(defvar my/exwm-views nil
  "Alist of (NAME . FN). FN is an interactive function that sets up a view.")

(defvar my/exwm-default-view 'home
  "Symbol name of the default view to apply at EXWM startup.")

(defun my/exwm-define-view (name fn)
  (setf (alist-get name my/exwm-views) fn))

(defun my/exwm-apply-view (name)
  "Apply a named EXWM view."
  (interactive
   (list (intern
          (completing-read "View: "
                           (mapcar (lambda (x) (symbol-name (car x))) my/exwm-views)
                           nil t))))
  (let ((fn (alist-get name my/exwm-views)))
    (unless fn (user-error "Unknown view: %S" name))
    (funcall fn)
    (message "View: %s" name)))

;; Default Home view

(defvar my/exwm--right-window nil)

(defun my/exwm-view-home ()
  "3-pane layout: right=Firefox, left top=Dired ~/, left bottom=shell, ratio 2:1."
  (interactive)
  (delete-other-windows)
  (setq my/exwm--right-window nil)

  (let* ((left (selected-window))
         (right (split-window-right))
         (bottom-height (max window-min-height (floor (/ (window-total-height left) 3))))
         (bottom (split-window-below (- bottom-height)))) ; negative => bottom gets that height
    (setq my/exwm--right-window right)

    ;; Left top: Dired
    (select-window left)
    (dired (expand-file-name "~"))

    ;; Left bottom: shell (short one)
    (select-window bottom)
    (shell "*shell*")

    ;; Start Firefox; manage hook will place it into RIGHT.
    (start-process "firefox" nil "firefox")))

(my/exwm-define-view 'home #'my/exwm-view-home)

(defun my/exwm-place-firefox ()
  (when (and my/exwm--right-window
             (derived-mode-p 'exwm-mode)
             (boundp 'exwm-class-name)
             (string= exwm-class-name "Firefox"))
    (set-window-buffer my/exwm--right-window (current-buffer))))

;; Window switch keys
(exwm-input-set-key (kbd "s-v") #'my/exwm-apply-view)         ; pick view
(exwm-input-set-key (kbd "s-h") (lambda () (interactive) (my/exwm-apply-view 'home))) ; home


;;;; ---- Startup ----

(defun my/exwm-startup ()
  "Start background apps."
  ;; Network manager
  (start-process-shell-command "nm-applet" nil "nm-applet"))

(add-hook 'exwm-init-hook #'my/exwm-startup)

;; Make sure system tray is on
(add-hook 'exwm-init-hook #'exwm-systemtray-mode)

;; Apply default view
(add-hook 'exwm-init-hook (lambda () (my/exwm-apply-view my/exwm-default-view)))
(add-hook 'exwm-manage-finish-hook #'my/exwm-place-firefox)


;;;; ---- Enable EXWM ----
;; Probably wrong
;; (exwm-enable)

(provide 'init-exwm)
;;; init-exwm.el ends here
