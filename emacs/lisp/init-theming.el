;;; init-theming.el --- Theme and appearance -*- lexical-binding: t -*-

;;; Commentary:
;; my-terza theme with aggressive no-bold enforcement.
;; Font: Terza Editor ss02 (install .otf to ~/.local/share/fonts/)

;;; Code:

;;;; ---- Custom Faces for Org Emphasis ----

(defface my/org-emphasis-bold
  '((t :inherit warning :weight normal))
  "Face for *bold* Org emphasis (no actual bold).")

(defface my/org-emphasis-italic
  '((t :inherit shadow :weight normal :slant normal))
  "Face for /italic/ Org emphasis.")

;;;; ---- No Bold Anywhere ----

;; Remap bold to default everywhere
(defun my/no-bold-remap ()
  "Prevent bold in current buffer."
  (add-to-list (make-local-variable 'face-remapping-alist)
               '(bold . default)))
(add-hook 'after-change-major-mode-hook #'my/no-bold-remap)

;; Strip bold from common faces
(defun my/strip-bold-from-faces ()
  "Set weight to normal on faces that tend to be bold."
  (dolist (f '(bold bold-italic mode-line mode-line-inactive header-line
               tab-line tab-line-tab tab-line-tab-current tab-line-tab-inactive
               font-lock-keyword-face font-lock-builtin-face
               font-lock-function-name-face font-lock-type-face
               font-lock-constant-face font-lock-variable-name-face
               font-lock-warning-face
               mode-line-buffer-id mode-line-emphasis mode-line-highlight
               tab-line-tab-modified tab-line-tab-inactive-modified
               tab-line-tab-special
               tab-bar tab-bar-tab tab-bar-tab-inactive tab-bar-tab-ungrouped
               buffer-menu-buffer))
    (when (facep f)
      (set-face-attribute f nil :weight 'normal))))

(my/strip-bold-from-faces)

;; Re-strip after theme changes
(advice-add 'enable-theme :after
            (lambda (&rest _) (my/strip-bold-from-faces)))

;; Mode-line buffer name without bold
(setq-default mode-line-buffer-identification
              '(:eval (propertize " %b " 'face 'mode-line)))

;;;; ---- Font Configuration ----

(defun my/setup-fonts ()
  "Configure fonts."
  (when (display-graphic-p)
    ;; Main font: Terza Editor ss02 (install to ~/.local/share/fonts/)
    ;; Falls back to DejaVu if not found
    (let ((font (if (member "Terza Editor ss02" (font-family-list))
                    "Terza Editor ss02"
                  "DejaVu Sans Mono")))
      (set-face-attribute 'default nil
                          :family font
                          :height 114
                          :weight 'regular))

    ;; APL/box-drawing characters (if you have APL385 Terza Grid)
    (when (member "APL385 Terza Grid" (font-family-list))
      (set-fontset-font t '(#x2200 . #x23FF) "APL385 Terza Grid" nil 'prepend)
      (set-fontset-font t '(#x2500 . #x25FF) "APL385 Terza Grid" nil 'prepend))))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame (my/setup-fonts))))
  (my/setup-fonts))

;;;; ---- my-terza Theme ----

(deftheme my-terza "Minimal, quiet theme with normal-weight faces.")

(let* ((bg      "#f7f6f5")
       (fg      "#000f21")
       (dim     "#705959")
       (accent  "#45243b")
       (accent2 "#55333f")
       (warn    "#a01e1e")
       (warn2   "#701a28")
       (region  "#ceccca")
       (region2 "#edecea")
       (hl      "#ada8a6")
       (line    "#ada8a6"))
  (custom-theme-set-faces
   'my-terza

   ;; Base
   `(default              ((t (:background ,bg :foreground ,fg :weight normal))))
   `(cursor               ((t (:background ,accent))))
   `(fringe               ((t (:background ,bg :foreground ,dim))))
   `(region               ((t (:background ,region :foreground ,fg))))
   `(hl-line              ((t (:background ,hl))))
   `(vertical-border      ((t (:foreground ,line))))

   ;; Mode line
   `(mode-line            ((t (:background ,line :foreground ,fg :box (:line-width 1 :color ,line) :weight normal))))
   `(mode-line-inactive   ((t (:background ,bg :foreground ,dim :box (:line-width 1 :color ,line) :weight normal))))
   `(mode-line-buffer-id  ((t (:inherit mode-line :weight normal))))
   `(mode-line-emphasis   ((t (:weight normal))))
   `(mode-line-highlight  ((t (:weight normal))))

   ;; Tab line (per-window tabs)
   `(tab-line             ((t (:background ,bg :foreground ,dim :box nil :weight normal))))
   `(tab-line-tab         ((t (:background ,bg :foreground ,dim :box (:line-width 1 :color ,line) :weight normal))))
   `(tab-line-tab-current ((t (:background ,line :foreground ,fg :box (:line-width 1 :color ,line) :weight normal))))
   `(tab-line-tab-inactive((t (:background ,bg :foreground ,dim :box (:line-width 1 :color ,line) :weight normal))))

   ;; Font lock
   `(font-lock-comment-face       ((t (:foreground ,dim :slant normal :weight normal))))
   `(font-lock-string-face        ((t (:foreground ,accent2 :weight normal))))
   `(font-lock-keyword-face       ((t (:foreground ,accent :weight normal))))
   `(font-lock-function-name-face ((t (:foreground ,fg :weight normal))))
   `(font-lock-type-face          ((t (:foreground ,accent :weight normal))))
   `(font-lock-constant-face      ((t (:foreground ,accent :weight normal))))
   `(font-lock-warning-face       ((t (:foreground ,warn :weight normal))))
   `(font-lock-builtin-face       ((t (:foreground ,accent :weight normal))))
   `(font-lock-variable-name-face ((t (:foreground ,fg :weight normal))))

   ;; Comint/REPL
   `(comint-highlight-input  ((t (:inherit default :weight normal))))
   `(comint-highlight-prompt ((t (:foreground ,accent :weight normal))))

   ;; Org headings
   `(org-level-1 ((t (:foreground ,accent :weight normal :height 1.0 :inherit nil))))
   `(org-level-2 ((t (:foreground ,accent2 :weight normal :height 1.0 :inherit nil))))
   `(org-level-3 ((t (:foreground ,accent :weight normal :inherit nil))))
   `(org-level-4 ((t (:foreground ,accent2 :weight normal :inherit nil))))
   `(org-level-5 ((t (:foreground ,accent :weight normal :inherit nil))))
   `(org-level-6 ((t (:foreground ,accent2 :weight normal :inherit nil))))
   `(org-level-7 ((t (:foreground ,accent :weight normal :inherit nil))))
   `(org-level-8 ((t (:foreground ,accent2 :weight normal :inherit nil))))

   ;; Org metadata
   `(org-document-title ((t (:foreground ,accent :weight normal :height 1.0 :inherit nil))))
   `(org-document-info ((t (:foreground ,dim :inherit nil))))
   `(org-document-info-keyword ((t (:foreground ,dim :inherit nil))))
   `(org-meta-line ((t (:foreground ,dim :slant normal :inherit nil))))

   ;; Org blocks
   `(org-block ((t (:background ,region2 :foreground ,fg :inherit nil))))
   `(org-block-begin-line ((t (:foreground ,dim :background ,bg :inherit nil))))
   `(org-block-end-line ((t (:foreground ,dim :background ,bg :inherit nil))))
   `(org-code ((t (:foreground ,fg :background ,hl :inherit nil))))
   `(org-verbatim ((t (:foreground ,fg :background ,region2 :inherit nil))))

   ;; Org misc
   `(org-link ((t (:foreground ,accent :underline t :inherit nil))))
   `(org-date ((t (:foreground ,accent2 :underline t :inherit nil))))
   `(org-todo ((t (:foreground ,warn :weight normal :inherit nil))))
   `(org-done ((t (:foreground ,dim :weight normal :inherit nil))))
   `(org-headline-done ((t (:foreground ,dim :strike-through t :inherit nil))))
   `(org-special-keyword ((t (:foreground ,dim :inherit nil))))
   `(org-drawer ((t (:foreground ,dim :inherit nil))))
   `(org-checkbox ((t (:foreground ,accent :inherit nil))))
   `(org-table ((t (:foreground ,fg :inherit nil))))

   ;; Custom org emphasis
   `(my/org-emphasis-bold ((t (:foreground ,warn2 :weight normal :inherit nil))))
   `(my/org-emphasis-italic ((t (:foreground ,dim :weight normal :inherit nil))))))

(provide-theme 'my-terza)
(enable-theme 'my-terza)

;;;; ---- Org Emphasis Setup ----

(setq org-fontify-emphasized-text t)

(with-eval-after-load 'org
  (setq org-emphasis-alist
        '(("*" my/org-emphasis-bold)
          ("/" my/org-emphasis-italic)
          ("_" underline)
          ("=" org-verbatim verbatim)
          ("~" org-code verbatim)
          ("+" (:strike-through t))))
  (org-set-emph-re 'org-emphasis-regexp-components
                   org-emphasis-regexp-components)
  (setq org-hide-emphasis-markers t))

(setq org-image-actual-width nil)
(add-hook 'org-mode-hook 'visual-line-mode)

;; Disable native src block fontification to enforce palette
(setq org-src-fontify-natively nil)

(provide 'init-theming)
;;; init-theming.el ends here
