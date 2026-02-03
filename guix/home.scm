;; -*- scheme -*-
;; Guix Home configuration
;; Reconfigure with: guix home reconfigure ~/dotfiles/guix/home.scm

(use-modules (gnu home)
             (gnu home services)
             (gnu home services shells)
             (gnu home services guix)
             (gnu home services shepherd)
             (gnu packages)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (gnu packages fonts)
             (gnu packages version-control)
             (gnu packages xorg)
             (gnu packages xdisorg)
             (gnu packages ncurses)
             (gnu packages admin)
             (gnu packages bash)
             (guix gexp)
             (guix channels)
             (guix profiles))

(define %my-channels
  (cons* (channel
           (name 'nonguix)
           (url "https://gitlab.com/nonguix/nonguix")
           (introduction
             (make-channel-introduction
               "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
               (openpgp-fingerprint
                 "2A39 3FFF 68F4 EF7A 3D29 12AF 6F51 20A0 22FB B2D5"))))
         %default-channels))

(home-environment
  (packages
    (list
      ;; Core
      emacs
      emacs-exwm
      emacs-desktop-environment  ; brightness/volume keys in EXWM
      emacs-use-package
      emacs-which-key
      
      ;; Fallback fonts (put your Terza .otf in ~/.local/share/fonts/)
      font-dejavu
      font-google-noto
      
      ;; Tools
      git
      maim
      xdotool
      xset
      ncurses
      neofetch
      copyq
      bash-minimal))

  (services
   (list
    (service home-channels-service-type %my-channels)
    (service home-shepherd-service-type
             (home-shepherd-configuration
              (services
               (list
                (shepherd-service
                 (provision '(copyq))
                 (documentation "CopyQ clipboard manager (runs as a long-lived GUI process).")
                 (start
                  #~(make-forkexec-constructor
                     (list #$(file-append bash-minimal "/bin/bash") "-lc"
                           (string-append
                            ;; Wait for X11 socket (EXWM/X11).
                            "while [ ! -S /tmp/.X11-unix/X0 ]; do sleep 0.2; done; "
                            "export DISPLAY=${DISPLAY:-:0}; "
                            ;; Run CopyQ in the foreground so Shepherd can supervise it.
                            "exec " #$(file-append copyq "/bin/copyq")))
                     #:log-file (string-append (getenv "HOME") "/.local/state/shepherd/copyq.log")))
                 (stop #~(make-kill-destructor)))))))

    (service home-bash-service-type
             (home-bash-configuration
              (guix-defaults? #t)
              (bash-profile
               (list (plain-file "bash-profile-extra" "
export _JAVA_AWT_WM_NONREPARENTING=1
")))))

    ;; Symlink Emacs config from dotfiles
    (simple-service 'emacs-config
                    home-xdg-configuration-files-service-type
                    `(("emacs/init.el"
                       ,(local-file "../emacs/init.el"))
                      ("emacs/early-init.el"
                       ,(local-file "../emacs/early-init.el"))
                      ("emacs/lisp/init-exwm.el"
                       ,(local-file "../emacs/lisp/init-exwm.el"))
                      ("emacs/lisp/init-theming.el"
                       ,(local-file "../emacs/lisp/init-theming.el"))))

    (simple-service 'extra-env-vars
                    home-environment-variables-service-type
                    '(("EDITOR" . "emacs")
                      ("_JAVA_AWT_WM_NONREPARENTING" . "1"))))))
