;; -*- scheme -*-
;; Guix System configuration with EXWM session
;; Based on installer-generated config

(use-modules (gnu)
             (nongnu packages linux)
             (nongnu system linux-initrd))
(use-service-modules cups desktop networking ssh xorg)

(operating-system
  (locale "en_US.utf8")
  (timezone "America/Chicago")
  (keyboard-layout (keyboard-layout "us"))
  (host-name "meep")

  (users (cons* (user-account
                  (name "morz")
                  (comment "Morz")
                  (group "users")
                  (home-directory "/home/morz")
                  (supplementary-groups '("wheel" "netdev" "audio" "video")))
                %base-user-accounts))

  (packages (append (list (specification->package "emacs")
                          (specification->package "emacs-exwm")
                          (specification->package "emacs-desktop-environment"))
                    %base-packages))

  (services
   (append (list (service xfce-desktop-service-type)
                 (service openssh-service-type)
                 (set-xorg-configuration
                  (xorg-configuration (keyboard-layout keyboard-layout)))
                 ;; EXWM session entry for display manager
                 (extra-special-file "/usr/share/xsessions/exwm.desktop"
                   (plain-file "exwm.desktop"
                     "[Desktop Entry]
Name=EXWM
Comment=Emacs X Window Manager
Exec=emacs --fullscreen
TryExec=emacs
Type=Application
DesktopNames=EXWM")))
           (modify-services %desktop-services
             (guix-service-type config =>
               (guix-configuration
                 (inherit config)
                 (substitute-urls
                   (cons* "https://substitutes.nonguix.org"
                          %default-substitute-urls))
                 (authorized-keys
                   (cons* (plain-file "nonguix.pub"
                            "(public-key (ecc (curve Ed25519) (q #C1FD53E5D4CE971933EC50C9F307AE2171A2D3B52C804642A7A35F84F3A4EA98#)))")
                          %default-authorized-guix-keys)))))))

  (kernel linux)
  (initrd microcode-initrd)
  (firmware (list linux-firmware))

  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (targets (list "/dev/sda"))
                (keyboard-layout keyboard-layout)))
  
  (swap-devices (list (swap-space
                        (target (uuid "92638750-49f1-42cb-ae4e-0f34ae146e48")))))

  (file-systems (cons* (file-system
                         (mount-point "/")
                         (device (uuid "4027a23a-d7cd-4564-a729-617cb9496444" 'ext4))
                         (type "ext4"))
                       %base-file-systems)))
