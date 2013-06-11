(defvar dc-reverse-video t)

(setq dc-sounds
      (list :thunder "/home/dcameron/Music/wav-files/thunder2.wav"
            :apert "/usr/lib/libreoffice/share/gallery/sounds/apert.wav"
            :none nil))
(setq dc-erc-sound-file (getf dc-sounds :thunder))
(defvar dc-erc-sound-match "\\(\\(^\\|[^a-z]\\)dc\\([^a-z]\\|$\\)\\)\\|donnie")

(defvar dc-erc-mute nil)
(defvar dc-erc-bot-enabled t)
(defvar dc-erc-repl-enabled t)
(defvar dc-erc-bot-name "vixen")
(defvar dc-erc-log "/home/dcameron/Dropbox/socialtext/erc/aprodite-erc.log")
(defvar dc-erc-auto-discard-away nil)
(defvar dc-erc-auto-set-away nil)
(defvar dc-erc-autoaway-idle-method (quote user))
(defvar dc-erc-autoaway-idle-seconds 600)
(defvar dc-erc-autoaway-message "I might be gone")
(defvar dc-erc-autoaway-mode nil)
(defvar dc-erc-autojoin-channels-alist (quote (("irc.socialtext.net" "#dev"))))
(defvar dc-erc-autojoin-delay 15)
(defvar dc-erc-autojoin-mode t)
(defvar dc-erc-away-nickname "dc\\away")
(defvar dc-erc-default-sound "/usr/lib/libreoffice/share/gallery/sounds/apert.wav")
(defvar dc-erc-email-userid "Donald.Cameron")
(defvar dc-erc-join-buffer (quote buffer))
(defvar dc-erc-modules (quote (autoaway autojoin button
                                        completion fill
                                        irccontrols list match
                                        menu move-to-prompt
                                        netsplit networks
                                        noncommands readonly ring
                                        sound stamp spelling
                                        track)))
(defvar dc-erc-nick "dc")
(defvar dc-erc-nick-uniquifier "-")
(defvar dc-erc-port 1111)
(defvar dc-erc-server "irc.somewhere.net")
(defvar dc-erc-password "password")
(defvar dc-erc-sound-mode t)
(defvar dc-erc-sound-path (quote ("/usr/lib/libreoffice/share/gallery/sounds")))
(defvar dc-erc-system-name "angel")
(defvar dc-erc-user-full-name "John Doe")


(setq dc-local-stuff-to-load '("dc-socialtext.el"))

(setq twittering-username "username")
(setq twittering-password "password")
(setq this-is-aquamacs nil)
(setq google-talk-password "password")
(setq dc-jabber-accounts
      `(("user@gmail.com"
         (:password . ,google-talk-password)
         (:network-server . "talk.google.com")
         (:connection-type . ssl))))
(setq dc-sound-program "/usr/bin/aplay")
(setq dc-jabber-message-wave-file "/path/to/a/wav-file")

(setq chattermancy-username "username")
(setq chattermancy-password "password")
(setq chattermancy-host "hostname")
(setq chattermancy-port "1111")
(setq chattermancy-protocol "http")
(defvar dc-file-prefix "/ssh:borg.sinistercode.com:/home/user/folder")

;; (setenv "PATH" "/home/macnod/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/lib/oracle/11.2/client64/bin")
(setenv "VISUAL" "/usr/bin/emacs")

;; (setq inferior-lisp-program "/usr/bin/sbcl")
;;(add-to-list 'load-path "/home/macnod/elisp/slime")
;;(require 'slime-autoloads)
;;(slime-setup '(slime-fancy))
(when window-system
  (setq default-frame-alist 
        '((background-color . "white")
          (width . 166)
          (height . 43)))

  ;; Default font
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is mo re than one, they won't work right.
   '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 75 :width normal :foundry "unknown" :family "Liberation Mono"))))))

