;; ;; Start the emacs server
;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(column-number-mode t)
;;  '(inhibit-startup-screen t)
;;  '(server-done-hook (quote delete-frame))
;;  '(server-kill-new-buffers t)
;;  '(server-raise-frame t)
;;  '(server-switch-hook (lambda nil (let ((server-buf (current-buffer))) 
;;                                     (bury-buffer) 
;;                                     (switch-to-buffer-other-frame server-buf))))
;;  '(show-paren-mode t))

(require 'cl)

;; Local Settings
(load "~/.local-settings.el")

;; Modify load-path
;; (setenv "PATH" (concat "/usr/local/bin" ":" (getenv "PATH")))
(setq load-path (cons "~/elisp" load-path))

;; Some code to help with my job at R.R. Bowker
;; (load "bowker.el")

;; Stuff like nxml, encryption, xquery, javascript, and so on
(load "dc-modes.el")

;; Utilities
(load "dc-utilities.el")

;; Macros
(load "dc-macros.el")

;; General settings
(load "dc-settings.el")

;; Host-dependent settings
(loop for file in dc-local-stuff-to-load
      do (load file))

;; Misc
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
;; (set-frame-size-according-to-resolution)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-auto-discard-away nil)
 '(erc-auto-set-away nil)
 '(erc-autoaway-idle-method (quote user))
 '(erc-autoaway-idle-seconds 600)
 '(erc-autoaway-message "I might be gone")
 '(erc-autoaway-mode nil)
 '(erc-autojoin-channels-alist (quote (("irc.socialtext.net" "#dev"))))
 '(erc-autojoin-delay 15)
 '(erc-autojoin-mode t)
 '(erc-away-nickname "dc\\away")
 '(erc-default-sound "/usr/lib/libreoffice/share/gallery/sounds/apert.wav")
 '(erc-email-userid "Donald.Cameron")
 '(erc-generate-log-file-name-function (quote dc-erc-log-file-name))
 '(erc-join-buffer (quote buffer))
 '(erc-log-channels-directory "~/Documents")
 '(erc-log-insert-log-on-open nil)
 '(erc-log-mode t)
 '(erc-log-write-after-insert t)
 '(erc-log-write-after-send t)
 '(erc-modules (quote (autoaway autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring sound stamp spelling track)))
 '(erc-nick "dc")
 '(erc-nick-uniquifier "-")
 '(erc-port 6666)
 '(erc-server "irc.socialtext.net")
 '(erc-sound-mode t)
 '(erc-sound-path (quote ("/usr/lib/libreoffice/share/gallery/sounds")))
 '(erc-system-name "aphrodite")
 '(erc-user-full-name "Donnie Cameron")
 '(inhibit-startup-screen t)
 '(jabber-account-list dc-jabber-accounts)
 '(jabber-alert-info-message-hooks (quote (jabber-info-display)))
 '(jabber-alert-message-hooks (quote (jabber-message-libnotify jabber-message-beep jabber-message-wave jabber-message-echo jabber-message-display jabber-message-scroll)))
 '(jabber-alert-message-wave dc-jabber-message-wave-file)
 '(jabber-alert-presence-hooks nil)
 '(jabber-auto-reconnect t)
 '(jabber-autoaway-verbose t)
 '(jabber-avatar-verbose nil)
 '(jabber-chat-buffer-format "*-jabber-person-%n-*")
 '(jabber-history-enabled t)
 '(jabber-mode-line-mode t)
 '(jabber-play-sound-file (quote dc-play-sound))
 '(jabber-roster-buffer "*-jabber-roster-*")
 '(jabber-roster-line-format " %c %-25n %u %-8s (%r)")
 '(jabber-roster-show-title nil)
 '(jabber-show-offline-contacts t)
 '(jabber-use-global-history t)
 '(jabber-vcard-avatars-retrieve nil)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(put 'narrow-to-region 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 96 :width normal :foundry "unknown" :family "Liberation Mono")))))
