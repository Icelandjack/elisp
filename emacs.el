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
(load "dc-vindicia.el")

;; Macros
(load "dc-macros.el")

;; General settings
(load "dc-settings.el")

;; Misc
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(set-frame-size-according-to-resolution)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode t)
 '(inhibit-startup-screen t)
 '(show-paren-mode t))
 
