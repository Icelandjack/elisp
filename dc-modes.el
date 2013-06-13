;; (load "~/elisp/nxhtml/autostart")

;; Reverse video
;; (x-handle-reverse-video (selected-frame) '((reverse . t)))

(defun big-screen nil
  (interactive)
  (set-frame-width (selected-frame) dc-big-screen-width)
  (set-frame-height (selected-frame) dc-big-screen-height))

;; For LiveScript mode
(require 'livescript-mode)
(add-to-list 'auto-mode-alist '("\\.ls" . livescript-mode))

;; Org-mode settings (these have to be set before org is ever loaded)
(setf org-hide-leading-stars t)
(setf org-replace-disputed-keys t)

;; For saving org-mode work-time clock history across Emacs sessions
;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)

;; For LiveScript
;; (require 'livescript-mode)
;; (add-to-list 'auto-mode-alist '("\\.ls" . livescript-mode))

;; For Javascript
;;(autoload 'js2-mode "js2-mode" nil t)
;;(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;; For YAML
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml" . yaml-mode))

;; ;; Predictive mode
;; (add-to-list 'load-path "~/elisp/predictive")
;; (autoload 'predictive-mode "predictive" "predictive" t)
;; (set-default 'predictive-auto-add-to-dict t)
;; ;; predictive-main-dict 'rpg-dictionary
;; (setq predictive-auto-learn t
;;       predictive-add-to-dict-ask nil
;;       predictive-use-auto-learn-cache nil
;;       predictive-which-dict t)

;; For returning to a window configuration
(winner-mode t)

;; Emacs IRC (erc)
(require 'erc)
(require 'tls)
(defun dc-erc-log-file-name (a b c d e) dc-erc-log)

;; Disabling auto-saves
(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " Sensitive"
  ;; The minor mode bindings.
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)	
        ;; disable auto-save
        (if auto-save-default
            (auto-save-mode -1)))
    ;; resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))


;; PHP
(c-set-offset 'arglist-intro 4)

;; Perl::Tidy
(defun perltidy ()
  "Run perltidy on the current region or function."
  (interactive)
  (let ((orig-point (point)))
    (unless mark-active (mark-defun))
    (shell-command-on-region (point) (mark) "perltidy -q" nil t)
    (goto-char orig-point)))
(global-set-key "\C-ct" 'perltidy)

;; Magit
(add-to-list 'load-path "~/elisp/magit")
(require 'magit)
(setq magit-git-executable "/usr/local/git/bin/git")

;; Multiterm
(require 'multi-term)
(setq multi-term-program "/bin/bash")

;; ;; Edit server for Chrome
(require 'edit-server)
(setq edit-server-new-frame t)
(setq edit-server-new-frame-alist 
      '((width . 80)
        (height . 40)))
(edit-server-start)

;; For JavaScript and associated REPL
;; (autoload 'js2-mode "js2-mode" nil t)
;; (require 'js2-mode)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (setq slime-js-swank-command "/usr/local/bin/swank-js")
;; (setq slime-js-swank-args '())
;; (global-set-key (kbd "C-c C-r") 'slime-js-reload)
;; (add-hook 'js2-mode-hook (lambda () (slime-js-minor-mode 1)))

;; Enable full-screen
(defun fullscreen (&optional f)
  (interactive)
  (if (functionp 'ns-toggle-fullscreen)
      (ns-toggle-fullscreen)
    (set-frame-parameter f 'fullscreen
                         (if (frame-parameter f 'fullscreen) nil 'fullboth))))
(global-set-key [f11] 'fullscreen)
(add-hook 'after-make-frame-functions 'fullscreen)

;; Zoom
(defun zoom-in () (interactive) (text-scale-increase 1))
(defun zoom-out () (interactive) (text-scale-increase -1))
(global-set-key [?\C--] 'zoom-out)
(global-set-key [?\C-=] 'zoom-in)

;; Cut and Paste
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Blinking red cursor (when viewed with inverse rendering)
;; (set-cursor-color 'cyan)
(blink-cursor-mode t)

;; Selection deleted when key pressed
(delete-selection-mode 1)

;; Zoom
(defun zoom-in () (interactive) (text-scale-increase 1))
(defun zoom-out () (interactive) (text-scale-increase -1))
(global-set-key [?\C--] 'zoom-out)
(global-set-key [?\C-=] 'zoom-in)

;; Cut and Paste
(global-set-key "\C-w" 'clipboard-kill-region)
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-y" 'clipboard-yank)

;; Blinking red cursor (when viewed with inverse rendering)
;; (set-cursor-color 'cyan)
(blink-cursor-mode t)

;; Selection deleted when key pressed
(delete-selection-mode 1)

;; Open *.t files in cperl-mode
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

;; Twittering mode
(autoload 'twittering-mode "twittering-mode" "" t) 
(autoload 'twittering-update-status-from-minibuffer "twittering-mode" "" t) 
 
;; (defun twit () 
;;   (interactive) 
;;   (twittering-update-status-from-minibuffer (buffer-substring (region-beginning) (region-end)))) 
   
;; Use cperl-mode instead of perl-mode
;; begin
(mapc
 (lambda (pair)
   (if (eq (cdr pair) 'perl-mode)
       (setcdr pair 'cperl-mode)))
 (append auto-mode-alist interpreter-mode-alist))
;; end

;; Git mode
;; (add-to-list 'load-path 
;;              (expand-file-name "/usr/share/doc/git-core/contrib/emacs"))
;; (require 'vc-git)
;; (when (featurep 'vc-git) (add-to-list 'vc-handled-backends 'git))
;; (require 'git)
;; (autoload 'git-blame-mode "git-blame"
;;   "Minor mode for incremental blame for Git." t)

;; nXML mode
;; begin
;; (load "nxml-mode-20041004/rng-auto.el")
;; (setq auto-mode-alist
;;       (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|rdf\\|xul\\)\\'" . nxml-mode)
;;             auto-mode-alist))

(defun format-xml ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward "><" nil t)
      (replace-match ">\n<" nil nil))
    (indent-region (point-min) (point-max) nil)))

(defun format-xml-region (beg end)
  "Indent XML in the specified region."
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (nxml-mode)
      (goto-char (point-min))
      (while (search-forward "><" nil t)
        (replace-match ">\n<" nil nil))
      (indent-region (point-min) (point-max) nil))))
;; end

;; XQuery mode
(require 'xquery-mode)
(setq auto-mode-alist
      (cons '("\\.\\(xqy\\|xquery\\)\\'" . xquery-mode)
            auto-mode-alist))

;; Format XQuery code
(defun badly-format-xquery-code (beg end)
  "Insert new-lines in some key places in the selected XQuery code"
  (interactive "*r")
  (save-restriction
    (narrow-to-region beg end)
    (execute-kbd-macro [?\M-x ?x ?q ?u ?e ?r ?y ?  ?m ?o ?d ?e return ?\M-< ?\M-% ?l ?e ?t ?  return ?\C-q ?\C-j ?  ?  backspace backspace ?l ?e ?t ?  backspace ?  return ?! ?\M-< ?\M-% ?i ?f ?( return ?\C-q ?\C-j ?i ?f ?( backspace ?  ?( return ?! ?\M-< ?\M-% ?i ?f ?( backspace ?  return ?\C-q ?\C-j ?i ?f ?  ?  backspace ?( backspace])))

;; ;; Javascript mode
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))
;; (autoload 'javascript-mode "javascript" nil t)

;; PHP mode
(require 'php-mode)

;; Apache mode
;; (autoload 'apache-mode "apache-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
;; (add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
;; (add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
;; (add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
;; (add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))
;; (add-to-list 'auto-mode-alist '("\\.vhost" . apache-mode))

;; Temmplate Toolkit files should be opened in HTML mode
(add-to-list 'auto-mode-alist '("\\.tt2" . html-mode))

;; For Haskell
(load "/usr/share/emacs24/site-lisp/haskell-mode/haskell-mode.elc")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

 ;; For edif
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))

;; For erc
 '(erc-auto-discard-away dc-erc-auto-discard-away)
 '(erc-auto-set-away dc-erc-auto-set-away)
 '(erc-autoaway-idle-method dc-erc-autoaway-idle-method)
 '(erc-autoaway-idle-seconds dc-erc-autoaway-idle-seconds)
 '(erc-autoaway-message dc-erc-autoaway-message)
 '(erc-autoaway-mode dc-erc-autoaway-mode)
 '(erc-autojoin-channels-alist dc-erc-autojoin-channels-alist)
 '(erc-autojoin-delay dc-erc-autojoin-delay)
 '(erc-autojoin-mode dc-erc-autojoin-mode)
 '(erc-away-nickname dc-erc-away-nickname)
 '(erc-default-sound dc-erc-default-sound)
 '(erc-email-userid dc-erc-email-userid)
 '(erc-join-buffer dc-erc-join-buffer)
 '(erc-modules dc-erc-modules)
 '(erc-nick dc-erc-nick)
 '(erc-nick-uniquifier dc-erc-nick-uniquifier)
 '(erc-port dc-erc-port)
 '(erc-server dc-erc-server)
 '(erc-sound-mode dc-erc-sound-mode)
 '(erc-sound-path dc-erc-sound-path)
 '(erc-system-name dc-erc-system-name)
 '(erc-user-full-name dc-erc-user-full-name)
 '(erc-password dc-erc-password)
 '(erc-prompt-for-password nil)

 ;; For Jabber
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

 ;; Misc
 '(column-number-mode t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

