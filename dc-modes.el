;; (load "~/elisp/nxhtml/autostart")

;; For LiveScript mode
(require 'livescript-mode)
(add-to-list 'auto-mode-alist '("\\.ls" . livescript-mode))

;; Org-mode settings (these have to be set before org is ever loaded)
(setf org-hide-leading-stars t)
(setf org-replace-disputed-keys t)

;; For saving org-mode work-time clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

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

;; Load slime
(setq inferior-lisp-program "/usr/bin/sbcl --dynamic-space-size 10000")
(add-to-list 'load-path "~/elisp/slime/")
(require 'slime-autoloads)
(slime-setup '(slime-fancy))

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

;; ;;
;; ;; BEGIN Haskell Setup
;; ;;

;; ;; Font Locking, Programming Modes, and Compilation settings
;; ;;

;; ;; maximum colors
;; (setq font-lock-maximum-decoration t)

;; ;; extra key bindings
;; (global-set-key "\M-C" 'compile)
;; (global-set-key "\C-^" 'next-error)
;; (global-set-key "\C-\M-g" 'goto-line)

;; ;; use spaces instead of tabs
;; (setq indent-tab-mode nil)

;; ;; haskell mode configuration
;; (setq auto-mode-alist
;;       (append auto-mode-alist
;;               '(("\\.[hg]s$"  . haskell-mode)
;;                 ("\\.hic?$"     . haskell-mode)
;;                 ("\\.hsc$"     . haskell-mode)
;;                 ("\\.chs$"    . haskell-mode)
;;                 ("\\.l[hg]s$" . literate-haskell-mode))))
;; (autoload 'haskell-mode "haskell-mode"
;;   "Major mode for editing Haskell scripts." t)
;; (autoload 'literate-haskell-mode "haskell-mode"
;;   "Major mode for editing literate Haskell scripts." t)

;; ;adding the following lines according to which modules you want to use:
;; (require 'inf-haskell)

;; (add-hook 'haskell-mode-hook 'turn-on-font-lock)
;; ;(add-hook 'haskell-mode-hook 'turn-off-haskell-decl-scan)
;; ;(add-hook 'haskell-mode-hook 'turn-off-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; ;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
;; ;(add-hook 'haskell-mode-hook 'turn-on-haskell-hugs)
;; ;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
;; ;(add-hook 'haskell-mode-hook 
;; ;   (function
;; ;    (lambda ()
;; ;      (setq haskell-program-name "ghci")
;; ;      (setq haskell-ghci-program-name "ghci6")
;; ;      (setq haskell-ghci-program-args 
;; ;         '("-fcontext-stack=30" 
;; ;           "-fglasgow-exts" 
;; ;           "-farrows")))))

;; ;;
;; ;; END Haskell Setup
;; ;;


