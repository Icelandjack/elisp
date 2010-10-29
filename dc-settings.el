;; Set the cursor type
(setq blink-cursor-alist '((bar . 4)))
(setq cursor-in-non-selected-windows nil)
(setq blink-cursor-interval 0.25)
(blink-cursor-mode 1)
(set-cursor-color 'cyan)

;; (setq default-cursor-type 'bar)

;; Org-mode settings
(setf org-hide-leading-stars t)
(setf org-replace-disputed-keys t)

;; Indent levels
(setq cperl-indent-level 2
      cperl-close-paren-offset -2
      cperl-continued-statement-offset 0
      cperl-indent-parens-as-block t
      cperl-tab-always-indent t)

;; Color the fonts
(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode 1))

;; Tab preferences
;; begin
(setq sql-indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; end

;; Show matching parens
(show-paren-mode t)

;; Unclutter window
;; begin
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; end

;; Fix broken Page Up and Page Down keys
;; begin
(defun sfp-page-down ()
  (interactive)
  (setq this-command 'next-line)
  (next-line
   (- (window-text-height)
      next-screen-context-lines)))

(defun sfp-page-up ()
  (interactive)
  (setq this-command 'previous-line)
  (previous-line
   (- (window-text-height)
      next-screen-context-lines)))

(global-set-key [next] 'sfp-page-down)
(global-set-key [prior] 'sfp-page-up)
;; end

;; Better way to resize windows
;; begin
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
;; end

;; Filling
;; begin
;; (global-set-key (kbd "C-c q") 'auto-fill-mode)
;; (add-hook 'text-mode-hook (lambda() (set-fill-column 75)))
;; (require 'filladapt)
;; (add-hook 'text-mode-hook 'auto-fill-mode)
;; (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
;;;; (global-set-key (kbd "M-q") 'fill-paragraph)
;; end

;; Show column numbers when editing perl or text
;; begin
(add-hook 'cperl-mode-hook (lambda() (column-number-mode 1)))
(add-hook 'text-mode-hook (lambda() (column-number-mode 1)))
;; end

;; Set the window title to the file we're working on
(setq frame-title-format "%b - Emacs")

;; Spelling
;; begin
;; (setq ispell-program-name "/usr/bin/aspell")
;; (require 'ispell)
;; end

;; Swapping text
;; begin
(global-set-key (kbd "C-x t") 'anchored-transpose)
(autoload 'anchored-transpose "anchored-transpose" nil t)
;;end

;; Goto line shortcut
;; (Don't need this anymore because emacs 23 has "M-g M-g")
;; (global-set-key (kbd "C-x C-g") 'goto-line)

;; Improve deletion
(global-set-key (kbd "C-c C-d") 'c-hungry-delete-forward)


;; WindMove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Cut and Paste
;; begin
;; (setq x-select-enable-clipboard t)
;; ;; (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
;; (delete-selection-mode 1)
;; end

;; These require bowker.el to be loaded
;; (global-set-key (kbd "<C-return>") 'query-ml-server)
;; (global-set-key (kbd "<C-S-return>") 'query-dbxml-with-region)

;; Wrap lines in partial width windows
(setq truncate-partial-width-windows nil)

;; Subversion interface
;;(require 'psvn)

;; Misc global-set-key
;; (global-set-key (kbd "C-S-s") 'search-forward)

;; These were put in by emacs at some point
;; (put 'erase-buffer 'disabled nil)
;; (setq font-lock-maximum-decoration t)  
;; (put 'downcase-region 'disabled nil)

