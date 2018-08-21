(provide 'maf-core)

(setq user-full-name
      (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                          "git config --get user.name")))

(setq user-mail-address
      (replace-regexp-in-string "\n$" "" (shell-command-to-string
                                          "git config --get user.email")))

(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(global-font-lock-mode t)

(setq message-log-max 16384)

(defvar maf/default-gc-threshold (* 20 1024 1024))

(setq garbage-collection-messages nil)

(setq gc-cons-threshold (* 100 1024 1024))

(add-hook 'after-init-hook
	  (lambda ()
	    (message "Resetting garbage collection..")
	    (setq gc-cons-threshold maf/default-gc-threshold)))

(setq idle-update-delay 2)

(setq line-number-display-limit-width 10000)

(setq gnutls-min-prime-bits 4096)

(delete-selection-mode 1)

(setq large-file-warning-threshold (* 25 1024 1024))

(setq-default indicate-empty-lines nil)
(setq-default indicate-buffer-boundaries nil)

(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode -1))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq ring-bell-function (lambda ()))
(setq inhibit-startup-screen t)

(setq read-file-name-completion-ignore-case t)

(defalias 'yes-or-no-p 'y-or-n-p)

(when (window-system)
  (setq confirm-kill-emacs 'yes-or-no-p))

(setq line-move-visual t)

(setq make-pointer-invisible t)

(setq-default fill-column 80)
(setq-default default-tab-width 2)
(setq-default indent-tabs-mode nil)

(setq system-uses-terminfo nil)

(setq-default find-file-visit-truename t)

(setq require-final-newline t)

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(global-set-key (kbd "C-x k") #'kill-this-buffer)

(setq sentence-end-double-space nil)

(random t)

(setq diff-switches "-u")

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package diminish
  :init (diminish 'auto-fill-function ""))

(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
	(bury-buffer)
      ad-do-it)))

(when (boundp 'global-prettify-symbols-mode)
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (push '("lambda" . ?λ) prettify-symbols-alist)))
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (push '("fn" . ?ƒ) prettify-symbols-alist)))
  (global-prettify-symbols-mode +1))

(defun save-persistent-scratch ()
    "Write the contents of *scratch* to the file name
`persistent-scratch-file-name'."
    (with-current-buffer (get-buffer-create "*scratch*")
      (write-region (point-min) (point-max) "~/.emacs.d/persistent-scratch")))

(defun load-persistent-scratch ()
    "Load the contents of `persistent-scratch-file-name' into the
  scratch buffer, clearing its contents first."
    (if (file-exists-p "~/.emacs-persistent-scratch")
	(with-current-buffer (get-buffer "*scratch*")
	  (delete-region (point-min) (point-max))
	  (insert-file-contents "~/.emacs.d/persistent-scratch"))))

(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)

(save-place-mode 1)

(setq x-select-enable-clipboard t)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(add-hook 'after-init-hook #'winner-mode)

(setq save-interprogram-paste-before-kill t)

;; delete-auto-save-files
(setq delete-auto-save-files t)
;; Create the directory for backups if it doesn't exist
(when (not (file-exists-p "~/.emacs_backups"))
  (make-directory "~/.emacs_backups"))

(setq-default backup-directory-alist
	      '((".*" . "~/.emacs_backups")))
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs_backups/" t)))

;; delete old backups silently
(setq delete-old-versions t)

(use-package ws-butler
  :ensure t
  :diminish ws-butler-mode
  :init
  (add-hook 'prog-mode-hook #'ws-butler-mode)
  (add-hook 'org-mode-hook #'ws-butler-mode)
  (add-hook 'text-mode-hook #'ws-butler-mode))

(use-package auto-indent-mode
  :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode t)
  :defer t
  :diminish ""
  :config
  (progn
    (define-key undo-tree-map (kbd "C-x u") 'undo-tree-visualize)
    (define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)))

(use-package shrink-whitespace
  :ensure t
  :bind ("M-SPC" . shrink-whitespace))

(use-package expand-region
  :ensure t
  :defer t
  :bind (("C-\\" . er/expand-region)
         ("C-M-@" . er/contract-region)))

(setq kill-do-not-save-duplicates t)

(setq switch-to-buffer-preserve-window-point t)

(setq reb-re-syntax 'string)

(setq completion-ignore-case t
      read-file-name-completion-ignore-case t)

(setq auto-revert-interval 10)

;; Mostly taken from
;; http://blog.binchen.org/posts/what-s-the-best-spell-check-set-up-in-emacs.html
(when (executable-find "aspell")
  (setq ispell-program-name (executable-find "aspell"))
  (setq ispell-extra-args
        (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
              "--lang=en_GB"
              "--ignore=4")))

;; hunspell
(when (executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-extra-args '("-d en_GB")))

(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
(add-to-list 'ispell-skip-region-alist '(":\\(PROPERTIES\\|LOGBOOK\\):" . ":END:"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_SRC" . "#\\+END_SRC"))
(add-to-list 'ispell-skip-region-alist '("#\\+BEGIN_EXAMPLE" . "#\\+END_EXAMPLE"))
