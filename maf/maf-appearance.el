(provide 'maf-appearance)

;;(defvar maf/background 'light)
(defvar maf/background 'dark)

(setq use-dialog-box nil)

(if (eq maf/background 'dark)
    (progn
      (use-package color-theme-sanityinc-tomorrow
        :ensure t
        :disabled t
        :init
        (load-theme 'sanityinc-tomorrow-night t)
        ;; Just ever so slightly more bright foreground text, default is
        ;; "#c5c8c6". Makes it easier to see on a sunny day
        (set-face-foreground 'default "#e5e8e6")
        ;; darken newline whitespace marks and blend in to the background
        (require 'whitespace)
        (set-face-foreground 'whitespace-newline "#555856")
        (set-face-background 'whitespace-newline (face-attribute 'default :background)))
      (use-package tao-theme
        :ensure t
        :disabled t
        :init
        (load-theme 'tao-yin t)
        (require 'git-gutter)
        (require 'git-gutter-fringe)
        (set-face-attribute 'git-gutter:deleted nil :foreground "red")
        (set-face-attribute 'git-gutter-fr:deleted nil :foreground "red")
        (set-face-attribute 'git-gutter:modified nil :foreground "light blue")
        (set-face-attribute 'git-gutter-fr:modified nil :foreground "light blue")
        (set-face-attribute 'git-gutter:added nil :foreground "green")
        (set-face-attribute 'git-gutter-fr:added nil :foreground "green")
        (require 'semantic/decorate/mode)
        (require 'semantic/decorate/include)
        (set-face-attribute 'semantic-tag-boundary-face nil :overline "grey25")
        (set-face-attribute 'semantic-decoration-on-unknown-includes
                            nil :background "grey15"))
      (use-package nord-theme
        :ensure t
        :init
        ;; Make comments 15% brighter
        (setq nord-comment-brightness 15)
        (load-theme 'nord t)
        (require 'semantic/decorate/mode)
        (require 'semantic/decorate/include)
        (set-face-attribute 'semantic-tag-boundary-face nil :overline "grey25")
        (set-face-attribute 'semantic-decoration-on-unknown-includes
                            nil :background "grey15")))
  (use-package tao-theme
    :ensure t
    :disabled t
    :init
    (load-theme 'tao-yang t)
    (setq-default line-spacing 1)
    (require 'git-gutter)
    (require 'git-gutter-fringe)
    (set-face-attribute 'git-gutter:deleted nil :foreground "red")
    (set-face-attribute 'git-gutter-fr:deleted nil :foreground "red")
    (set-face-attribute 'git-gutter:modified nil :foreground "light blue")
    (set-face-attribute 'git-gutter-fr:modified nil :foreground "light blue")
    (set-face-attribute 'git-gutter:added nil :foreground "green")
    (set-face-attribute 'git-gutter-fr:added nil :foreground "green"))
  (use-package dakrone-light-theme
    :ensure t
    :init (load-theme 'dakrone-light t)))

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      hscroll-margin 5
      hscroll-step 5)

(use-package smart-mode-line
  :ensure t
  :init
  (setq sml/theme 'respectful))
  (sml/setup)
  :config
  (setq sml/shorten-directory t
	sml/shorten-modes t)

(defun maf/set-fringe-background ()
  "Set the fringe background to the same color as the regular background."
  (setq maf/fringe-background-color
	(face-background 'default))
  (custom-set-faces
   `(fringe ((t (:background ,maf/fringe-background-color))))))

(add-hook 'after-init-hook #'maf/set-fringe-background)

(setq-default indicate-buffer-boundaries nil ;; 'right
	      fringe-indicator-alist
	      (delq (assq 'continuation fringe-indicator-alist)
		    fringe-indicator-alist)
	      fringes-outside-margins t
	      ;; Keep cursors and highlights in current window only
	      cursor-in-non-selected-windows nil)

(when (fboundp 'fringe-mode)
  (fringe-mode 4))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
		      :foreground 'unspecified
		      :inherit 'error))

(use-package visible-mark
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'visible-mark-mode)
  :config
  (setq visible-mark-max 1)
  (setq visible-mark-faces '(visible-mark-active))
  (if (eq maf/background 'dark)
      (set-face-attribute 'visible-mark-active nil :background "#444444")
    (set-face-attribute 'visible-mark-active nil :background "#DDDDDD"))
  (set-face-attribute 'visible-mark-active nil :foreground nil))

(defun maf/setup-fonts ()
  (when (eq window-system 'x)
    (set-face-attribute 'default nil
                        :family "Dina")
    (dolist (face '(mode-line mode-line-inactive minibuffer-prompt))
      (set-face-attribute face nil :family "Dina"))
    (set-face-attribute 'variable-pitch nil
                        :family "Dina")))

(when (eq window-system 'x)
  (add-hook 'after-init-hook #'maf/setup-fonts))
