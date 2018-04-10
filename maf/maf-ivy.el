(provide 'maf-ivy)

(use-package counsel
  :ensure t)

(use-package ivy
  :ensure t
  :demand t
  :diminish (ivy-mode . "")
  :bind (("C-'" . ivy-avy)
        ("C-s" . swiper)
        ("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-c g" . counsel-git)
        ("C-c j" . counsel-git-grep)
        ("C-c k" . counsel-ag)
        ("C-x f" . counsel-recentf))
  :init (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-initial-inputs-alist nil)
    (setq ivy-count-format "%d/%d "))

(use-package avy
  :ensure t
  :bind (("S-." . avy-goto-word-or-subword-1)
         ("S-," . avy-goto-char)))
