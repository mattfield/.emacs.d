(provide 'maf-es)

(use-package es-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.es$" . es-mode))
  (add-hook 'es-mode-hook 'auto-indent-mode)
  (add-hook 'es-result-mode-hook 'hs-minor-mode)
  :config
  (setq es-warn-on-delete-query nil
        es-always-pretty-print t))
