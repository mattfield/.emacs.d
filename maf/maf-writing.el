(provide 'maf-writing)

(use-package markdown-mode
  :ensure t
  :defer t
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("github\\.com.*\\.txt\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-enable-wiki-links t
        markdown-italic-underscore t
        markdown-make-gfm-checkboxes-buttons t
        markdown-gfm-additional-languages '("sh")))

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package adoc-mode
  :ensure t
  :defer t)
