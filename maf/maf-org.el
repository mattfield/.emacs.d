(provide 'maf-org)

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :init
  (require 'ox-md nil t)
  (when (boundp 'org-export-backends)
      (custom-set-variables '(org-export-backends '(ascii beamer html latex md)))))

(use-package epresent
  :ensure t)

(use-package htmlize
  :ensure t)

(use-package ox-gfm
  :ensure t
  :init
  (when (boundp 'org-export-backends)
    (customize-set-variable 'org-export-backends
                            (cons 'gfm org-export-backends))))
