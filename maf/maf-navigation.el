(provide 'maf-navigation)

;; Font size
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

;; Start or switch to eshell
(global-set-key (kbd "C-x C-m") 'eshell)

(use-package engine-mode
  :ensure t
  :defer 10
  :init (engine-mode 1)
  :config
  ;; Use an external browser for these
  (setq engine/browser-function 'browse-url-default-browser)

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine elasticsearch
    "https://github.com/elastic/elasticsearch/search?q=%s&type="
    :keybinding "e")

  (defengine x-pack
    "https://github.com/elastic/x-pack-elasticsearch/search?q=%s&type="
    :keybinding "x")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")

  (defengine cloud
    "https://github.com/elastic/cloud/search?q=%s&type="
    :keybinding "c"))
