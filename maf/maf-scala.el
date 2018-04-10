(provide 'maf-scala)

(use-package scala-mode
  :ensure t
  :defer t
  :config
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)

  (defun maf-newline-and-indent-with-asterisk ()
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))

  (define-key scala-mode-map (kbd "RET")
    #'maf-newline-and-indent-with-asterisk))

(use-package sbt-mode
  :ensure t
  :defer t
  :bind (:map scala-mode-map
              ("C-c m b c" . sbt-command)
              ("C-c m b r" . sbt-run-previous-command))
  :config
  ;; Do not pop up SBT buffers automatically
  (setq sbt:display-command-buffer nil)

  (defun maf-scala-pop-to-sbt (new-frame)
        "Open SBT REPL for this project, optionally in a NEW-FRAME.
Select the SBT REPL for the current project in a new window. If
the REPL is not yet running, start it. With prefix arg, select
the REPL in a new frame instead."
        (interactive "P")
        ;; Start SBT when no running, taken from `sbt:command'
        (when (not (comint-check-proc (sbt:buffer-name)))
          (sbt:run-sbt))

        (let ((display-buffer-overriding-action
               (if new-frame '(display-buffer-pop-up-frame) nil)))
          (pop-to-buffer (sbt:buffer-name))))

  (with-eval-after-load 'scala-mode
    (bind-key "C-c m s" #'maf-scala-pop-to-sbt scala-mode-map)))

(use-package ensime
  :ensure t
  :pin melpa-stable
  :after scala-mode
  :bind (:map ensime-mode-map
              ("C-c m E" . ensime-reload)
              ;; Free M-n and M-p again
              ("M-n" . nil)
              ("M-p" . nil)
              ("<f5>" . ensime-sbt-do-compile)
              :map scala-mode-map ("C-c m e" . ensime))
  :config
  ;; ;; Enable Ensime for all Scala buffers.
  (add-hook 'scala-mode-hook #'ensime-mode))
