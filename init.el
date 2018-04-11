(setq debug-on-error t)
(setq debug-on-quit t)

(require 'cl)

(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
    (load custom-file))

(defvar my/did-refresh-packages nil
  "Flag for whether packages have been refreshed yet")

(defun install-pkgs (list)
  (dolist (pkg list)
    (progn
      (if (not (package-installed-p pkg))
	  (progn
	    (if (not my/did-refresh-packages)
		(progn (package-refresh-contents)
		       (setq my/did-refresh-packages t)))
	    (package-install pkg))))))

(install-pkgs '(use-package diminish))

(require 'use-package nil t)
(setq use-package-verbose nil)
(setq use-package-always-defer t)

(add-to-list 'load-path "~/.emacs.d/maf/")

(defmacro try-load (module)
  "Try to load the given module, logging an error if unable to load"
  `(condition-case ex
       (require ,module)
     ('error
      (message "Unable to load [%s] module: %s" ,module ex))))

(try-load 'maf-core)
(try-load 'maf-appearance)
(try-load 'maf-navigation)
(try-load 'maf-writing)
(try-load 'maf-ivy)
(try-load 'maf-git)
(try-load 'maf-org)
(try-load 'maf-scala)

(setq initial-scratch-message ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n;; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n;; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n")

(setq debug-on-error nil)
(setq debug-on-quit nil)
