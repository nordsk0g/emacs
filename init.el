(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; General styling
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(display-line-numbers-mode 1)

;; Evil
(require 'evil
	 :custom (setq evil-want-keybinding nil))
(evil-mode 1)
(require 'evil-collection
	 :after evil)

;; Magit
(require 'magit)
