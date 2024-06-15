;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; General styling
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(display-line-numbers-mode 1)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night :no-confirm))
;;(load-theme 'doom-tokyo-night :no-confirm)

;; Evil
(setq evil-want-keybinding nil)
;;(require 'evil)
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
;;(require 'evil-collection)
;;(evil-collection-init)

;; Magit
(use-package magit)

;; Org mode
(use-package org)

;; Consult
(use-package consult)

;; Neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; LSP
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (csharp-mode . lsp))
         ;; if you want which-key integration
         ;;(lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)
(use-package lsp-ui :commands lsp-ui-mode)

;; Which-Key
(use-package which-key
  :config
  (which-key-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key lsp-mode neotree consult magit evil-collection doom-themes))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
