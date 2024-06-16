;; Package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; General styling
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Theme
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night :no-confirm))
(set-frame-font "Berkeley Mono Trial 14" nil t)
;;(load-theme 'doom-tokyo-night :no-confirm)

;; Ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function '*ivy-format-function-line))

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
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (csharp-mode . lsp)
  :config
  (lsp-enable-which-key-integration t))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)
(use-package dap-netcore)

;; Which-Key
(use-package which-key
  :config
  (which-key-mode))

;; Projectile
(use-package projectile
  :ensure t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (projectile-mode +1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ivy-rich projectile dap-mode lsp-treemacs lsp-ui which-key lsp-mode neotree consult magit evil-collection doom-themes))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
