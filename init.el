; Package manager
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'exec-path "/home/steven/.cargo/bin")

;; General styling
(setq inhibit-splash-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; Scratch message
(setq initial-scratch-message "; Docs: C-h f [function] C-h v [variable] C-h k [keybinding] C-h m [mode] M-x ielm [REPL]")

;; Theme
(use-package ef-themes)
(use-package modus-themes)

;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)

;; Load the theme of your choice.
(load-theme 'ef-elea-dark :no-confirm)
;;(load-theme 'modus-vivendi :no-confirm)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
;;(use-package doom-themes
;;  :config
;;  (load-theme 'doom-tokyo-night :no-confirm))
(setq initial-frame-alist
      '((top . 50)
	(left . 50)
	(width . 120)
	(height . 45)
	(font . "Iosevka Comfy Medium-14")))
(setq default-frame-alist initial-frame-alist)
;;(add-to-list 'default-frame-alist '(font . "Iosevka Comfy Medium-14"))
;;(set-frame-font "Iosevka Comfy Medium" nil t)
;;(set-frame-font "TX-02 SemiLight 14" nil t)
;;(load-theme 'doom-tokyo-night :no-confirm)

;; Quality of life
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; vterm
(use-package vterm
  :ensure t
  :hook (vterm-mode . my/disable-evil-in-vterm))

(defun my/disable-evil-in-vterm ()
  (evil-local-mode -1))

;(add-hook 'vterm-mode-hook (lambda () (setq evil-default-state 'emacs)))
;(add-hook 'vterm-mode-hook #'evil-emacs-state)
;(evil-set-initial-state 'vterm-mode 'emacs)

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
;;(use-package ivy-rich
;;  :init
;;  (ivy-rich-mode 1)
;;  :config
;;  (setq ivy-format-function '*ivy-format-function-line))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; Disable evil in vterm
(with-eval-after-load 'evil-collection
  (setq evil-collection-mode-list
        (remove 'vterm evil-collection-mode-list)))
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
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(setq org-agenda-files (list "~/org/work.org"
			     "~/org/personal.org"))

;; Org-roam
(use-package org-roam
  :custom
  (org-roam-directory "~/org-roam")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 ("C-c n I" . org-roam-node-insert-immediate)
	 :map org-mode-map
	 ("C-M-i"   . completion-at-point)))

;; Org-roam immediate insert
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
  (apply #'org-roam-node-insert args)))

;; Consult
(use-package consult)

;; Treemacs
(use-package treemacs
  :ensure t
  :bind
  (:map global-map
	([f8] . treemacs)
	("C-<f8>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t)
  (setq treemacs-follow-mode t)
  (setq treemacs-project-follow-mode t))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

;; Sly
;; (defun my/disable-evil-in-sly ()
;;   "Disable evil mode in SLY buffers."
;;   (evil-local-mode -1))
(use-package sly)
;; (add-hook 'sly-mode-hook #'my/disable-evil-in-sly)
;; (add-hook 'sly-mrepl-mode-hook #'my/disable-evil-in-sly)

;; LSP
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-inlay-hint-enable t)
  ;(setq lsp-rust-analyzer-server-path "~/.local/bin/rust-analyzer/")
  :hook (csharp-mode . lsp)
  :hook (c-mode . lsp)
  :hook (go-mode . lsp)
  :hook (zig-mode . lsp)
  :config
  (lsp-enable-which-key-integration t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (add-hook 'rust-mode-hook 'lsp-deferred))
(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)
(use-package dap-netcore)
(add-hook 'after-init-hook 'global-company-mode)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; Rust
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5)
  :bind
  (:map company-active-map
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("M-<" . company-select-first)
	("M->" . company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


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

;; Flycheck
(use-package flycheck :ensure)
(global-flycheck-mode)

;; AUCTeX
(use-package auctex
  :ensure t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;'(org-format-latex-options
;   '(:foreground default :background default :scale 5.0 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers
; 		 ("begin" "$1" "$" "$$" "\\(" "\\[")))
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex company consult counsel dap-mode doom-themes ef-themes
	    evil-collection evil-mu4e flycheck flycheck-rust go-mode
	    ivy-rich lsp-mode lsp-treemacs lsp-ui magit modus-themes
	    org-roam org-roam-ui projectile rust-mode sly
	    treemacs-evil treemacs-icons-dired treemacs-magit
	    treemacs-projectile vterm which-key yasnippet zig-mode
	    zone-select))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here

