;;; init.el --- Steven's init file:
;;; Commentary:

;;; Code:
;;; -*- lexical-binding: t -*-
(require 'package)
(setopt packages-archives
	'(("gnu" . "https://elpa.gnu.org/packages/")
	  ("nongnu" . "https://elpa.nongnu.org/nongnu")
	  ("melpa" . "https://melpa.org/packages/")))

(when (< emacs-major-version 24)
  ;; Backwards compatibility
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'exec-path "/home/steven/.cargo/bin")

;; General styling
(setq inhibit-splash-screen nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(setq-default display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(auto-fill-mode t)

;; auto-saving
(setq auto-save-default 1)
(setq auto-save-interval 20)
(setq auto-save-visited-mode 1)
(setq auto-save-visited-interval 1)

;; store back up files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

;; Theme
(use-package ef-themes)
(use-package modus-themes)

;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)

;; Load the theme of your choice.
;;(load-theme 'ef-elea-dark :no-confirm)
(load-theme 'modus-vivendi :no-confirm)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
(setq default-frame-alist initial-frame-alist)
(set-frame-font "Aporetic Sans Mono 14" nil t)

;; Quality of life
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; rainbow-mode
(use-package rainbow-mode)

;; Ivy
(use-package ivy
  :diminish
  :bind (("C-M-s" . swiper)
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

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

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
(use-package sly)

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
  (setq-default lsp-ui-sideline-enable nil)
  :hook (csharp-mode . lsp)
  :hook (c-mode . lsp)
  :hook (c++-mode . lsp)
  :hook (go-mode . lsp)
  :hook (zig-mode . lsp)
  :config
  (setq-default lsp-enable-which-key-integration t)
  (add-hook 'rust-mode-hook 'lsp-deferred))
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
; java
(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)
(require 'lsp-java-boot)

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)
(add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

;
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
  :ensure t
  :hook ((text-mode
	  prog-mode
	  conf-mode
	  snippet-mode) . yas-minor-mode-on)
  :init
  (setq yas-snippet-dirs "~/.emacs.d/snippets/"))

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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

(unless (package-installed-p 'inf-cljure)
  (package-refresh-contents)
  (package-install 'inf-clojure))

(add-hook 'clojure-mode-hook #'inf-clojure-minor-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(provide 'init)
;;; init.el ends here

