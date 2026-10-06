;;; init.el --- Steven's init file: -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:
(defun src/display-startup-time ()
  "Log start up time for Emacs."
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'src/display-startup-time)

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; Backwards compatibility
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'exec-path "~/.cargo/bin")

;; General styling
(setq inhibit-splash-screen nil)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq visible-bell 1)
(setq-default display-line-numbers-type 'relative)
'(global-display-line-numbers-mode t)
'(column-number-mode t)
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
(setq-default show-trailing-whitespace t)

;; auto-saving
(setq auto-save-default nil)
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
(use-package modus-themes
  :ensure t
  :demand t)

(use-package ef-themes
  :ensure t
  :demand t)

;; Add all your customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs nil)

;; Load the theme of your choice.
(load-theme 'modus-vivendi :no-confirm)

(define-key global-map (kbd "<f5>") #'modus-themes-toggle)
(setq default-frame-alist '((font . "Aporetic Sans Mono 14")))
;;(set-frame-font "Aporetic Sans Mono 14" nil t)

;; Quality of life
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key [remap list-buffers] 'ibuffer)

;; Window management
(global-set-key (kbd "M-o") 'other-window)
(windmove-default-keybindings)

;; Elemental movement
'(global-subword-mode t)
'(global-superword-mode t)

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

(use-package annotate)

;; Org-roam immediate insert
(defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
	(org-roam-capture-templates (list (append (car org-roam-capture-templates)
						  '(:immediate-finish t)))))
  (apply #'org-roam-node-insert args)))

;; Consult
(use-package consult)

					; PROGRAMMING

;; clang-format

(use-package clang-format
  :init
  (setq clang-format-fallback-style "gnu"))

;; Sly
(use-package sly
  :init
  (setq inferior-lisp-program "/usr/sbin/sbcl"))

;; Treesitter
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (c++ . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (c-sharp
	 . ("https://github.com/tree-sitter/tree-sitter-c-sharp"))
	(commonlisp
	 . ("https://github.com/tree-sitter-grammars/tree-sitter-commonlisp"))
	(bash
	 . ("https://github.com/tree-sitter/tree-sitter-bash"))
	(elisp
	 . ("https://github.com/tree-sitter/tree-sitter-elisp"))))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
	(c++-mode . c++-ts-mode)
	(csharp-mode . csharp-ts-mode)
	(rust-mode . rust-ts-mode)
	(bash-mode . bash-ts-mode)
	))

(setq-default lsp-clients-clangd-executable "/usr/bin/clangd")

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
  :hook (csharp-ts-mode . lsp)
  :hook (c-ts-mode . lsp)
  :hook (c++-ts-mode . lsp)
  :config
  (setq-default lsp-enable-which-key-integration t)
  (add-hook 'rust-ts-mode-hook 'lsp-deferred))

;; to enable the lenses
(add-hook 'lsp-mode-hook #'lsp-lens-mode)

(add-hook 'after-init-hook 'global-company-mode)

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; Compile hooks
(add-hook 'c-ts-mode-hook
	  (lambda ()
	    (setq-local compile-command
			(format "gcc -Og -o \"%s\" %s"
			(file-name-sans-extension buffer-file-name)
			(file-name-nondirectory buffer-file-name)))))
(add-hook 'c++-ts-mode-hook
	  (lambda ()
	    (setq-local compile-command
			(format "g++ -std=c++23 -fmodules-ts %s -o %s"
			(file-name-sans-extension buffer-file-name)
			(file-name-nondirectory buffer-file-name)))))

(add-hook 'csharp-ts-mode-hook
	  (lambda ()
	    (setq-local compile-command
			(format "dotnet run"))))

;; Rust
(add-hook 'rust-ts-mode-hook
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
  :ensure t
  :defer t
  :hook (LaTeX-mode . (lambda ()
			(push (list 'output-pdf "Okular")
			      TeX-view-program-selection))))
(setq-default TeX-auto-save t)
(setq-default TeX-parse-self t)
(setq-default TeX-master nil)
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.8))

(setq custom-file "~/.emacs.d/custom.el")

(provide 'init)
;;; init.el ends here

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
