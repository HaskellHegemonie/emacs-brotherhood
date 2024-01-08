(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(defun my-configure-init-frame (frame)
	(progn
				(keyboard-translate ?\C-t ?\C-x)
				(keyboard-translate ?\C-x ?\C-t)
				(set-frame-font "JetBrainsMonoNerdFont 11" nil nil)
				))


(add-hook 'after-make-frame-functions #'my-configure-init-frame)
(setq make-backup-files nil)
(require 'use-package)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-g-bindings nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-h-delete t)
	:bind (
				 ;; ("C-w C-v" . #!evil-window-vsplit)
				 ;; ("C-w C-s" . #'evil-window-split)
				 ;; ("C-w C-l" . #'evil-window-right)
				 ;; ("C-w C-h" . #'evil-window-left)
				 ;; ("C-w C-j" . #'evil-window-down)
	)
  :config
  (evil-mode 1)
	(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
	(define-key evil-normal-state-map (kbd "C-p") 'previous-line)
	(define-key evil-normal-state-map (kbd "C-n") 'next-line)
	(define-key evil-normal-state-map (kbd "C-y") 'yank)
	(define-key evil-normal-state-map (kbd "M-y") 'yank-pop)
	(define-key evil-normal-state-map (kbd "C-f") 'forward-char)
	(define-key evil-normal-state-map (kbd "C-b") 'backward-char)

	(define-key evil-normal-state-map (kbd "M-n") 'evil-scroll-down)
	(define-key evil-normal-state-map (kbd "M-p") 'evil-scroll-up)

	(define-key evil-insert-state-map (kbd "M-n") 'evil-scroll-down)
	(define-key evil-insert-state-map (kbd "M-p") 'evil-scroll-up)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
	(define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
	(define-key evil-visual-state-map (kbd "C-f") 'forward-char)
	(define-key evil-visual-state-map (kbd "C-b") 'backward-char)
	(define-key evil-visual-state-map (kbd "C-y") 'yank)
	(define-key evil-visual-state-map (kbd "M-y") 'yank-pop)
  (setq evil-insert-state-cursor 'box))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :config
  (setq orderless-matching-styles '(orderless-regexp))
  (setq completion-styles '(orderless basic)))

(use-package org
  :config
  (setq org-directory "~/orgRoam/agenda")
  (setq org-agenda-files `("~/orgRoam/agenda/schedule.org"))
  (setq org-agenda-span 14)
  :bind
  (("C-c o a" . 'org-agenda)
  ("C-c o s" . 'org-schedule)
  ("C-c o t" . 'org-todo)
  ("C-c o r" . 'org-refile)
  ("C-c o d" . 'org-deadline)
  ("C-c o ;" . 'org-timer-set-timer)
  ("C-c o !" . #'org-time-stamp-inactive)
  ("C-c o ," . 'org-timer-pause-or-continue)
  ("C-c o _" . 'org-timer-stop)
  ("C-c o 0" . 'org-timer-start)
  ("C-c o ." . 'org-time-stamp)
  ("C-c o c" . #'org-capture)
  ("C-c o x" . #'org-toggle-checkbox)))

(use-package org-roam
  :custom
  (org-roam-directory "~/orgRoam")
  :bind
  (("C-c o l" . #'org-roam-buffer-toggle)
   ("C-c o f" . #'org-roam-node-find)
   ("C-c o i" . #'org-roam-node-insert)
   ("C-c o n" . #'org-roam-dailies-capture-date)
   ("C-c o N" . #'org-roam-dailies-goto-date)
   )
  :config
  (org-roam-setup)
  (setq org-roam-dailies-directory "journal/"))

(use-package magit
  :bind
  (
		("C-c C-g" . magit-dispatch)
		("C-c g g" . 'magit)
		("C-c g c" . 'magit-clone)
		("C-c g i" . 'magit-init))
  :config
  (setq transient-default-level 7))

;; (use-package doom-modeline
;;   :init
;;   (doom-modeline-mode 0))

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq tab-with 2)
(setq indent-line-function 'noindent)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; (setq lsp-clients-clangd-args "")
  )

(use-package dap-mode)

(use-package haskell-mode
  :config
  (setq haskell-interactive-popup-errors nil))

(use-package kotlin-mode)

(use-package nix-mode)
(use-package rust-mode)
(use-package scala-mode)

(use-package projectile
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
   (setq projectile-project-search-path '("~/code" "~/nixos" "~/org")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package consult
  :bind
  ("C-c c l" . #'consult-line)
  ("C-c c f" . #'consult-find)
  ("C-c c r" . #'consult-ripgrep)
  ("C-c c b" . #'consult-buffer)
  ("C-c c h" . #'consult-org-heading)
  ("C-c c a" . #'consult-org-agenda))

(use-package pdf-tools
  :commands (pdf-tools-install)
	:mode "\\.pdf\\'"
	:bind (:map pdf-view-mode-map
							("j" . pdf-view-next-line-or-next-page)
							("k" . pdf-view-previous-line-or-previous-page)
							("C-+" . pdf-view-enlarge)
							("C--" . pdf-view-shrink))
	:init (pdf-loader-install)
	:config (add-to-list 'revert-without-query ".pdf")
	)
(add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))

(use-package all-the-icons)


(setq display-line-numbers-type t)
(setq blink-cursor-mode nil)
(setq truncate-lines 1)
(setq standard-indent 2)
(setq tab-width 2)
(setq c-tab-always-indent nil)
(setq c-syntactic-indentation nil)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq scroll-margin 8)
(setq display-line-numbers 'relative)
(setq display-line-numbers-type 'relative)
(setq eletric-indent-mode nil)
(global-display-line-numbers-mode)

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-type-request '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defvar evil-window-maps (make-sparse-keymap))

(global-set-key (kbd "C-w") evil-window-maps)
(define-key evil-window-maps (kbd "C-v") 'evil-window-vsplit)
(define-key evil-window-maps (kbd "C-s") 'evil-window-split)
(define-key evil-window-maps (kbd "C-l") 'evil-window-right)
(define-key evil-window-maps (kbd "C-h") 'evil-window-left)
(define-key evil-window-maps (kbd "C-j") 'evil-window-down)
(define-key evil-window-maps (kbd "C-k") 'evil-window-up)
(define-key evil-window-maps (kbd "C-q") 'delete-window)
(define-key evil-window-maps (kbd "C-w") 'kill-region)
(global-set-key (kbd "M-p") 'evil-scroll-up)
(global-set-key (kbd "M-n") 'evil-scroll-down)
(global-set-key (kbd "C-^") 'evil-buffer)
(define-minor-mode evil-window-mode
	nil
	"Ewin"
	evil-window-maps)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
