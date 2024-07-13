(require 'use-package)

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

(use-package rainbow-delimiters
  :init
  (rainbow-delimiters-mode)
  :config
  (rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package all-the-icons)

(use-package doom-themes
  :init
  (load-theme 'doom-one t))

(use-package emacs
  :init
  (global-display-line-numbers-mode)

  :bind
  (
   ("C-x C-h" . #'switch-to-buffer)
   ("C-c C-<return>" . #'mode-line-other-buffer)
   ("C-c C-d" . #'evil-scroll-down)
   ("C-c C-u" . #'evil-scroll-up)
   ("C-M-e" . #'eshell) 
   )

  :custom
  (make-backup-files nil)
  (tab-with 2)
  (indent-line-function 'noindent)
  (display-line-numbers-type t)
  (blink-cursor-mode nil)
  (truncate-lines 1)
  (standard-indent 2)
  (tab-width 2)
  (c-tab-always-indent nil)
  (c-syntactic-indentation nil)
  (indent-tabs-mode t)
  (tab-width 2)
  (scroll-margin 8)
  (display-line-numbers 'relative)
  (display-line-numbers-type 'relative)
  (eletric-indent-mode nil)
  )

(use-package keychain-environment
  :config
  (keychain-refresh-environment))

(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-type-request '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package evil
  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-g-bindings nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-C-h-delete nil)
  :init
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'eshell 'emacs)

  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'shell 'emacs)

  (evil-set-initial-state 'Man-mode 'emacs)
  ;; :bind (
  ;; 			 )
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
  (setq evil-insert-state-cursor 'box)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

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

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(use-package vertico
  :config
  (vertico-mode 1))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-regexp))
  (completion-styles '(orderless basic)))

(use-package consult
  :bind
  ("C-c c l" . #'consult-line)
  ("C-c c f" . #'consult-find)
  ("C-c c r" . #'consult-ripgrep)
  ("C-c c b" . #'consult-buffer)
  ("C-c c h" . #'consult-org-heading)
  ("C-c c a" . #'consult-org-agenda))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback)
  (epa-armor t))



(use-package eshell
  :custom
  (eshell-aliases-file "/etc/nixos/config/emacs/eshell-aliases")
  )

(defun hsheg/tangle-save-in-org ()
  (when
      (string= (file-name-extension (buffer-file-name)) "org")
    (org-babel-tangle)
    )
  )

(require 'org)
(use-package org
  :custom
  (org-directory "~/orgRoam/agenda")
  (org-agenda-span 14)
  (org-agenda-files nil) ;; can also set with =C-c [= per project
  (org-confirm-babel-evaluate nil)

  ;; Original value was
  ;; (("a" . "export ascii")
  ;;  ("c" . "center")
  ;;  ("C" . "comment")
  ;;  ("e" . "example")
  ;;  ("E" . "export")
  ;;  ("h" . "export html")
  ;;  ("l" . "export latex")
  ;;  ("q" . "quote")
  ;;  ("s" . "src")
  ;;  ("v" . "verse"))
  (org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "src elisp")
     ("E" . "export")
     ("h" . "src haskell")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))
   )
  :config
  (add-hook 'after-save-hook 'hsheg/tangle-save-in-org)
  :bind
  (("C-c o w" . #'org-store-link)
   ("C-c o y" . #'org-insert-link)
   ("C-c o >" . #'org-goto-calendar)
   ("C-c o <" . #'org-date-from-calendar)
   ("C-c o a" . #'org-agenda)
   ("C-c o c" . #'org-capture) ;; recommended [[https://orgmode.org/manual/Activation.html][1.3]] at  of the org manual
   ("C-c o s" . #'org-schedule)
   ("C-c o d" . #'org-deadline)

   ("C-c o ;" . #'org-timer-set-timer)
   ("C-c o !" . #'org-time-stamp-inactive)
   ("C-c o ," . #'org-timer-pause-or-continue)
   ("C-c o _" . #'org-timer-stop)
   ("C-c o 0" . #'org-timer-start)
   ("C-c o ." . #'org-time-stamp)
   )
  :hook
  (org-mode . org-indent-mode)
  )
;; (after-save-hook . org-babel-tangle))

(use-package org-roam
  :custom
  (org-roam-directory "~/orgRoam")
  :bind
  (
   ;; ("C-c o l" . #'org-roam-buffer-toggle)
   ("C-c o f" . #'org-roam-node-find)
   ("C-c o i" . #'org-roam-node-insert)
   ("C-c o n" . #'org-roam-dailies-capture-date)
   ("C-c o N" . #'org-roam-dailies-goto-date)
   )
  :config
  (org-roam-setup)
  (setq org-roam-dailies-directory "journal/"))

(require 'tls)
(use-package erc
  :custom
  (erc-prompt (lambda () (concat "[" (buffer-name) "]")))
  (erc-server "irc.libera.chat")
  (erc-nick "hosklla'")
  ;; (erc-auto-query 'bury)
  (erc-fill-column 100)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 20))

(use-package ement)

(use-package net-utils
  :custom
  (netstat-program "ss")
  ;; (netstat-program-options '("-tunlp"))
  (ifconfig-program "ip")
  (ifconfig-program-options '("a"))

  :bind
  (
   ("C-c p s" . proced)
   ("C-c n n" . netstat)
   ("C-c n p" . ping)
   ("C-c n i" . ifconfig)
   ("C-c n l" . nslookup-host)
   ("C-c n d" . dig)
   ("C-c n s" . smbclient)
   ("C-c n f" . finger)
   ("C-c n w" . whois)
   )
  )

(use-package magit
  :bind
  (
   ;; getting to the magit status buffer is C-x g by default
   ("C-c g" . 'magit-file-dispatch)
   ("C-c i" . 'magit-init)
   )
  :config
  (setq transient-default-level 7)

  (setq magit-refresh-status-buffer t)
  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-headers)
  (setq magit-diff-highlight-indentation nil
        magit-diff-highlight-trailing nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-hunk-body nil
        magit-diff-refine-hunk nil)

  ;; No significant improvement for me
  ;; (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header)
  ;; (remove-hook 'magit-status-headers-hook 'magit-insert-status-headers)
  ;; (remove-hook 'magit-status-headers-hook 'magit-insert-unpushed-to-pushremote)
  ;; (remove-hook 'magit-status-headers-hook 'magit-insert-unpushed-to-upstream-or-recent)
  ;; (remove-hook 'magit-status-headers-hook 'magit-insert-unpulled-from-upstream)
  ;; (remove-hook 'magit-status-headers-hook 'magit-insert-unpulled-from-pushremote)
  )

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo.gpg")))

;; weird evil-collection keybinds
(use-package git-timemachine)

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package eglot
  :hook (prog-mode-hook . eglot-ensure)
  :config
  (defvar eglot-keymap (make-sparse-keymap))
  (global-set-key (kbd "C-l") eglot-keymap)
  (define-key eglot-keymap "a" #'eglot-code-actions)
  (define-key eglot-keymap "r" #'eglot-rename)
  (define-key eglot-keymap "i" #'eglot-find-implementation)
  (define-key eglot-keymap "t" #'eglot-find-typeDefinition)
  (define-key eglot-keymap "d" #'eglot-find-declaration)
  (define-key eglot-keymap "f" #'eglot-format)
  (setq eldoc-echo-area-use-multiline-p nil)
  :bind (
         ;; ("C-l" . #'eglot-keymap)
         ;; ("C-l a" . #'eglot-code-actions)
         ;; ("C-l r" . #'eglot-rename)
         ;; ("C-l f" . #'eglot-format)
         ;; ("C-l i" . #'eglot-find-imlementation)
         ;; ("C-l t" . #'eglot-find-typeDefinition)
         ;; ("C-l d" . #'eglot-find-declaration)
         ("M-j" . flymake-goto-next-error)
         ("M-k" . flymake-goto-prev-error)
         )
  :hook
  (haskell-mode-hook . eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  )

(use-package dap-mode)

(use-package haskell-mode
  :config
  (setq haskell-interactive-popup-errors nil)
  (add-hook 'haskell-cabal-mode #'electric-indent-mode))

;; (use-package agda2-mode
;;   :custom
;;   (agda2-program-args '("--ignore-interfaces" "--local-interfaces" "--guardedness"))
;;   )
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))

(use-package nix-mode)

(use-package proof-general)

(use-package agda2-mode)

(use-package rust-mode)

(use-package julia-mode)
(use-package julia-repl)
(use-package julia-vterm)

;; (use-package pdf-tools
;;   :mode "\\.pdf\\"
;;   :config
;;   (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
;;   )
