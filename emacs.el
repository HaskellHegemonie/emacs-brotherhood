(setq lexical-binding t)
(require 'use-package)

(defmacro define (id &rest args)
  (if (consp id)
      `(defun ,(car id) ,(cdr id) ,@args)
    `(setf  ,id ,@args)
    )
  )

;; examples
;; (define (fac n)
;; 				(if (= n 1)
;; 						1
;; 					(* n (fac (- n 1)))
;; 					)
;; 				)
;; (fac 5)
;; (define foo 'bar)


(defmacro λ (&rest body)
  (define (partition list keys &optional acc)
          (if list
              (let
                  ((head (car list)))
                (if (member head keys)
                    `(,acc . ,(cdr list))
                  (partition (cdr list) keys (append acc (list head)))
                  )
                )
            acc))
  (define (convlist x)
          (if (sequencep x)
              x
            (list x)))
  ;; real code
  (let*
      ((arrows `(→ ->))
       (res (partition body arrows))
       (args (car res))
       (body (cdr res)))
    `(lambda ,(convlist args) ,@body)))


(defalias 'symbol->string 'symbol-name)
(defalias 'string->symbol 'intern)
(defalias 'number->string 'number-to-string)
(defalias 'string->number 'string-to-number)

(define (id x)
        x)

(define id (λ x → x))

(define (succ x)
        (+ 1 x))
(define (pred x)
        (- 1 x))

;; Haskellisierung
(defalias 'elem 'member)
(defalias 'filter 'cl-find-if-not) ;; doesn't work yet
(defmacro foldr (function initial list)
  `(seq-reduce ,function ,list ,initial))

;; (filter (λ x → (= x 1)) (list 1 2 3 4 5 7 nil))

(defmacro words (xs)
  `(string-join ,xs " "))
(defmacro intercalate (x xs)
  `(string-join ,xs ,x))
;; (words `("It's" "nice" "to" "be" "a" "Preiss"))
;; (intercalate " " `("but" "it's" "higher" "to" "be" "a" "Bayer"))


;; string-split seems ok
(defmacro splitOn  (seperators xs)
  `(string-split ,xs ,seperators))

(defmacro chunksOf (size list)
  `(seq-partition ,list ,size))

(defmacro sh (&rest args)
  (let
      ((test
        (mapcar
         (λ a →
            (if (stringp a)
                (format "\"%s\"" a)
              (format "%s" a)
              ))
         args)))
    `(shell-command-to-string ,(intercalate " " test))
    )
  )
(defmacro sh+ (&rest args)
  `(splitOn "\n" (sh ,@args)))
;; (defmacro ⌷sh (index


;; Allows this syntax
;; (sh echo "hello")
;; (sh nix-instantiate --eval -E  "<nixpkgs>")
;; (define my-little-temp-dir (car (sh+ mktemp -d)))
;; you may use my-little-temp-dir as an ordinary string
;; (cd my-little-temp-dir)
;; (pwd)
;; (sh ls)

;; A Lisp Language
;; Noch schlechtere Ideen als "Schlechte Ideen"


(defmacro ⌷ (idx seq)
  `(cond
    ((not (listp ,seq)) nil)
    ((listp ',idx)
     (mapcar
      (λ i →
         (cl-nth-value i ,seq)
         )
      ',idx
      ))
    (t
     (cl-nth-value ,idx ,seq))
    ))


(defmacro ⍳ (length &optional adder)
  `(cl-loop for x from 0 until ,length
            collect (+ x (or ,adder 0))))

;; (⌷ (0 9 2) (⍳ 10))
;; (⌷ (0 0 0 0 0 3 2 1 0) '("Bayern" "des" "samma" "mia"))

(defalias '↑ 'take)
(defalias '↓ 'drop)
(defalias '⍪  'concat)
;; (↑ 3 (⍳ 5 6))

(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(defun my-configure-init-frame (frame)
  (progn
    (keyboard-translate ?\C-t ?\C-x)
    (keyboard-translate ?\C-x ?\C-t)
    (set-frame-font "JetBrainsMonoNerdFont 10" nil nil)
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
  ;; (global-display-line-numbers-mode)

  (display-time-mode 1)
  :hook
  (prog-mode . display-line-numbers-mode)
  (prog-mode . toggle-input-method)
  (prog-mode . hl-line-mode)

  (text-mode  . display-line-numbers-mode)
  (text-mode  . toggle-input-method)
  (text-mode  . hl-line-mode)

  :bind*
  (
   ("C-x C-h" . #'switch-to-buffer) ;; oh yes
   ("C-M-e" . #'eshell)
   ("C-v" . #'universal-argument)
   ("C-c C-r" . (lambda ()
                  (interactive)
									(pcase major-mode
										('agda2-mode (call-interactively 'agda2-refine))
										(t (progn
												 (recompile)
												 (delete-window)
												 )))
									)
		)
   )

  :config
  (define-key key-translation-map (kbd "M-t") [ersatz-meta-x])
  (global-set-key [ersatz-meta-x] 'execute-extended-command)

  :custom
  (make-backup-files nil)
  (tab-with 2)
  (indent-line-function 'noindent)
  ;; (display-line-numbers-type t)
  (blink-cursor-mode nil)
  (truncate-lines 1)
  (standard-indent 2)
  (tab-width 2)
  (c-tab-always-indent nil)
  (c-syntactic-indentation nil)
  (indent-tabs-mode t)
  (tab-width 2)
  (scroll-margin 8)
  ;; (display-line-numbers 'relative)
  (display-line-numbers-type 'relative)
  (eletric-indent-mode nil)
  (lexical-binding t)
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
  :init
  (global-unset-key (kbd "C-v"))

  :custom
  (evil-want-integration t)
  (evil-want-keybinding nil)
  (evil-want-C-g-bindings nil)
  (evil-want-C-u-scroll t)
  (evil-want-C-d-scroll t)
  (evil-want-C-h-delete nil)

  :bind*
  (("C-c C-h" . #'mode-line-other-buffer)
   ("C-M-v" . #'evil-visual-block)
   ("C-v" . #'universal-argument))
  :config
  (evil-mode 1)

  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'eshell 'emacs)
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'shell 'emacs)
  (evil-set-initial-state 'comint-mode 'emacs)

  (evil-set-initial-state #'mrepl 'emacs)
  (evil-set-initial-state #'sly-db-mode 'emacs)

  (evil-set-initial-state 'Man-mode 'emacs)
  (define-key evil-normal-state-map (kbd "C-e") #'move-end-of-line)
  (define-key evil-normal-state-map (kbd "C-p") #'previous-line)
  (define-key evil-normal-state-map (kbd "C-n") #'next-line)
  (define-key evil-normal-state-map (kbd "C-y") #'yank)
  (define-key evil-normal-state-map (kbd "M-y") #'yank-pop)
  (define-key evil-normal-state-map (kbd "C-f") #'forward-char)
  (define-key evil-normal-state-map (kbd "C-b") #'backward-char)

  (define-key evil-normal-state-map (kbd "M-n") #'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "M-p") #'evil-scroll-up)

  (define-key evil-insert-state-map (kbd "M-n") #'evil-scroll-down)
  (define-key evil-insert-state-map (kbd "M-p") #'evil-scroll-up)

  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state)
  (define-key evil-insert-state-map (kbd "TAB") #'tab-to-tab-stop)
  (define-key evil-visual-state-map (kbd "C-e") #'move-end-of-line)
  (define-key evil-visual-state-map (kbd "C-f") #'forward-char)
  (define-key evil-visual-state-map (kbd "C-b") #'backward-char)
  (define-key evil-visual-state-map (kbd "C-y") #'yank)
  (define-key evil-visual-state-map (kbd "M-y") #'yank-pop)
  (setq evil-insert-state-cursor 'box)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defvar evil-window-maps (make-sparse-keymap))

(global-set-key (kbd "C-w") evil-window-maps)
(define-key evil-window-maps (kbd "C-v") #'evil-window-vsplit)
(define-key evil-window-maps (kbd "C-s") #'evil-window-split)
(define-key evil-window-maps (kbd "C-l") #'evil-window-right)
(define-key evil-window-maps (kbd "C-h") #'evil-window-left)
(define-key evil-window-maps (kbd "C-j") #'evil-window-down)
(define-key evil-window-maps (kbd "C-k") #'evil-window-up)
(define-key evil-window-maps (kbd "C-q") #'delete-window)
(define-key evil-window-maps (kbd "C-w") #'kill-region)
(define-key evil-window-maps (kbd "x") #'evil-window-exchange)
(define-key evil-window-maps (kbd "=") #'balance-windows)
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

(use-package info
  :bind
  ("C-d" . #'evil-scroll-down)
  ("C-u" . #'evil-scroll-up)
  ;; ("j"   . #'evil-next-line)
  ;; ("k"   . #'evil-previous-line)
  )

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

(use-package corfu
  :init
  (global-corfu-mode)
  )

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  ;; Since 29.1, use `dabbrev-ignored-buffer-regexps' on older.
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package rg
  :init
  (global-unset-key (kbd "C-r"))
  (keymap-global-unset "C-r")
  :bind
  (("C-r"  . #'rg)
   ))

(use-package epg
  :custom
  (epg-pinentry-mode 'loopback)
  (epa-armor t))



(use-package eshell
  :custom
  (eshell-aliases-file "/etc/nixos/config/emacs/eshell-aliases")
  )

(use-package project
	:bind
	(
	 ("C-x p r"
		. (lambda ()
				(interactive)
				(project-recompile)
				(delete-window)
				))
	 )
	)

(use-package guix)

(use-package geiser
	:config
	;; (add-to-list 'geiser-guile-load-path (file-name-concat (getenv "GUIX_PROFILE") "share"))
	;; (
	 ;; (add-to-list 'geiser-guile-load-path "~/code/Guix/src/guix/")
  ;; (with-eval-after-load 'geiser-guile   (add-to-list 'geiser-guile-load-path "~/code/Guix/src/guix"))
  ;; (with-eval-after-load 'geiser-guile   (add-to-list 'geiser-guile-load-path "~/code/Guix/src/nonguix"))

  ;; :c
	ustom
  (geiser-default-implementation 'guile)
  )
(use-package geiser-guile)

(use-package nix-mode
  :bind
  (("C-M-n" . #'nix-repl))
  )

(require 'org)
(use-package org
  ;; :after consult
  :init
  (defun hsheg/tangle-save-in-org ()
    (when
        (string= (file-name-extension (buffer-file-name (current-buffer))) "org")
      (org-babel-tangle)
      ))
  (defun char-to-hex (char)
    (interactive "cEnter char: ")
    (format "%x" char)
    )
  :hook
  (org-mode . org-indent-mode)
  (org-mode . org-num-mode)
  (after-save . hsheg/tangle-save-in-org)
  :config
  :custom
  (org-list-allow-alphabetical t)
  ;; (org-directory "~/orgRoam/agenda")
  (org-agenda-span 14)
  (org-agenda-files nil) ;; can also set with =C-c [= per project
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'split-window-below)
  (org-todo-keywords
   `(
     (sequence "TODO(t)" "DONE(d)")
     (sequence "IN-PROGRESS(p)" "FIXED(f)" "KILLED(k)")
     (sequence "LAZY(l)")
     ))
  (setf org-src-window-setup 'current-window)
  (setf (cdr (assoc 'output-pdf TeX-view-program-selection)) '("Zathura"))

  (org-structure-template-alist
   '(("a" . "export agda2")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "src elisp")
     ("h" . "src haskell")
     ("g" . "src scheme")
     ("r" . "src rust")
     ("E" . "export")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))
   )
  :bind*
  (
   ("C-c o l" . #'org-store-link)
   ("C-c o a" . #'org-agenda)
   ("C-c o c" . #'org-capture) ;; recommended [[https://orgmode.org/manual/Activation.html][1.3]] at  of the org manual

   ("C-c o y" . #'org-insert-link)
   ("C-c o o" . #'org-open-at-point)
   ("C-c o >" . #'org-goto-calendar)
   ("C-c o <" . #'org-date-from-calendar)
   ("C-c o s" . #'org-schedule)
   ("C-c o d" . #'org-deadline)

   ("C-c o !" . #'org-time-stamp-inactive)
   ("C-c o ," . #'org-timer-pause-or-continue)
   ("C-c o ." . #'org-time-stamp)
   ("C-c o ;" . #'org-timer-set-timer)
   ("C-c o _" . #'org-timer-stop)
   ("C-c o 0" . #'org-timer-start)


   ("C-c C-o C-p" . #'org-set-property)
   ("C-c C-o C-d" . #'org-insert-drawer)
   ("C-c C-o C-h" . #'org-delete-property)
   ("C-c C-s" . #'consult-org-heading)
   )
  :bind*
  (:map
   org-mode-map
   ("C-c C-l" . (lambda ()
                  (interactive)
                  ;; (setq-local compile-command (cdr (assoc 'hsheg-org-cc file-local-variables-alist)))
                  (setq-local compile-command nil)
                  (if compile-command
                      (funcall compile-command)
                    (progn
                      (org-latex-export-to-pdf)
                      )
                    )
                  ))
   ("M-l" . #'org-metaright)
   ("M-h" . #'org-metaleft)
   ("M-j" . #'org-metadown)
   ("M-k" . #'org-metaup)

   ("C-c t" . #'org-todo)
   )
  )

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
	 (:map magit-mode-map
				 ("C-n" . #'magit-section-forward)
				 ("C-p" . #'magit-section-backward)
				 )
   )
  :config
  (setq transient-default-level 7)

  (setq magit-refresh-status-buffer nil)
  (setq auto-revert-buffer-list-filter 'magit-auto-revert-repository-buffer-p)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff)
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-headers)
  (setq magit-diff-highlight-indentation t
        magit-diff-highlight-trailing t
        magit-diff-paint-whitespace t
        magit-diff-highlight-hunk-body t
        magit-diff-refine-hunk t)

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
  ;; :hook
  ;; (prog-mode . eglot-ensure)
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

(use-package vterm)
(use-package multi-vterm
  :config
  :bind
  (("C-c l" . #'multi-vterm-next)
   ("C-c h" . #'multi-vterm-prev)
   ("C-c v" . #'multi-vterm)
   ))

(use-package dap-mode)

(use-package haskell-mode
  :config
  (setq haskell-interactive-popup-errors nil)
  (add-hook 'haskell-cabal-mode #'electric-indent-mode))

(load-file (let ((coding-system-for-read 'utf-8))
						 (shell-command-to-string "agda-mode locate")))
(setq default-input-method "Agda")

(use-package agda2-mode

  :custom
  (agda-input-user-translations
   ;; ⌈ ⌊?
   `(
     ("gl"  . ("\\"))
     ("ok"  . ("∂"))
     ("GNA" . ("∇"))
     ("Gr"  . ("⍴"))
     ("Gi"  . ("⍳"))
     ("Gi_" . ("⍸"))
     ("Ge"  . ("∊"))
     ("Ge=" . ("⍷"))
     ("Ga"  . ("⍺"))
     ("Gaa" . ("⍺⍺"))
     ("Go"  . ("⍵"))
     ("Goo" . ("⍵⍵"))
     ("o\"" . ("⍤"))
     ("O\"" . ("⍥"))
     ("o."  . ("∘."))
     ("*\"" . ("⍣"))
     ("o_"  . ("⍛"))
     ("ol"  . ("⍉"))
     ("op"  . ("⌽"))
     ("on"  . ("⊖"))

     ("xx"  . ("×"))
     ("x*"  . ("×"))
     ("x#"  . ("⍒"))
     ("xl"  . ("⎕"))
     ("xe"  . ("⍟"))
     ("xo"  . ("○"))
     ("xc"  . ("⍝"))
     ("xu"  . ("⍋"))
     ("xd"  . ("⍒"))
     ("xb"  . ("⍎"))
     ("xt"  . ("⍕"))
     ("xS"  . ("⍀"))
     ("xz"  . ("⍪"))
     ("x\"" . ("¨"))
     ("x~"  . ("⍨"))
     ("x;"  . ("⋄"))
     ("x!"  . ("⌷"))
     ("x0"  . ("⍬"))
     ("x,"  . ("⊂"))
     ("x."  . ("⊃"))
     ("x,="  . ("⊆"))
     ("x.="  . ("⊇"))
     ("xw"  . ("∧"))
     ("xw~" . ("⍲"))
     ("xW"  . ("∩"))
     ("xv"  . ("∨"))
     ("xv~" . ("⍱"))
     ("xV"  . ("∪"))
     ("xr"  . ("÷"))
     ("xR"  . ("⌹"))
     ("xn"  . ("¯"))
     ("x/"  . ("⌿"))
     ("x="  . ("⌸"))
     ("x'"  . ("⍞"))
     ("xs"  . ("⌺"))
     ))
  )
;; (defvar gnu-apl--symbols '(;; Top row
;;                            ;; `
;;                            ("diamond" "◊" "`")
;;                            ;; 1
;;                            ("diaeresis" "¨" "1")
;;                            ("i-beam" "⌶" "!")
;;                            ;; 2
;;                            ("macron" "¯" "2")
;;                            ("del-tilde" "⍫" "@")
;;                            ;; 3
;;                            ("less-than" "<" "3")
;;                            ("del-stile" "⍒" "#")
;;                            ;; 4
;;                            ("less-than-or-equal-to" "≤" "4")
;;                            ("delta-stile" "⍋" "$")
;;                            ;; 5
;;                            ("equals" "=" "5")
;;                            ("circle-stile" "⌽" "%")
;;                            ;; 6
;;                            ("greater-than-or-equal-to" "≥" "6")
;;                            ("circle-backslash" "⍉" "^")
;;                            ;; 7
;;                            ("greater-than" ">" "7")
;;                            ("circled-minus" "⊖" "&")
;;                            ;; 8
;;                            ("not-equal-to" "≠" "8")
;;                            ("circle-star" "⍟" "*")
;;                            ;; 9
;;                            ("logical-or" "∨" "9")
;;                            ("down-caret-tilde" "⍱" "(")
;;                            ;; 0
;;                            ("logical-and" "∧" "0")
;;                            ("up-caret-tilde" "⍲" ")")
;;                            ;; -
;;                            ("multiplication-sign" "×" "-")
;;                            ("exclamation-mark" "!" "_")
;;                            ;; =
;;                            ("division-sign" "÷" "=")
;;                            ("quad-divide" "⌹" "+")

;;                            ;; First row
;;                            ;; q
;;                            ("question-mark" "?" "q")
;;                            ;; w
;;                            ("omega" "⍵" "w")
;;                            ("omega-underbar" "⍹" "W")
;;                            ;; e
;;                            ("epsilon" "∊" "e")
;;                            ("epsilon-underbar" "⍷" "E")
;;                            ;; r
;;                            ("rho" "⍴" "r")
;;                            ;; t
;;                            ("tilde" "∼" "t")
;;                            ("tilde-diaeresis" "⍨" "T")
;;                            ;; y
;;                            ("uparrow" "↑" "y")
;;                            ("yen-sign" "¥" "Y")
;;                            ;; u
;;                            ("downarrow" "↓" "u")
;;                            ;; i
;;                            ("iota" "⍳" "i")
;;                            ("iota-underbar" "⍸" "I")
;;                            ;; o
;;                            ("circle" "○" "o")
;;                            ("circle-diaeresis" "⍥" "O")
;;                            ;; p
;;                            ("star-operator" "⋆" "p")
;;                            ("star-diaeresis" "⍣" "P")
;;                            ;; [
;;                            ("leftarrow" "←" "[")
;;                            ("quote-quad" "⍞" "{")
;;                            ;; ]
;;                            ("rightarrow" "→" "]")
;;                            ("zilde" "⍬" "}")
;;                            ;; \
;;                            ("right-tack" "⊢" "\\")
;;                            ("left-tack" "⊣" "|")

;;                            ;; Second row
;;                            ;; a
;;                            ("alpha" "⍺" "a")
;;                            ("alpha-underbar" "⍶" "A")
;;                            ;; s
;;                            ("left-ceiling" "⌈" "s")
;;                            ;; d
;;                            ("left-floor" "⌊" "d")
;;                            ;; f
;;                            ("underscore" "_" "f")
;;                            ("del-tilde" "⍫" "F")
;;                            ;; g
;;                            ("nabla" "∇" "g")
;;                            ;; h
;;                            ("increment" "∆" "h")
;;                            ("delta-underbar" "⍙" "H")
;;                            ;; j
;;                            ("ring-operator" "∘" "j")
;;                            ("jot-diaeresis" "⍤" "J")
;;                            ;; k
;;                            ("apostrophe" "'" "k")
;;                            ("quad-diamond" "⌺" "K")
;;                            ;; l
;;                            ("quad" "⎕" "l")
;;                            ("squish-quad" "⌷" "L")
;;                            ;; ;
;;                            ("down-tack-jot" "⍎" ";")
;;                            ("identical-to" "≡" ":")
;;                            ;; '
;;                            ("up-tack-jot" "⍕" "'")
;;                            ("not-identical-to" "≢" "\"")

;;                            ;; Third row
;;                            ;; z
;;                            ("subset-of" "⊂" "z")
;;                            ;; x
;;                            ("superset-of" "⊃" "x")
;;                            ("greek-letter-chi" "χ" "X")
;;                            ;; c
;;                            ("intersection" "∩" "c")
;;                            ("left-shoe-stile" "⍧" "C")
;;                            ;; v
;;                            ("union" "∪" "v")
;;                            ;; b
;;                            ("up-tack" "⊥" "b")
;;                            ("pound-sign" "£" "B")
;;                            ;; n
;;                            ("down-tack" "⊤" "n")
;;                            ;; m
;;                            ("divides" "|" "m")
;;                            ;; ,
;;                            ("shoe-jot" "⍝" ",")
;;                            ("comma-bar" "⍪" "<")
;;                            ;; .
;;                            ("backslash-bar" "⍀" ">")
;;                            ;; /
;;                            ("slash-bar" "⌿" "/")
;;                            ("quad-colon" "⍠" "?")

;;                            ;; Extras
;;                            ("pi" "π")
;;                            ("root" "√")
;;                            ("inverted-exclamation-mark" "¡")
;;                            ("quad-backslash" "⍂")
;;                            ("inverted-question-mark" "¿")
;;                            ))

(use-package gnu-apl-mode
  :bind*
  (:map gnu-apl-mode-map
   ("C-c C-l" . (lambda
                  ()
                  (interactive)
                  (let
                      (
                       (compile-command
                        (or
                         (cdr
                          (assoc
                           'compile-command
                           file-local-variables-alist))
                         (format
                          "dyalogscript %s"
                          (file-name-nondirectory
                           (buffer-file-name
                            (current-buffer))))))
                       )
                    (recompile)
                    (delete-window)))
    )
   )
  :config
  (define (apl-gen-header)
          (interactive)
          (let*
              ((bfname (buffer-file-name (current-buffer)))
               (curPoint (point))
               (str
                (⍪
                 "⍝ " "-*- compile-command: \"dyalogscript "
                 (file-name-base bfname)
                 "."
                 (file-name-extension bfname)
                 "\"; -*-\n"
                 )
                )
               )
            (goto-char (point-min))
            (insert str)
            (hack-local-variables)
            (goto-char (+ curPoint (length str)))
            ))
  )

(use-package rust-mode
  :hook
  (rust-mode . (lambda
                 ()
                 (setq indent-tabs-mode nil)))
  :bind*
  (:map rust-mode-map
        ("C-c C-l" . (lambda ()
                       (interactive)
                       (setq-local compile-command "cargo run")
                       (recompile)
                       (delete-window)
                       ))
        ("C-c C-c C-x" . (lambda ()
                       (interactive)
                       (setq-local compile-command "cargo test")
                       (recompile)
                       (delete-window)
                       ))
        ("C-c C-c C-u" . (lambda ()
                       (interactive)
                       (setq-local compile-command "cargo check")
                       (recompile)
                       (delete-window)
                       ))
        ("C-r C-h" . (lambda (documentation)
                       (interactive "sSearch for: ")
                       (shell-command (concat "rustup doc" documentation))
                       ))

        )
  )

(use-package purescript-mode)

(use-package racket-mode)
(use-package quack)

(use-package nim-mode)

(use-package proof-general)

(use-package julia-mode)
(use-package julia-repl)
(use-package julia-vterm)

(require 'sly-autoloads)
(use-package sly
  :custom
  (inferior-lisp-program "/run/current-system/sw/bin/sbcl")
  :bind
  (("C-c C-s C-n" . #'sly-stickers-next-sticker)
   ("C-c C-s C-p" . #'sly-stickers-prev-sticker)
   ("C-c C-s C-h" . #'sly-stickers-replay-prev)
   ("C-c C-s C-l" . #'sly-stickers-replay-next)
   ("C-c C-s C-j" . #'sly-stickers-replay-jump)
   )
  )

(use-package sly-asdf)



;; (use-package pdf-tools
;;   :mode "\\.pdf\\"
;;   :config
;;   (add-hook 'pdf-view-mode-hook #'(lambda () (interactive) (display-line-numbers-mode -1)))
;;   )
