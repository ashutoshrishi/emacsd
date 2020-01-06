;;; custom-editor.el --- Setup for the editing part of Emacs

;;; Commentary:
;; Contains everything besides the language and programming specific settings.

;;; Code:

;; Some nice defaults
(setq-default
 confirm-kill-emacs 'yes-or-no-p    ; Confirm before exiting Emacs
 delete-by-moving-to-trash t        ; Delete files to trash
 display-time-format "%H:%M"        ; Format the time string
 cursor-type 'bar                   ; Single line bar cursor
 fill-column 80                     ; Set width for automatic line breaks
 indent-tabs-mode nil               ; Stop using tabs to indent
 tab-width 4                        ; but maintain correct tab width
 c-basic-offset 2                   ; C based languages indent offset
 inhibit-startup-screen t           ; Disable start-up screen
 require-final-newline t            ; Newline at the end of the file
 global-auto-revert-mode t          ; Auto revert buffs
 dired-dwim-target t                ; If there is a dired buffer displayed
                                        ; in the next window, use it's current
                                        ; subdir instead
 column-number-mode t) ; column numbers

(delete-selection-mode 1)           ; Replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)       ; Replace yes/no prompts with y/n
(global-subword-mode 1)             ; Iterate through CamelCase words
(set-default-coding-systems 'utf-8) ; Default to utf-8 encoding

;; Nice scrolling and performance
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1
      ;; reduce the frequency of garbage collection by making it happen on
      ;; each 50MB of allocated data (the default is on every 0.76MB)
      gc-cons-threshold 50000000
      ;; warn when opening files bigger than 100MB
      large-file-warning-threshold 10000000
      ;; Flymake
      flymake-run-in-place nil
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Editor Config                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save recent files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" "savefile")
        recentf-max-saved-items 30
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

;; Meaningful names for buffers with the same name
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator " • ")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; A useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " - " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name
                                             (buffer-file-name)) "%b"))))


;; Default Frame size
(add-to-list 'default-frame-alist '(height . 53))
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(top . 30))
(add-to-list 'default-frame-alist '(left . 400))

;; Backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings                                                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Align your code in a pretty way.
(global-set-key (kbd "C-x \\") 'align-regexp)

;; Font size
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Window switching. (C-x o goes to the next window)
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1))) ;; back one

;; Whitespace mode quick interaction
(global-set-key (kbd "C-c C-w") 'whitespace-cleanup)

;; Align anything
(global-set-key (kbd "M-[") 'align)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing and movement                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :ensure t
  :diminish
  :commands (projectile-mode)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package counsel-projectile
  :ensure t
  :diminish
  :hook (after-init . counsel-projectile-mode))

;; Ivy
(use-package ivy
  :ensure t
  :diminish
  :after (projectile)
  :bind (("\C-s"    . swiper)
         ("C-c C-r" . ivy-resume)
         ("<f6>"    . ivy-resume)
         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)
         ("C-l" . ivy-backward-delete-char))
  :config
  (ivy-mode 1)
  ;; Allow commands in minibuffers
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 20)
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :config (ivy-rich-mode 1))

;; Faster searching
(use-package counsel
  :ensure t
  :after (ivy)
  :diminish ivy-mode counsel-mode
  :bind (("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("<f1> f"  . counsel-describe-function)
         ("<f1> v"  . counsel-describe-variable)
         ("<f1> l"  . counsel-find-library)
         ("<f2> i"  . counsel-info-lookup-symbol)
         ("<f2> u"  . counsel-unicode-char)
         ("C-c g"   . counsel-git)
         ("C-c k"   . counsel-rg)
         ("C-c i"   . counsel-imenu)
         ("C-x l"   . counsel-locate)
         ("M-y"     . counsel-yank-pop))
  :config
  (counsel-mode 1)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

;; Better M-x, enhanced by counsel
(use-package smex
  :ensure t)

;; Rich interface for IVY
(use-package all-the-icons
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c j" . ace-jump-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming editor                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :commands (magit-status)
  :bind ("C-x g" . magit-status))

(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package show-paren-mode
  :commands (show-paren-mode)
  :hook (prog-mode . show-paren-mode))

(use-package smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(use-package diff-hl
  :ensure t
  :commands (diff-hl-mode)
  :hook (prog-mode . diff-hl-mode))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))

(use-package whitespace-cleanup-mode
  :ensure t
  :commands (whitespace-cleanup-mode))

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :commands (flycheck-global-mode flycheck-mode)
  :init (global-flycheck-mode))

;; Set flycheck to only bother on save.
;; (setq flycheck-check-syntax-automatically '(save idle-change)))

(use-package company
  :ensure t
  :diminish company
  :config
  (global-company-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :bind (("C-c C-f" . lsp-format-buffer))
  :init
  ;; Preferring flycheck over flymake
  (setq lsp-prefer-flymake nil)
  (add-to-list 'exec-path "~/.local/src/elixir-ls/release")
  :hook ((elixir-mode     . lsp-deferred)
         (rust-mode       . lsp-deferred)
         (web-mode        . lsp-deferred)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . lsp-ui-mode))
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-flycheck-live-reporting nil))

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic file modes                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package flyspell
  :ensure t
  :commands (flyspell-mode flyspell-prog-mode)
  :hook ( ;; (prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode))
  :init
  ;; Dictionary environment
  (setenv "DICTIONARY" "en_GB")
  :config
  (setq flyspell-issue-message-flag nil)
  (setq ispell-list-command "--list")
  (setq ispell-program-name "aspell"))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :hook (markdown-mode . visual-line-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc"))

(use-package json-mode
  :ensure t
  :mode ("\\.json$")
  :config (setq js-indent-level 2))

;; Yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml$"))

;; YASnippets
(use-package yasnippet
  :ensure t)

;; Org mode
(use-package ox-gfm
  :defer 3
  :after org)

(use-package ox-md
  :defer 3
  :after org)

(use-package org
  :ensure t
  :commands (org-mode org-agenda)
  :bind (("\C-ca" . org-agenda))
  :hook ((org-mode . visual-line-mode)
         (org-mode . visual-fill-column-mode))
  :init
  (setq org-agenda-files (list "~/plans/me.org" "~/plans/cadmus.org")))


(use-package org-present
  :ensure t
  :commands (org-present)
  :init
  (eval-after-load "org-present"
    '(progn
       (add-hook 'org-present-mode-hook
                 (lambda ()
                   (org-present-big)
                   (org-display-inline-images)
                   (org-present-hide-cursor)
                   (org-present-read-only)))
       (add-hook 'org-present-mode-quit-hook
                 (lambda ()
                   (org-present-small)
                   (org-remove-inline-images)
                   (org-present-show-cursor)
                   (org-present-read-write))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package material-theme
  :ensure t
  :init
  (load-theme 'material t))

;; natural-title-bar option was removed from this formula
;; duplicate its effect:
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (add-to-list 'default-frame-alist '(ns-appearance . light))

(provide 'custom-editor)
;;; custom-editor.el ends here
