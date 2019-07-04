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
 column-number-mode t)               ; column numbers

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
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic Editor Config                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; save recent files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" arr-savefile-dir)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing and movement                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy
(use-package ivy
  :ensure t
  :diminish
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

(use-package ivy-rich
  :ensure t
  :after (ivy)
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  ;; To abbreviate paths using abbreviate-file-name
  (setq ivy-rich-path-style 'abbrev)
  :config
  ;; Enable ivy-rich interface
  (ivy-rich-mode 1)

  ;; Add icons for ivy-switch-buffer
  ;; Defining a transformer
  (defun ivy-rich-switch-buffer-icon (candidate)
    (with-current-buffer
        (get-buffer candidate)
      (let ((icon (all-the-icons-icon-for-mode major-mode)))
        (if (symbolp icon)
            (all-the-icons-icon-for-mode 'fundamental-mode)
          icon))))
  ;; And adding it to ivy-rich--display-transformers-List
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-switch-buffer-icon :width 2)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))))))

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

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c j" . ace-jump-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming editor                                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

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
  :init (global-flycheck-mode)
  :config
  ;; Set flycheck to only bother on save.
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

(use-package company
  :ensure t
  :diminish company
  :config
  (global-company-mode))

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
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "pandoc"))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theming                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-themes
  :ensure t
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config))

(provide 'arr-editor)
;;; arr-editor.el ends here
