;;; arr-programming.el -- Programming modes
;;
;; Copyright (c) 2019 Ashutosh Rishi Ranjan
;;
;; Author: Ashutosh Rishi Ranjan <arishiranjan@gmail.com
;; Version: 2.0.0

;;; Commentary:

;;; Code:

;; Code formatting
(use-package format-all
  :ensure t
  :commands (format-all-mode))

;; LSP
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (add-to-list 'exec-path "~/.local/src/elixir-ls/release")
  :hook ((elixir-mode     . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode        . lsp-deferred)))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :after lsp-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elixir                                                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elixir-mode
  :mode ("\\.exs?$")
  :commands elixir-mode
  :ensure t
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

;; (use-package lsp-elixir
;;   :ensure t
;;   :after elixir-mode
;;   :config
;;   (add-hook 'elixir-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript                                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts$"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Web                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package web-mode
  :ensure t
  :mode ("\\.tsx$"))

(provide 'arr-programming)
;;; arr-programming ends here