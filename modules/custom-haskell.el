;;; custom-haskell.el --- Haskell

;;; Commentary:

;;; Code:

(use-package flycheck-haskell
  :ensure t
  :commands flycheck-haskell-setup
  :hook (haskell-mode . flycheck-haskell-setup))

(use-package haskell-mode
  :ensure t
  :commands haskell-mode
  :mode "\\.hs$"
  :hook ((haskell-mode . haskell-decl-scan-mode))
  :bind (:map haskell-mode-map
              ("C-c C-l" . haskell-process-load-file)
              ("C-`"     . haskell-interactive-bring)
              ("C-c C-t" . haskell-process-do-type)
              ("C-c C-i" . haskell-process-do-info)
              ("C-c C-c" . haskell-process-cabal-build)
              ("C-c C-k" . haskell-interactive-mode-clear))
  :init
  ;; (setq haskell-stylish-on-save t)
  ;; (setq haskell-mode-stylish-haskell-path "brittany")
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook
            (lambda () (setq tab-width 2)))
  :config
  (setq tab-width 2)

  ;; Haskell interactive mode variables
  (custom-set-variables
   '(haskell-process-suggest-remove-import-lines t)
   '(haskell-process-auto-import-loaded-modules t)
   '(haskell-process-log t))

  ;; Auto align helpers
  ;; Remember that (align) is bound to M-[
  (require 'align)
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode)))))

(use-package hindent
  :commands hindent-mode
  :hook (haskell-mode . hindent-mode))

(provide 'custom-haskell)
;;; custom-haskell ends here
