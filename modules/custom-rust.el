;;; custom-rust.el --- Rust

;;; Commentary:

;;; Code:

(use-package rust-mode
  :ensure t
  :mode "\\.rs$")

(use-package flycheck-rust
  :ensure t
  :commands (flycheck-rust-setup)
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(provide 'custom-rust)
;;; custom-rust ends here
