;;; custom-javascript.el --- JS + TS + Web stuff

;;; Commentary:

;;; Code:

;; (use-package tide
;;   :ensure t
;;   :config
;;   (setq company-tooltip-align-annotations t))

;; (use-package tide
;;   :ensure t
;;   :after (typescript-mode company flycheck web-mode)
;;   :hook ((typescript-mode . tide-setup)
;;          (typescript-mode . tide-hl-identifier-mode)
;;          (web-mode . (lambda ()
;;                       (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                         (tide-setup))))
;;          (web-mode . (lambda ()
;;                       (when (string-equal "ts" (file-name-extension buffer-file-name))
;;                         (tide-setup))))))

(use-package add-node-modules-path
  :ensure t
  :commands (add-node-modules-path))


(use-package prettier-js
  :ensure t
  :commands prettier-js-mode)

(use-package web-mode
  :ensure t
  :mode ("\\.tsx$" "\\.ts$")
  :init
  (setq js-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))  
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode))



(provide 'custom-javascript)
;;; custom-javascript ends here
