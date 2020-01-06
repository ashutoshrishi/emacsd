;;; custom-macos.el --- MacOS Specific settings

;;; Commentary:
;; MacOS specific settings like switching to the real alt key

;;; Code:

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. Let's fix that:
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)))

;; Keybinds
(global-set-key [(hyper a)] 'mark-whole-buffer)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)
(global-set-key [(hyper s)] 'save-buffer)
(global-set-key [(hyper l)] 'goto-line)
(global-set-key [(hyper w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(hyper z)] 'undo)
;; mac switch meta key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)(setq ns-function-modifier 'hyper)

(menu-bar-mode +1)

;; Use emacs terminfo and not system terminfo
(setq system-uses-terminfo nil)

(provide 'custom-macos)
;;; custom-macos.el ends here
