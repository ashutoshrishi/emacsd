;;; init.el --- My emacs configuration -*- no-byte-compile: t -*-

;;; Commentary:

;;; Code:

;; Add the special directories to the load paths
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; Melpa package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Melpa unstable
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(setq load-prefer-newer t)
(package-initialize)
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Main package management utility
(require 'use-package)

;; Creating the savefile dir if it doesn't exist
(unless (file-exists-p "savefile") (make-directory "savefile"))

;; Loading user packages
;; OSX specific settings
(use-package custom-macos
  :if (memq window-system '(mac ns)))
;; Core functions and personal additions
(use-package custom-core)
;; General Emacs editing related config
(use-package custom-editor)
;; Programming languages
(use-package custom-haskell)
(use-package custom-rust)
(use-package custom-javascript)

;; Config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Start the emacs server
(use-package server
  :config
  (if (and (fboundp 'server-running-p)
         (not (server-running-p)))
   (server-start)))

(provide 'init)
;;; init ends here
