
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Defining important directory names
(defvar arr-root-dir (file-name-directory load-file-name)
  "The root configuration directory.")
(defvar arr-modules-dir (expand-file-name "modules" arr-root-dir)
  "The core modules directory.")
(defvar arr-vendor-dir (expand-file-name "vendor" arr-root-dir)
  "External packages installed through the package manager.")
(defvar arr-savefile-dir (expand-file-name "savefile" arr-root-dir)
  "Auto saves and history files.")
(defvar arr-themes-dir (expand-file-name "themes" arr-root-dir)
  "For the themes!.")

;; Melpa package manager
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Melpa unstable
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; Main package management utility
(require 'use-package)

;; Creating the savefile dir if it doesn't exist
(unless (file-exists-p arr-savefile-dir)
  (make-directory arr-savefile-dir))

;; Add the special directories to the load paths
(add-to-list 'load-path arr-vendor-dir)
(add-to-list 'load-path arr-modules-dir)
(add-to-list 'custom-theme-load-path arr-themes-dir)

;; Byte compilation
(setq load-prefer-newer t)
(byte-recompile-directory arr-root-dir 0)


;; Loading user packages
(use-package arr-macos
  :if (memq window-system '(mac ns))) ;; OSX specific settings
(use-package arr-core) ;; Core functions and personal additions
(use-package arr-editor) ;; General Emacs editing related config

;; (use-package arr-programming) ;; Emacs general programming setup


;; Config changes made through the customize UI will be store here
(setq custom-file (expand-file-name "custom.el" arr-root-dir))
(load custom-file)

;; Start the emacs server
(use-package server
  :config
  (unless (server-running-p) (server-start)))

;; Byte compilation setup
