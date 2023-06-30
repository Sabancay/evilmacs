;;; init.el -*- lexical-binding: t; -*-
;;; Volker Edelmann, based on Patrick Thomson's Emacs setup.
;;
;;; Commentary:
;; This file loads use-package, org-mode and compiles and executes
;; my emacs org configuration
;;
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("ublt" . "https://elpa.ubolonton.org/packages/") t)
;;(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(setq package-native-compile t)
(setq use-package-always-ensure nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (message "refreshing contents")
  (unless package-archive-contents (package-refresh-contents))
;;  (setq use-package-always-ensure t)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (require 'ob-tangle))

(defun reload-config ()
  "Reload the literate config from several files"
  (interactive)
  (message "Reloading my configuration")
  (toggle-debug-on-error)
  (org-babel-load-file (concat user-emacs-directory "/environment-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/basic-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/modal-editing.org"))
  (org-babel-load-file (concat user-emacs-directory "/text_gtd-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/programming-common-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/proglang-oldschool-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/proglang-modern-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/windows-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/linux-setup.org"))
  (org-babel-load-file (concat user-emacs-directory "/keybindings.org"))
  )


(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  max-lisp-eval-depth 2000
            )
          )
  )

(add-to-list 'load-path (concat user-emacs-directory "/ve"))

(print "Load Path:")
(princ (mapconcat #'identity load-path "\n"))

(reload-config)


(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'init)
;;; init.el ends here
