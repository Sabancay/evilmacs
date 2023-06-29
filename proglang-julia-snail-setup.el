* LSP (eglot)

#+begin_src emacs-lisp
  (use-package eglot
      :defer t
      :commands (eglot eglot-ensure)
        :hook (
               ;;(python-mode . eglot-ensure)
               (julia-mode . eglot-ensure)
               )
        :bind (:map eglot-mode-map
                    ("C-<down-mouse-1>" . #'xref-find-definitions)
                    ("C-S-<down-mouse-1>" . #'xref-find-references)
                    ("C-c a r" . #'eglot-rename)
                    ("C-c C-c" . #'eglot-code-actions))
        :custom
        (eglot-confirm-server-initiated-edits nil)
        (eglot-autoshutdown t)
        (eglot-send-changes-idle-time 0.1)
        :config
        (defun pt/add-eglot-to-prog-menu (old startmenu click)
          "Add useful Eglot functions to the prog-mode context menu."
          (let ((menu (funcall old startmenu click))
                (identifier (save-excursion
                              (mouse-set-point click)
                              (xref-backend-identifier-at-point
                               (xref-find-backend)))))
            (when identifier
              (define-key-after menu [eglot-find-impl]
                `(menu-item "Find Implementations" eglot-find-implementation
                            :help ,(format "Find implementations of `%s'" identifier))
                'xref-find-ref))
            menu))
        (advice-add 'prog-context-menu :around #'pt/add-eglot-to-prog-menu)
        (setq eglot-stay-out-of '(company))
        )

      (use-package consult-eglot
        :config
        (defun pt/consult-eglot ()
          (interactive)
          (let ((completion-styles '(emacs22)))
            (call-interactively #'consult-eglot-symbols)))
        :bind (:map eglot-mode-map ("s-t" . #'pt/consult-eglot)))


  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       #'cape-file))))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
#+end_src


* Julia

#+begin_src emacs-lisp

(use-package julia-mode
  :ensure t
  :init (setenv "JULIA_NUM_THREADS" "6")
  :mode
  ("\\.jl\\'"  . julia-mode)
  :config
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-to-list 'julia-arguments "-t12")
  (add-to-list 'julia-arguments "--project=\".\"")
  )

(when sys/linuxp
  (use-package julia-snail
    :ensure t
    :hook
    (julia-mode . julia-snail-mode)
    :config
    (setq julia-snail-use-emoji-mode-lighter t)
    (setq julia-snail-popup-display-eval-results nil)
    (setq julia-snail-repl-display-eval-results t)
    (setq julia-snail-doc-lookup t)
    )
  )

  (use-package eglot-jl
        :after eglot
        :commands eglot-jl-init
        :config
        (setq eglot-connect-timeout 100)
        (cl-defmethod project-root ((project (head julia)))
          (cdr project))
        )
#+end_src

* Python
#+begin_src emacs-lisp
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
#+end_src


;;#+begin_src emacs-lisp
(use-package python-mode
  :defer
  :after eglot
  :config
  (when (executable-find "ipython3")
    (setq python-shell-interpreter "ipython3"
          python-shell-interpreter-args "--simple-prompt --classic"))
)
;;#+end_src

;;; extended-setup.el ends here
