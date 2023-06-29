;; auf einem PC muessen die Variablen HOSTNAME und HOME gesetzt werden
;; das .emacs.d Verzeichnis befindet sich im HOME Verzeichnis
(defun my-platform-setup (filename progdir)
   (progn
      (setq my-platform-file filename)
      (setq ve-prog-dir progdir)
      (load (expand-file-name (concat user-emacs-directory "/ve/" my-platform-file)) 'error)
   )
)


(defun linux-default-setup ()
   (message "Linux default setup")
   )


(defun windows-setup ()
   (progn
      (cond ((string= (system-name) "EPost-PC")
               (progn
                  (message "Setup for EPost PC")
                  (my-platform-setup "ve-eps.el" "C:/ved/Programme")
                  )
               )
         ((string= (system-name) "EPS-A9004")
            (progn
               (message "Setup for EPost TCB-Rechner")
               (my-platform-setup "ve-eps-tcb.el" "C:/ved/Programme"))
            )
         )
      )
   )

(cond ((memq system-type '(windows-nt ms-dos)) (windows-setup))
   ((eq system-type 'gnu/linux) (linux-default-setup))
   )
