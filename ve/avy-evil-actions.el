(setf (alist-get ?w avy-dispatch-alist)
      #'my/avy-evil-delete)
(setf (alist-get ?y avy-dispatch-alist)
    #'my/avy-evil-copy)
(setf (alist-get ?c avy-dispatch-alist)
      #'my/avy-evil-change)
(setf (alist-get ?a avy-dispatch-alist)
      #'my/avy-embark-act)

(defun my/avy-embark-act (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr
      (ring-ref avy-ring 0))))
  t)

(defvar my/evil-extract-count-keys nil)

(defun my/evil-extract-count (orig-fn keys)
  "When an evil operation is called from avy `evil-extract-count'
will get the last key given to avy as input. In order to behave
normally we wrap the function so that we can give the input
manually using a let-binding."
  (funcall orig-fn
           (or my/evil-extract-count-keys keys)))
(advice-add #'evil-extract-count 
            :around #'my/evil-extract-count)


(defun my/avy-evil-delete (pt)
  (interactive)
  (save-excursion
    (goto-char pt)
    (let ((my/evil-extract-count-keys "d"))
      (call-interactively #'evil-delete)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defun my/avy-evil-copy (pt)
  (interactive)
  (save-excursion
    (goto-char pt)
    (let ((my/evil-extract-count-keys "y"))
      (call-interactively #'evil-yank)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)

(defvar my/avy-evil-change-marker nil
  "The place where the user called ivy from.")

(defun my/avy-evil-change (pt)
  (interactive)
  (setq my/avy-evil-change-marker (point-marker))
  (goto-char pt)
  (add-hook 'evil-insert-state-exit-hook #'my/avy-evil-change-h)
  (call-interactively #'evil-change)
  t)

(defun my/avy-evil-change-h ()
  (remove-hook 'evil-insert-state-exit-hook #'my/avy-evil-change-h)
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  (goto-char (marker-position my/avy-evil-change-marker)))
