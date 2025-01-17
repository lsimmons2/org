;; TODO: not sure major mode is best for this screen - using it atow cuz it's easiest way to get
;; keybindings working
(define-derived-mode confirm-screen-mode special-mode "ConfirmScreen"
  "Major mode for the confirm screen."
  ;; Evil keybindings for normal state
  (evil-define-key 'normal confirm-screen-mode-map
    (kbd "y") (lambda ()
                (interactive)
                (when (and (boundp 'confirm) confirm)
                  (funcall confirm))
                (kill-buffer)))
  (evil-define-key 'normal confirm-screen-mode-map
    (kbd "n") (lambda ()
                (interactive)
                (when (and (boundp 'cancel) cancel)
                  (funcall cancel))
                (kill-buffer))))


(defun view-confirm-screen (message on-yes on-no)
  "Display a confirm screen with MESSAGE and two buttons: Yes and No.
ON-YES and ON-NO are functions to call when the user confirms or declines."
  (let ((buffer-name "*Confirm Screen*"))
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Activate the confirm-screen mode
      (confirm-screen-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Insert the message and buttons
        (insert message "\n\n")
        (insert-button "Yes"
                       'action (lambda (_) (funcall on-yes)
                                 (kill-buffer buffer-name))
                       'follow-link t)
        (insert "    ") ; Space between buttons
        (insert-button "No"
                       'action (lambda (_) (funcall on-no)
                                 (kill-buffer buffer-name))))
      ;; Set the local callbacks
      (setq-local confirm on-yes)
      (setq-local cancel on-no))
    ;; Switch to the confirm screen buffer
    (switch-to-buffer buffer-name)))




(provide 'confirm-screen)
