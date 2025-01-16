(defvar create-entity-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "S") 'create-entity-submit)
    (define-key map (kbd "C-c C-c") 'create-entity-cancel)
    map)
  "Keymap for `create-entity-mode`.")

(define-derived-mode create-entity-mode fundamental-mode "EntityForm"
  "Major mode for creating entities with name and text fields."
  (setq-local create-entity-callback nil)
  ;; Ensure Evil mode keybindings are set locally
  (when (featurep 'evil)
    (evil-define-key 'normal create-entity-mode-map
      (kbd "S") 'create-entity-submit
      (kbd "C-c C-s") 'create-entity-cancel)))

(defun create-entity-submit ()
  "Submit the form and call the entity creation callback."
  (interactive)
  (let ((name (save-excursion
                (goto-char (point-min))
                (re-search-forward "Name: \\(.*\\)")
                (string-trim (match-string 1))))
        (text (save-excursion
                (goto-char (point-min))
                (if (re-search-forward "Text: \\(.*\\)" nil t)
                    (string-trim (match-string 1))
                  nil))))
    (if (string-empty-p name)
        (message "Name field must be filled.")
      (funcall create-entity-callback name (if (string-empty-p text) nil text)))))

(defun create-entity-cancel ()
  "Cancel the form and close the buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun create-entity-form (entity-name create-function)
  "Create a reusable form for creating an entity with ENTITY-NAME using CREATE-FUNCTION."
  (let ((buffer (generate-new-buffer (format "*Create %s*" entity-name))))
    (switch-to-buffer buffer)
    (create-entity-mode)
    (setq-local create-entity-callback create-function)
    ;; Insert the form
    (insert (format "Create a new %s\n\n" entity-name))
    (insert "Name: \n") ;; User will type here
    (insert "Text: \n") ;; User will type here
    (insert "\nPress S to submit or C-c C-c to cancel.\n")
    ;; Position cursor for user input
    (goto-char (point-min))
    (re-search-forward "Name: ")
    (message "Fill out the fields and press S to submit.")))


(provide 'shared-components)
