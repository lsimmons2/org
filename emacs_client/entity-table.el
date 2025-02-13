;;; -*- lexical-binding: t -*-


(defconst entity-id-col-width 7)
(defconst entity-name-col-width 35)
(defconst created-at-col-width 10)


(defun entity-list-visit-entry ()
  "Open the entity details page for the selected entry."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (and id entity-list-view-callback)
        (funcall entity-list-view-callback id)
      (message "No entry selected!"))))

(defun entity-list-delete-entry ()
  "Delete the selected entity."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (and id entity-list-delete-callback)
        (view-confirm-screen
         (format "Delete entity %d?" id)
         (lambda ()
           (message (format "Deleting entity %d" id))
           (funcall entity-list-delete-callback id))
         (lambda () (message "Aborting delete")))
      (message "No entry selected!"))))


(define-derived-mode entity-list-mode base-list-mode "EntityList"
  "Mode for viewing a list of things."
  (message "Starting entity-list-mode!!!!!!")
  (evil-define-key 'normal entity-list-mode-map (kbd "RET") #'entity-list-visit-entry)
  (evil-define-key 'normal entity-list-mode-map (kbd "d") #'entity-list-delete-entry))


(defun view-entity-list (plural-entity-name col-names entities view-callback delete-callback)
  "Render a list of ENTITIES in the current buffer.
VIEW-CALLBACK is called when an entity is selected.
DELETE-CALLBACK is called when an entity is deleted."

  (entity-list-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not entities)
        (insert (format "*** No %s available ***\n" plural-entity-name))
      (let* ((available-width (- (window-width) 5)) ;; Adjust for margins
             (text-col-width (- available-width (+ entity-id-col-width entity-name-col-width created-at-col-width))))
        (setq tabulated-list-format
              `[(,(nth 0 col-names) ,entity-id-col-width t)
                (,(nth 1 col-names) ,entity-name-col-width t)
                (,(nth 2 col-names) ,text-col-width nil)
		(,(nth 3 col-names) ,created-at-col-width nil)])

        (setq tabulated-list-entries
              (mapcar (lambda (entity)
                        (let ((id (alist-get 'id entity))
                              (name (alist-get 'name entity))
                              (text (or (alist-get 'text entity) "-"))
			      (created-at (alist-get 'created_at entity)))
                          (list id (vector
                                    (number-to-string id)
                                    name
                                    (truncate-string (format-for-tabulated-list-cell text) text-col-width)
				    (format-created-at created-at)))))
                      entities))

        (setq entity-list-view-callback view-callback)
        (setq entity-list-delete-callback delete-callback)

        (tabulated-list-init-header)
        (tabulated-list-print t)))))


(provide 'entity-table)
