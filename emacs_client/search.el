
(defun goto-set (set-id)
  (view-set-details set-id))

(defun goto-thing (thing-id)
  (view-thing-details thing-id))

(defun goto-tag (tag-id)
  (view-tag-details tag-id))

(defun search-sets-things-tags ()
  (interactive)
  (fetch-all-goto-candidates
   (lambda (data)
     (setq candidates (append data nil))

     (if (null candidates)
	 (message "No goto opportunities here!")
       (helm :sources (helm-build-sync-source "Select an option"
			:candidates (lambda ()

				      (mapcar
				       (lambda (c)
					 (cons (alist-get 'entity_name c)
					       (list (alist-get 'entity_id c) (alist-get 'entity_type c))))
				       candidates))
			:action (lambda (selected-entity)
				  (message "Selected entity of type %s and ID: %s"
					   (nth 1 selected-entity) (nth 0 selected-entity))
				  (let ((entity-id (nth 0 selected-entity))
					(entity-type (nth 1 selected-entity)))
				    (cond
				     ((string= entity-type "Set_") (goto-set entity-id))
				     ((string= entity-type "Thing") (goto-thing entity-id))
				     ((string= entity-type "Tag") (goto-tag entity-id))
				     (t (message (format "Bad entity type: %s" entity-type))))
				    )
				  ))
	     :buffer "*helm-select-thing*")
       ))
   (lambda (err)
     )
   ))

(provide 'search)
