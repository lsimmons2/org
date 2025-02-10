
(defun goto-set (set-id)
  (view-set-details set-id))

(defun goto-thing (thing-id)
  (view-thing-details thing-id))

(defun goto-tag (tag-id)
  (view-tag-details tag-id))

(defun format-candidate-type (str)
  (propertize str 'face 'font-lock-keyword-face))

(defun render-goto-list (search-header candidates)
  (if (null candidates)
      (message "No goto opportunities here!")
    (helm :sources (helm-build-sync-source search-header
		     :candidates (lambda ()

				   (mapcar
				    (lambda (c)
				      (cons (format "%s: %s"
						    (format-candidate-type (upcase (alist-get 'entity_type c)))
						    (alist-get 'entity_name c))
					    (list (alist-get 'entity_id c) (alist-get 'entity_type c))))
				    candidates))
		     :action (lambda (selected-entity)
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
    )
  )

(defun search-sets ()
  (interactive)
  (fetch-goto-candidates
   (lambda (data)
     (setq candidates (append data nil))
     (render-goto-list "Select a set" candidates))
   (lambda (err) )
   '((type set))))

(defun search-things ()
  (interactive)
  (fetch-goto-candidates
   (lambda (data)
     (setq candidates (append data nil))
     (render-goto-list "Select a thing" candidates))
   (lambda (err) )
   '((type thing))))

(defun search-tags ()
  (interactive)
  (fetch-goto-candidates
   (lambda (data)
     (setq candidates (append data nil))
     (render-goto-list "Select a tag" candidates))
   (lambda (err) )
   '((type tag))))

(defun search-all-entities ()
  (interactive)
  (fetch-goto-candidates
   (lambda (data)
     (setq candidates (append data nil))
     (render-goto-list "Select an entity" candidates))
   (lambda (err))
   ))

(provide 'search)
