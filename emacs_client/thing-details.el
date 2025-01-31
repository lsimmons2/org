;;; thing-details.el --- Thing Details View -*- lexical-binding: t -*-



(defun add-tag-to-thing ()
  (interactive)
  (fetch-tags-available-for-thing
   thing-id
   (lambda (data)
     (setq tags (append data nil))

     (if (null tags)
	 (message "Thing is tagged with all available tags - no tags left!")
       (helm :sources (helm-build-sync-source "Select a Thing"
			:candidates (lambda ()
				      (mapcar
				       (lambda (tag)
					 (cons (alist-get 'name tag)
					       (alist-get 'id tag)))
				       tags))
			:action (lambda (selected-tag-id)
				  (message "Selected Tag ID: %s" selected-tag-id)
				  (tag-thing
				   selected-tag-id
				   thing-id
				   (lambda (data)
				     (view-thing-details thing-id)
				     ))
				  ))
	     :buffer "*helm-select-thing*")
       ))
   ))

(defconst name-label "Name:")
(defconst text-label "Description:")
(defconst tags-label "Tags:")

(defvar-local thing-id nil
  "ID of the thing being displayed in thing-details-mode.")

(defun parse-name-text-from-buffer ()
  (save-excursion
    ;; First check if both required labels exist
    (goto-char (point-min))
    (unless (and (search-forward name-label nil t)
                 (search-forward text-label nil t))
      (error (format "Buffer must contain '%s', '%s', and '%s' labels" name-label text-label tags-label)))
    
    ;; If we get here, both labels exist, so let's parse
    (let (name description)
      (goto-char (point-min))
      (when (search-forward name-label nil t)
        (setq name (string-trim
                    (buffer-substring-no-properties
                     (point)  ; Start right after "Name:"
                     (progn
		       (if (search-forward text-label nil t)
                           (- (match-beginning 0) 1)  ; Stop right before "Description:"
                         (point-max)))))))  ; Or take until end of buffer if no Description label
      
      (goto-char (point-min))  ; Go back to start to search for Description
      (when (search-forward text-label nil t)
        (setq description (string-trim
			   (buffer-substring-no-properties
			    (point)  ; Start right after "Name:"
			    (progn
			      (if (search-forward tags-label nil t)
				  (- (match-beginning 0) 1)  ; Stop right before "Description:"
				(point-max)))))))
      (list name description)
      )
    ))


(defun save-thing-name-and-text ()
  "Submit the form and process the data."
  (interactive)
  (let* ((parsed-data (parse-name-text-from-buffer))
         (name (nth 0 parsed-data))
         (description (nth 1 parsed-data)))
    (message "Saving - Name: %s, Description: %s" name description)
    (update-thing thing-id name description
                  (lambda (response)
                    (let ((thing-id (alist-get 'id response)))
		      (message "Thing updated successfully: %s" thing-id)
		      (view-thing-details thing-id))))))

(define-derived-mode thing-details-mode fundamental-mode "Thing-Details"
  "Major mode for viewing details of a single thing."

  (setq-local buffer-read-only nil)

  (when (string-match "*Thing \\([0-9]+\\)*" (buffer-name))
    (setq thing-id (string-to-number (match-string 1 (buffer-name)))))

  (when (featurep 'evil)
    (message "Setting thing details mappings please")
    (evil-define-key 'normal thing-details-mode-map
      (kbd "SPC a a") 'add-tag-to-thing
      (kbd "S") 'save-thing-name-and-text
      ))


  )


(defun render-thing-details (thing)
  "Render the details of THING and its associated TAGS."
  (let ((tags (append (alist-get 'tags thing) nil)))
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") (alist-get 'name thing)))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (alist-get 'text thing)))
    (insert (format "%s\n" (format-header "Tags:")))
    (render-tag-tiles
     "thing-tags"
     tags
     (lambda (tag-id)
       (untag-thing
	tag-id
	thing-id
	(lambda (_)
	  (view-thing-details thing-id)))
       ))
    ))


(defun view-thing-details (thing-id)
  "Display the details of a thing by THING-ID."
  (interactive)
  (message "Here in view-thing-details view for thing with ID: %d" thing-id)
  (let ((buffer (get-buffer-create (format "*Thing %d*" thing-id))))
    (switch-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading thing details...\n"))
    (thing-details-mode)

    (fetch-thing
     thing-id
     (lambda (data)
       (with-current-buffer buffer
         (render-thing-details data)))
     (lambda (err)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
	   (erase-buffer)
	   (insert (format-error "Error fetching thing\n"))))
       )
     )
    ))


(provide 'thing-details)
