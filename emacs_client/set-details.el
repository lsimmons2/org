;; -*- lexical-binding: t; -*-


(require 'domain)
(require 'utils)
(require 'tag-tiles)


(defconst name-label "Name:")
(defconst text-label "Description:")
(defconst yes-tags-label "Yes Tags:")
(defconst no-tags-label "No Tags:")

(defvar-local set-id nil
  "ID of the set being displayed in set-details-mode.")


(defun add-tag-to-set (for-yes-tags) ;; else for no tags
  (interactive)
  (fetch-tags-available-for-set
   set-id
   (lambda (data)
     (setq tags (append data nil))

     (if (null tags)
	 (message "Set is tagged with all available tags - no tags left!")
       (helm :sources (helm-build-sync-source "Select a Tag"
			:candidates (lambda ()
				      (mapcar
				       (lambda (tag)
					 (cons (alist-get 'name tag)
					       (alist-get 'id tag)))
				       tags))
			:action (lambda (selected-tag-id)
				  (if for-yes-tags
				      (update-set
				       set-id
				       (lambda (_) (view-set-details set-id))
				       :yes-ids-to-add (list selected-tag-id))
				    (update-set
				     set-id
				     (lambda (_) (view-set-details set-id))
				     :no-ids-to-add (list selected-tag-id)))))
	     :buffer "*helm-select-tag*")
       ))
   ))

(defun parse-name-text-from-buffer ()
  (save-excursion
    ;; First check if both required labels exist
    (goto-char (point-min))
    (unless (and (search-forward name-label nil t)
                 (search-forward text-label nil t))
      (error (format "Buffer must contain '%s', '%s', '%s', and '%s' labels" name-label text-label yes-tags-label no-tags-label)))
    
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
			      (if (search-forward yes-tags-label nil t)
				  (- (match-beginning 0) 1)  ; Stop right before "Description:"
				(point-max)))))))
      (list name description)
      )
    ))


(defun save-set-name-and-text ()
  "Submit the form and process the data."
  (interactive)
  (let* ((parsed-data (parse-name-text-from-buffer))
         (name (nth 0 parsed-data))
         (description (nth 1 parsed-data)))
    (message "Saving - Name: %s, Description: %s" name description)
    (update-set set-id
                (lambda (response)
                  (let ((set-id (alist-get 'id response)))
		    (message "Set updated successfully: %s" set-id)
		    (view-set-details set-id)))
		:name name
		:text description
		)))



(define-derived-mode set-details-mode fundamental-mode "Set-Details"
  "Major mode for viewing details of a single set."

  (setq-local buffer-read-only nil)

  (when (string-match "*Set \\([0-9]+\\)*" (buffer-name))
    (setq set-id (string-to-number (match-string 1 (buffer-name)))))

  (when (featurep 'evil)
    (message "Setting set details mappings please")
    (evil-define-key 'normal set-details-mode-map (kbd "SPC a y")
      (lambda () (interactive) (add-tag-to-set t)))
    (evil-define-key 'normal set-details-mode-map (kbd "SPC a n")
      (lambda () (interactive) (add-tag-to-set nil)))
    (evil-define-key 'normal set-details-mode-map (kbd "S")
      'save-set-name-and-text)
    )
  )


(defun render-set-details (set)
  "Render the details of SET and its associated TAGS."
  (let ((yes-tags (append (alist-get 'yes_tags set) nil))
	(no-tags (append (alist-get 'no_tags set) nil))
	(things (append (alist-get 'things set) nil)))
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") (alist-get 'name set)))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (alist-get 'text set)))

    (insert (format "%s\n" (format-header "Yes Tags:")))
    (render-tag-tiles
     "set-yes-tags"
     yes-tags
     (lambda (tag-id)
       (update-set
	set-id
	(lambda (_) (view-set-details set-id))
	:yes-ids-to-remove (list tag-id)
	)
       ))

    (insert "\n\n")

    (insert (format "%s\n" (format-header "No Tags:")))
    (render-tag-tiles
     "set-no-tags"
     no-tags
     (lambda (tag-id)
       (update-set
	set-id
	(lambda (_) (view-set-details set-id))
	:no-ids-to-remove (list tag-id)
	)
       ))

    (insert (format "\n\n%s" (format-header "Things:")))

    (if (null things)
	(insert "\n<No things in this set>")
      (dolist (thing things)
	(insert-text-button
	 (format "\n- %s" (alist-get 'name thing))
         'action (lambda (_) (view-thing-details (alist-get 'id thing)))
         'follow-link t
	 'face '(:inherit default)
	 ))
      )

    ))


(defun view-set-details (set-id)
  "Display the details of a set by SET-ID."
  (interactive)
  (message "Here in view-set-details view for set with ID: %d" set-id)
  (let ((buffer (get-buffer-create (format "*Set %d*" set-id))))
    (switch-to-buffer buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Loading set details...\n"))
    (set-details-mode)

    (fetch-set
     set-id
     (lambda (data)
       (with-current-buffer buffer
         (render-set-details data)))
     (lambda (err)
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
	   (erase-buffer)
	   (insert (format-error "Error fetching set\n"))))
       ))))


(provide 'set-details)
