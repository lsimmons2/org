;;; tag-details.el --- Tag Details View -*- lexical-binding: t -*-

(define-derived-mode tag-details-mode special-mode "Tag-Details"
  "Major mode for viewing details of a single tag.")

(defun render-tag-details (tag)
  "Render the details of TAG and its associated TAGS."
  (let ((inhibit-read-only t)) ;; Temporarily disable read-only mode
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") (alist-get 'name tag)))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (alist-get 'text tag)))
    )
  )

(defun view-tag-details (tag-id)
  "Display the details of a tag by TAG-ID."
  (interactive)
  (message "Here in view-tag-details view for tag with ID: %d" tag-id)
  (let ((buffer (get-buffer-create (format "*Tag %d*" tag-id))))
    (fetch-tag
     tag-id
     (lambda (data)
       (with-current-buffer buffer
	 (tag-details-mode)
	 (setq tag (append data nil)) ;; Converts `data` to a list
         (render-tag-details tag)
	 )))
    (switch-to-buffer buffer)))

(provide 'tag-details)
;;; tag-details.el ends here
