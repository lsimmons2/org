;;; tag-details.el --- Tag Details View -*- lexical-binding: t -*-

(define-derived-mode tag-details-mode special-mode "Tag-Details"
  "Major mode for viewing details of a single tag.")

(defun render-tag-details (tag)
  "Render the details of TAG and its associated TAGS."
  (let ((inhibit-read-only t)
	(tag-name (alist-get 'name tag))
	(tagged-things (append (alist-get 'things tag) nil))) ;; Temporarily disable read-only mode
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") tag-name))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (alist-get 'text tag)))
    (insert (format "%s" (format-header (format "Things tagged with \"%s\":" tag-name))))
    (if (null tagged-things)
	(insert "\n<No things tagged with this tag yet>")
      (progn
	;; TODO: this repeated in set-details - trying it out for a bit before I make abstraction
	(dolist (tagged-thing tagged-things)
	  (insert-text-button
	   (format "\n[%s]" (alist-get 'name tagged-thing))
           'action (lambda (_) (view-thing-details (alist-get 'id tagged-thing)))
           'follow-link t
	   'face '(:inherit default))
	  )
	))
    ))

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
