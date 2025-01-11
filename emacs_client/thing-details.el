;;; thing-details.el --- Thing Details View -*- lexical-binding: t -*-


(require 'domain)
(require 'utils)

(defun thing-details-visit-tag ()
  "Visit the tag under the cursor."
  (interactive)
  (let ((tag-id (get-text-property (point) 'tag-id))) ;; Retrieve `tag-id` at point
    (if tag-id
        (view-tag-details tag-id)
      (message "No tag under the cursor.")))) ;; Fallback message

(define-derived-mode thing-details-mode special-mode "Thing-Details"
  "Major mode for viewing details of a single thing."
  (define-key thing-details-mode-map (kbd "RET") #'thing-details-visit-tag))


(defun render-thing-details (thing)
  "Render the details of THING and its associated TAGS."
  (let ((inhibit-read-only t)) ;; Temporarily disable read-only mode
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") (alist-get 'name thing)))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (alist-get 'text thing)))
    (insert (format "%s\n" (format-header "Tags:")))
    (dolist (tag (append (alist-get 'tags thing) nil))
      (let ((tag-name (alist-get 'name tag))
            (tag-id (alist-get 'id tag)))
        ;; Add text properties to associate the tag ID with the text
        (insert-text-button
         (format "[%s] " tag-name)
         'action (lambda (_) (view-tag-details tag-id)) ;; Clickable action
         'follow-link t
         'tag-id tag-id)))) ;; Add `tag-id` as a text property
  )
)

(defun view-thing-details (thing-id)
  "Display the details of a thing by THING-ID."
  (interactive)
  (message "Here in view-thing-details view for thing with ID: %d" thing-id)
  (let ((buffer (get-buffer-create (format "*Thinggg %d*" thing-id))))
    (fetch-thing
     thing-id
     (lambda (data)
       (with-current-buffer buffer
	 (thing-details-mode)
	 (setq thing (append data nil)) ;; Converts `data` to a list
         (render-thing-details thing)
	 )))
    (switch-to-buffer buffer)))

(provide 'thing-details)
;;; thing-details.el ends here
