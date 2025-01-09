;;; thing-details.el --- Thing Details View -*- lexical-binding: t -*-

(defun format-header (text)
  "Format TEXT to be bold and styled like a programming keyword."
  (propertize text 'face '(:weight bold :inherit font-lock-keyword-face)))

(require 'domain)

(define-derived-mode thing-details-mode special-mode "Thing-Details"
  "Major mode for viewing details of a single thing.")

(defun render-thing-details (thing)
  "Render the details of THING and its associated TAGS."
  (let ((inhibit-read-only t)) ;; Temporarily disable read-only mode
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") (alist-get 'name thing)))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (alist-get 'text thing)))
    (insert (format "%s\n" (format-header "Tags:")))
    (dolist (tag (append (alist-get 'tags thing) nil))
      (insert (format "[%s] " (alist-get 'name tag))))
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
