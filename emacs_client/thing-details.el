;;; thing-details.el --- Thing Details View -*- lexical-binding: t -*-

(require 'domain)

(define-derived-mode thing-details-mode special-mode "Thing-Details"
  "Major mode for viewing details of a single thing.")

(defun render-thing-details (thing tags)
  "Render the details of THING and its associated TAGS."
  (erase-buffer)
  (insert (format "Thing: %s\n\n" (alist-get 'name thing)))
  (insert "Tags:\n")
  (dolist (tag tags)
    (insert (format "[%s] " (alist-get 'name tag)))))

(defun view-thing-details (thing-id)
  "Display the details of a thing by THING-ID."
  (interactive)
  (let* ((thing (fetch-thing thing-id))         ;; Fetch from API
         (tags (fetch-tags-for-thing thing-id)) ;; Fetch from API
         (buffer (get-buffer-create "*Thing Details*")))
    (with-current-buffer buffer
      (thing-details-mode)
      (render-thing-details thing tags))
    (switch-to-buffer buffer)))

(provide 'thing-details)
;;; thing-details.el ends here
