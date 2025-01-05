;;; tag-details.el --- Tag Details View -*- lexical-binding: t -*-

(require 'domain)

(define-derived-mode tag-details-mode special-mode "Tag-Details"
  "Major mode for viewing details of a single tag.")

(defun render-tag-details (tag things)
  "Render the details of TAG and its associated THINGS."
  (erase-buffer)
  (insert (format "Tag: %s\n\n" (alist-get 'name tag)))
  (insert "Things:\n")
  (dolist (thing things)
    (insert (format "[%s] " (alist-get 'name thing)))))

(defun view-tag-details (tag-id)
  "Display the details of a tag by TAG-ID."
  (interactive)
  (let* ((tag (fetch-tag tag-id))         ;; Fetch from API
         (things (fetch-things-for-tag tag-id)) ;; Fetch from API
         (buffer (get-buffer-create "*Tag Details*")))
    (with-current-buffer buffer
      (tag-details-mode)
      (render-tag-details tag things))
    (switch-to-buffer buffer)))

(provide 'tag-details)
;;; tag-details.el ends here
