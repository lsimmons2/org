

(require 'shared-components)
(require 'domain)


(defun create-tag-handler (name text)
  "Wrapper around the create-set function to match the create-entity-form API."
  (create-tag name text (lambda (response)
			  (let ((tag-id (alist-get 'id response)))
			    (message "Tag created successfully: %s" tag-id)
			    (view-tag-details tag-id)))))


(defun create-tag-ui ()
  "Show a form for creating a set."
  (interactive)
  (create-entity-form "Tag" #'create-tag-handler))


(provide 'create-tag-form)
