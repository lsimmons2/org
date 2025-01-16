
(require 'shared-components)
(require 'domain)


(defun create-thing-handler (name text)
  "Wrapper around the create-set function to match the create-entity-form API."
  (create-thing name text (lambda (response)
			    (let ((thing-id (alist-get 'id response)))
			      (message "Thing created successfully: %s" thing-id)
			      (view-thing-details thing-id)
			      ))))

(defun create-thing-ui ()
  "Show a form for creating a set."
  (interactive)
  (create-entity-form "Thing" #'create-thing-handler))

(provide 'create-thing-form)
