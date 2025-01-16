(require 'shared-components)
(require 'domain)


(defun create-set-handler (name text)
  "Wrapper around the create-set function to match the create-entity-form API."
  (create-set name text (lambda (response)
			  (let ((set-id (alist-get 'id response)))
			    (message "Set created successfully: %s" set-id)
			    (view-set-details set-id)
			    ))))

(defun create-set-ui ()
  "Show a form for creating a set."
  (interactive)
  (create-entity-form "Set" #'create-set-handler))

(provide 'create-set-form)
