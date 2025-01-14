(require 'shared-components)
(require 'domain)


(defun create-set-handler (name text callback)
  "Wrapper around the create-set function to match the create-entity-form API."
  (create-set name text callback))

(defun create-set-ui ()
  "Show a form for creating a set."
  (interactive)
  (create-entity-form "Set" #'create-set-handler))

(provide 'create-set-form)
