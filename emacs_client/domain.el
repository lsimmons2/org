
(defun fetch-things ()
  "Fetch a list of things (mocked for now)."
  `((id 1 name "Thing 1") (id 2 name "Thing 2") (id 3 name "Thing 3")))

(defun fetch-tags ()
  "Fetch a list of tags (mocked for now)."
  `((id 1 name "Tag A") (id 2 name "Tag B") (id 3 name "Tag C")))

(defun fetch-thing (id)
  "Fetch a single thing by ID (mocked)."
  `((id ,id name "Thing 1" text "Description of Thing 1")))

(provide 'domain) ;; Makes the API functions available to other files
