;;; thing-list.el --- Thing List View -*- lexical-binding: t -*-


(defun view-thing-list ()
  "Display a list of things."
  (interactive)
  (let ((buffer (get-buffer-create "*Things*")))
    (fetch-things
     (lambda (data)
       (with-current-buffer buffer
	 (setq things (append data nil)) ;; Converts `data` to a list
         (view-entity-list
	  "Things"
	  (list "Id" "Name" "Text" "Created at")
	  things
	  #'view-thing-details
	  (lambda (id)
            (delete-thing id (lambda () (view-thing-list))))
	  ))))
    (switch-to-buffer buffer)))

(provide 'thing-list)
