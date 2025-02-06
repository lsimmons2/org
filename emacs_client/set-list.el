;; -*- lexical-binding: t; -*-


(defun view-set-list ()
  "Display a list of sets."
  (interactive)
  (let ((buffer (get-buffer-create "*Sets*")))
    (fetch-sets
     (lambda (data)
       (with-current-buffer buffer
	 (setq sets (append data nil)) ;; Converts `data` to a list
         (view-entity-list
	  "Sets"
	  (list "Id" "Name" "Text")
	  sets
	  #'view-set-details
	  (lambda (id)
            (delete-set id (lambda () (view-set-list))))
	  ))))
    (switch-to-buffer buffer)))

(provide 'set-list)
