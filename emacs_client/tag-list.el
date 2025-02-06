;;; tag-list.el --- Tag List View -*- lexical-binding: t -*-


(defun view-tag-list ()
  "Display a list of tags."
  (interactive)
  (let ((buffer (get-buffer-create "*Tags*")))
    (fetch-tags
     (lambda (data)
       (with-current-buffer buffer
	 (setq tags (append data nil)) ;; Converts `data` to a list
         (view-entity-list
	  "Tags"
	  (list "Id" "Name" "Text")
	  tags
	  #'view-tag-details
	  (lambda (id)
            (delete-tag id (lambda () (view-tag-list))))
	  ))))
    (switch-to-buffer buffer)))

(provide 'tag-list)
;;; tag-list.el ends here
