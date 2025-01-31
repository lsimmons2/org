;;; tag-list.el --- Tag List View -*- lexical-binding: t -*-

(defun tag-list-visit-entry ()
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if id
        (view-tag-details id)
      (message "No entry selected!"))))

(defun tag-list-delete-entry ()
  (interactive)
  (message "deleting tag")
  (let (
	(id (tabulated-list-get-id))
	(entry (tabulated-list-get-entry))
	)
    (if (and id entry)
	(view-confirm-screen
	 (format "Delete tag %s?" (aref entry 2))
         (lambda () 
	   (message (format "deleting thing %d" id))
	   (delete-tag id (lambda () (view-tag-list)))
	   )
         (lambda () (message "aborting delete"))
	 )
      (message "No entry selected!"))))

(define-derived-mode tag-list-mode base-list-mode "TagList"
  "Major mode for listing tags."
  (message "creating tag-list-mode!")
  (evil-define-key 'normal tag-list-mode-map (kbd "RET") #'tag-list-visit-entry)
  (evil-define-key 'normal tag-list-mode-map (kbd "d") #'tag-list-delete-entry))

(defun render-tag-list (tags)
  "Render a list of TAGS in the current buffer using a tabulated list."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not tags)
	(insert "*** No tags created yet ***\n")
      (progn
	(setq tabulated-list-format
              [("Id" 15 t)
	       ("Name" 30 t)
               ("Text" 50 nil)])
	(setq tabulated-list-entries
              (mapcar (lambda (tag)
			(let ((id (alist-get 'id tag))
                              (name (alist-get 'name tag))
                              (text (or (alist-get 'text tag) "-")))
			  (list id (vector (number-to-string id) name text))))
                      tags))
	(tabulated-list-init-header)
	(tabulated-list-print t)))
    ))

(defun view-tag-list ()
  "Display a list of tags."
  (interactive)
  (message "rendering tag list!")
  (let ((buffer (get-buffer-create "*Tags*")))
    (with-current-buffer buffer
      (tag-list-mode)

      (fetch-tags
       (lambda (data)
         (with-current-buffer buffer
	   (setq tags (append data nil)) ;; Converts `data` to a list

           (render-tag-list tags)))))
    (switch-to-buffer buffer)))

(provide 'tag-list)
;;; tag-list.el ends here
