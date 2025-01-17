;; -*- lexical-binding: t; -*-

(require 'domain)
(require 'base-modes)


(defun render-set-list (sets)
  "Render a list of SETS in the current buffer using a tabulated list."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not sets)
	(insert "*** No sets created yet ***\n")
      (progn
	(setq tabulated-list-format
              [("Id" 5 t)
               ("Name" 30 t)
               ("Text" 50 nil)])
	(setq tabulated-list-entries
              (mapcar (lambda (set)
			(let ((id (alist-get 'id set))
                              (name (alist-get 'name set))
                              (text (or (alist-get 'text set) "-")))
			  (list id (vector (number-to-string id) name text))))
                      sets))
	(tabulated-list-init-header)
	(tabulated-list-print t)))
    ))




(defun set-list-visit-entry ()
  "Open the set details page for the current line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if id
        (view-set-details id)
      (message "No entry selected!"))))


(defun set-list-delete-entry ()
  (interactive)
  (message "deleting set")
  (let (
	(id (tabulated-list-get-id))
	(entry (tabulated-list-get-entry))
	)
    (if (and id entry)
	(view-confirm-screen
	 (format "Delete set %s?" (aref entry 2))
         (lambda () 
	   (message (format "deleting set %d" id))
	   (delete-set id (lambda () (view-set-list)))
	   )
         (lambda () (message "aborting delete"))
	 )
      (message "No entry selected!"))))


(define-derived-mode set-list-mode base-list-mode "SetList"
  "Mode for viewing a list of sets."
  (evil-define-key 'normal set-list-mode-map (kbd "RET") #'set-list-visit-entry)
  (evil-define-key 'normal set-list-mode-map (kbd "d") #'set-list-delete-entry))


(defun view-set-list ()
  "Display a list of sets."
  (interactive)
  (let ((buffer (get-buffer-create "*Sets*")))
    (fetch-sets
     (lambda (data)
       (with-current-buffer buffer
	 (set-list-mode)
	 (let ((sets (append data nil)))
	   (render-set-list sets))
         )))
    (switch-to-buffer buffer)))

(provide 'set-list)
