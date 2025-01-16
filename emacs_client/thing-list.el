;;; thing-list.el --- Thing List View -*- lexical-binding: t -*-

(require 'domain)
(require 'base-modes)



(defun render-thing-list (things)
  "Render a list of THINGS in the current buffer using a tabulated list."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if (not things)
	(insert "*** No things created yet ***\n")
      (progn
	(setq tabulated-list-format
              [("Id" 15 t)
	       ("Name" 30 t)
               ("Text" 50 nil)])
	(setq tabulated-list-entries
              (mapcar (lambda (thing)
			(let ((id (alist-get 'id thing))
                              (name (alist-get 'name thing))
                              (text (or (alist-get 'text thing) "-")))
			  (list id (vector (number-to-string id) name text))))
                      things))
	(tabulated-list-init-header)
	(tabulated-list-print t)))
    ))




(defun thing-list-visit-entry ()
  "Open the thing details page for the current line."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if id
        (view-thing-details id)
      (message "No entry selected!"))))


(define-derived-mode thing-list-mode base-list-mode "ThingList"
  "Mode for viewing a list of things."
  (evil-define-key 'normal thing-list-mode-map (kbd "RET") #'thing-list-visit-entry))


(defun view-thing-list ()
  "Display a list of things."
  (interactive)
  (message "rendering thing list new!!!")
  (let ((buffer (get-buffer-create "*Things*")))
    (fetch-things
     (lambda (data)
       (with-current-buffer buffer
	 (thing-list-mode)
	 (setq things (append data nil)) ;; Converts `data` to a list

         (render-thing-list things))))
    (switch-to-buffer buffer)))

(provide 'thing-list)
