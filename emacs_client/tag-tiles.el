;; -*- lexical-binding: t -*-


(defun update-tag-keybinding (tag-group-id on-remove)
  "Enable or disable the 'x' binding based on cursor position."
  (let ((tag-id (get-text-property (point) 'tag-id))
	(hovered-tag-group-id (get-text-property (point) 'tag-group-id)))
    (if (and tag-id (string= hovered-tag-group-id tag-group-id))
	;; If we're on a tag, set the keybindings
	(progn
	  (message (format "adding keybindings to tag %d" tag-id))
          (evil-local-set-key 'normal (kbd "x")
			      (lambda ()
				(interactive)
				(message "Removing tag %d" tag-id)
				(funcall on-remove tag-id)))
	  (evil-local-set-key 'normal (kbd "RET")
			      (lambda ()
				(interactive)
				(message "Navigating to tag %d" tag-id)
				(view-tag-details tag-id)))
	  )
      )))


(defun render-tag-tiles (tag-group-id tags on-remove)

  (add-hook 'post-command-hook (lambda () (update-tag-keybinding tag-group-id on-remove)) nil t)

  (if (null tags)
      (insert "<No tags>")
    (progn
      (dolist (tag tags)
	(let ((tag-name (alist-get 'name tag))
              (tag-id (alist-get 'id tag)))
          (insert-text-button
           (format "[%s]" tag-name)
           'action (lambda (_) (view-tag-details tag-id)) ;; Customizable action
	   'face '(:inherit default)
           'follow-link t
           'tag-id tag-id ;; Add `tag-id` as a text property
	   'tag-group-id tag-group-id
	   ))
	;; Insert separator between tags, but not after the last one
	(unless (eq tag (car (last tags)))
          (insert " "))))
    ))

(provide 'tag-tiles)
