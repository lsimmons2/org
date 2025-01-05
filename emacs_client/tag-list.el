;;; tag-list.el --- Tag List View -*- lexical-binding: t -*-

(require 'domain)

(define-derived-mode tag-list-mode special-mode "Tag-List"
  "Major mode for listing tags.")

(defun render-tag-list (tags)
  "Render a list of TAGS in the current buffer."
  (erase-buffer)
  (dolist (tag tags)
    (insert-text-button
     (format "Tag: %s\n" (alist-get 'name tag))
     'action (lambda (_) (view-tag-details (alist-get 'id tag)))
     'follow-link t)))

(defun view-tag-list ()
  "Display a list of tags."
  (interactive)
  (let ((buffer (get-buffer-create "*Tags*")))
    (with-current-buffer buffer
      (tag-list-mode)
      (render-tag-list (fetch-tags))) ;; Fetch from API
    (switch-to-buffer buffer)))

(provide 'tag-list)
;;; tag-list.el ends here
