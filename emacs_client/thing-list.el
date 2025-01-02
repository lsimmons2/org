;;; thing-list.el --- Thing List View -*- lexical-binding: t -*-

(require 'api) ;; Import API functions to fetch things

(define-derived-mode thing-list-mode special-mode "Thing-List"
  "Major mode for listing things.")

(defun render-thing-list (things)
  "Render a list of THINGS in the current buffer."
  (erase-buffer)
  (dolist (thing things)
    (insert-text-button
     (format "Thing: %s\n" (alist-get 'name thing))
     'action (lambda (_) (view-thing-details (alist-get 'id thing)))
     'follow-link t)))

(defun view-thing-list ()
  "Display a list of things."
  (interactive)
  (let ((buffer (get-buffer-create "*Things*")))
    (with-current-buffer buffer
      (thing-list-mode)
      (render-thing-list (fetch-things))) ;; Call the API function
    (switch-to-buffer buffer)))

(provide 'thing-list) ;; Makes this file's features available to others
;;; thing-list.el ends here
