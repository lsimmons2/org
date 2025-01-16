;; -*- lexical-binding: t; -*-


(require 'domain)
(require 'utils)

(defun set-details-visit-tag ()
  "Visit the tag under the cursor."
  (interactive)
  (let ((tag-id (get-text-property (point) 'tag-id))) ;; Retrieve `tag-id` at point
    (if tag-id
        (view-tag-details tag-id)
      (message "No tag under the cursor.")))) ;; Fallback message

(define-derived-mode set-details-mode special-mode "Set-Details"
  "Major mode for viewing details of a single set."
  (define-key set-details-mode-map (kbd "RET") #'set-details-visit-tag))


(defun render-set-details (set)
  "Render the details of SET and its associated TAGS."
  (let ((inhibit-read-only t)) ;; Temporarily disable read-only mode
    (erase-buffer)
    (insert (format "%s\n%s\n\n" (format-header "Name:") (alist-get 'name set)))
    (insert (format "%s\n%s\n\n" (format-header "Description:") (or (alist-get 'text set) "-")))

    ;; (insert (format "%s\n" (format-header "Yes Tags:")))
    ;; (dolist (tag (append (alist-get 'tags set) nil))
    ;;   (let ((tag-name (alist-get 'name tag))
    ;;         (tag-id (alist-get 'id tag)))
    ;;     ;; Add text properties to associate the tag ID with the text
    ;;     (insert-text-button
    ;;      (format "[%s] " tag-name)
    ;;      'action (lambda (_) (view-tag-details tag-id)) ;; Clickable action
    ;;      'follow-link t
    ;;      'tag-id tag-id)))) ;; Add `tag-id` as a text property

    ;; (insert (format "%s\n" (format-header "No Tags:")))
    ;; (dolist (tag (append (alist-get 'tags set) nil))
    ;;   (let ((tag-name (alist-get 'name tag))
    ;;         (tag-id (alist-get 'id tag)))
    ;;     ;; Add text properties to associate the tag ID with the text
    ;;     (insert-text-button
    ;;      (format "[%s] " tag-name)
    ;;      'action (lambda (_) (view-tag-details tag-id)) ;; Clickable action
    ;;      'follow-link t
    ;;      'tag-id tag-id)))) ;; Add `tag-id` as a text property

    ))


(defun view-set-details (set-id)
  "Display the details of a set by SET-ID."
  (interactive)
  (let ((buffer (get-buffer-create (format "*Set %d*" set-id))))
    (fetch-set
     set-id
     (lambda (data)
       (with-current-buffer buffer
	 (set-details-mode)
	 (let ((set (append data nil)))
           (render-set-details set))
	 )))
    (switch-to-buffer buffer)))


(provide 'set-details)
