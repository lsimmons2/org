;;; -*- lexical-binding: t -*-


(require 'domain)


(defconst name-label "Name:")
(defconst text-label "Description:")

(defun new-tag-parse-buffer ()
  "Parse the current buffer to extract name and description."
  (save-excursion
    ;; First check if both required labels exist
    (goto-char (point-min))
    (unless (and (search-forward name-label nil t)
                 (search-forward text-label nil t))
      (error (format "Buffer must contain both '%s' and '%s' labels" name-label text-label)))
    
    ;; If we get here, both labels exist, so let's parse
    (let (name description)
      (goto-char (point-min))
      (when (search-forward name-label nil t)
        (setq name (string-trim
                    (buffer-substring-no-properties
                     (point)  ; Start right after "Name:"
                     (progn
                       (if (search-forward text-label nil t)
                           (- (match-beginning 0) 1)  ; Stop right before "Description:"
                         (point-max)))))))  ; Or take until end of buffer if no Description label
      
      (goto-char (point-min))  ; Go back to start to search for Description
      (when (search-forward text-label nil t)
        (setq description (string-trim
                           (buffer-substring-no-properties
                            (point)  ; Start right after "Description:"
                            (point-max)))))
      (list name description)
      )))


(defun new-tag-submit ()
  "Submit the form and process the data."
  (interactive)
  (let* ((form-buffer (current-buffer)) ;; Save the current form buffer
         (parsed-data (new-tag-parse-buffer))
         (name (nth 0 parsed-data))
         (description (nth 1 parsed-data)))
    (create-tag name description
                (lambda (response)
                  (let ((tag-id (alist-get 'id response)))
                    (view-tag-details tag-id)
                    (when (buffer-live-p form-buffer) ;; Ensure the buffer still exists
                      (kill-buffer form-buffer)))))))


(define-derived-mode create-tag-mode fundamental-mode "New Tag"
  "Major mode for creating new tags."
  (when (featurep 'evil)
    (evil-define-key 'normal create-tag-mode-map
      (kbd "S") 'new-tag-submit))

  ;; Set up any mode-specific variables and syntax tables here
  (setq-local buffer-read-only nil)
  
  ;; Insert the form template
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n\n%s\n" (format-header name-label) (format-header text-label))))
  
  ;; Move cursor to after the Name: label
  (goto-char (point-min))
  (forward-line 1)
  
  )

(defun create-new-tag ()
  "Create a new buffer with the new-tag-mode."
  (interactive)
  (let ((buffer (get-buffer-create "* Create New Tag *")))
    (switch-to-buffer buffer)
    (create-tag-mode)))


(provide 'create-tag-form)
