;;; -*- lexical-binding: t -*-

(require 'domain)


(defconst name-label "Name:")
(defconst text-label "Description:")

(defun new-thing-parse-buffer ()
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
      )
    ))


(defun new-thing-submit ()
  "Submit the form and process the data."
  (interactive)
  (let* ((form-buffer (current-buffer)) ;; Save the current form buffer
         (parsed-data (new-thing-parse-buffer))
         (name (nth 0 parsed-data))
         (description (nth 1 parsed-data)))
    (create-thing name description
                  (lambda (response)
                    (let ((thing-id (alist-get 'id response)))
                      (view-thing-details thing-id)
                      (when (buffer-live-p form-buffer) ;; Ensure the buffer still exists
                        (kill-buffer form-buffer)))))))

(define-derived-mode create-thing-mode fundamental-mode "New Thing"
  "Major mode for creating new things."
  (when (featurep 'evil)
    (evil-define-key 'normal create-thing-mode-map
      (kbd "S") 'new-thing-submit))

  ;; Set up any mode-specific variables and syntax tables here
  (setq-local buffer-read-only nil)
  
  ;; Insert the form template
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format "%s\n\n%s\n" (format-header name-label) (format-header text-label))))
  
  ;; Move cursor to after the Name: label
  (goto-char (point-min))
  (forward-line 1)
  
  ;; Set up evil-mode keybinding for submit
  ;; (evil-local-set-key 'normal "S" 'new-thing-submit)
  )

(defun create-new-thing ()
  "Create a new buffer with the new-thing-mode."
  (interactive)
  (let ((buffer (get-buffer-create "* Create New Thing *")))
    (switch-to-buffer buffer)
    (create-thing-mode)))


(provide 'create-thing-form)
