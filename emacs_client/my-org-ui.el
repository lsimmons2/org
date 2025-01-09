
(message "main.el running")

(require 'thing-list)
(require 'tag-list)
(require 'thing-details)
(require 'tag-details)
(require 'editable-buffer)

(message "main.el loaded all files")



(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC h i") 'view-thing-list)
  (evil-define-key 'normal 'global (kbd "SPC h a") 'view-tag-list)
  ;; (global-set-key (kbd "SPC h i") 'view-thing-list) ;; List things
  ;; (global-set-key (kbd "SPC h a") 'view-tag-list) ;; View a specific thing
  ;; (global-set-key (kbd "SPC e n")
  ;;                 (lambda ()
  ;;                   (interactive)
  ;;                   (open-editable-buffer
  ;;                    "Default content"
  ;;                    (lambda (content)
  ;;                      (message "Edited content: %s" content)))))
  )

(defun my-ui-start ()
  (interactive)
  (view-thing-list))


(provide 'my-org-ui)

(message "loaded and provided my-org-ui")
