
(message "main.el running")

(require 'thing-list)
(require 'tag-list)
(require 'thing-details)
(require 'tag-details)
(require 'editable-buffer)

(message "main.el loaded all files")



;(with-eval-after-load 'evil
  ;(global-set-key (kbd "SPC t l") 'view-thing-list) ;; List things
  ;(global-set-key (kbd "SPC t d") 'view-thing-details) ;; View a specific thing
  ;(global-set-key (kbd "SPC e n")
                  ;(lambda ()
                    ;(interactive)
                    ;(open-editable-buffer
                     ;"Default content"
                     ;(lambda (content)
                       ;(message "Edited content: %s" content))))))

(defun my-ui-start ()
  "Start the UI app."
  (interactive)
  (view-thing-list)) ;; Start with the thing list view


(provide 'my-org-ui) ;; Makes this the main entry point

(message "loaded and provided my-org-ui")
