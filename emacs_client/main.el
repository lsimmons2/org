
(require 'thing-list)
(require 'tag-list)
(require 'thing-details)
(require 'tag-details)
(require 'editable-buffer)

;; Global Keybindings
(global-set-key (kbd "SPC t l") 'view-thing-list) ;; List things
(global-set-key (kbd "SPC t d") 'view-thing-details) ;; View a specific thing
(global-set-key (kbd "SPC e n")
                (lambda ()
                  (interactive)
                  (open-editable-buffer
                   "Default content"
                   (lambda (content)
                     (message "Edited content: %s" content)))))

(provide 'my-ui) ;; Makes this the main entry point
