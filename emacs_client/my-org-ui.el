
(message "main.el running")

(require 'set-list)
(require 'set-details)
(require 'create-set-form)

(require 'thing-list)
(require 'thing-details)
(require 'create-thing-form)

(require 'tag-list)
(require 'tag-details)
(require 'create-tag-form)

(require 'editable-buffer)
(require 'confirm-screen)

(message "main.el loaded all files")

(with-eval-after-load 'evil
  (add-hook 'my-mode-hook
            (lambda ()
              (evil-local-set-key 'normal (kbd "S") 'my-special-save-function))))

(with-eval-after-load 'evil
  (evil-define-key 'normal 'global (kbd "SPC h i") 'view-thing-list)
  (evil-define-key 'normal 'global (kbd "SPC h a") 'view-tag-list)
  (evil-define-key 'normal 'global (kbd "SPC h s") 'view-set-list)

  (evil-define-key 'normal 'global (kbd "SPC n s") 'create-new-set)
  (evil-define-key 'normal 'global (kbd "SPC n i") 'create-new-thing)
  (evil-define-key 'normal 'global (kbd "SPC n a") 'create-new-tag)
  )

(defun my-ui-start ()
  (interactive)
  (view-thing-list))


(provide 'my-org-ui)

(message "loaded and provided my-org-ui")
