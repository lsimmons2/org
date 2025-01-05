;;; editable-buffer.el --- Editable Buffer Component -*- lexical-binding: t -*-

(defun open-editable-buffer (initial-content callback)
  "Open an editable buffer with INITIAL-CONTENT and trigger CALLBACK on C-Enter."
  (let ((buffer (get-buffer-create "*Editable*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert initial-content)
      (local-set-key (kbd "C-<return>") (lambda ()
                                          (interactive)
                                          (funcall callback (buffer-string))
                                          (kill-buffer)))
      (setq buffer-read-only nil))
    (switch-to-buffer buffer)))

(provide 'editable-buffer)
;;; editable-buffer.el ends here
