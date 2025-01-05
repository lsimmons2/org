;;; base-special-mode.el --- Base mode for custom views -*- lexical-binding: t -*-

(define-derived-mode base-list-mode special-mode "OrgBaseList"
  "Base mode for all custom list views with shared behaviors."
  ;; Enable line highlighting
  (message "in base-list-mode!!!!!!!!")
  (hl-line-mode 1))


(provide 'base-modes)
