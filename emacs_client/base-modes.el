;;; base-special-mode.el --- Base mode for custom views -*- lexical-binding: t -*-

(define-derived-mode base-list-mode special-mode "OrgBaseList"
  "Base mode for all custom list views with shared behaviors."
  ;; Enable line highlighting
  (hl-line-mode 1))


(provide 'base-modes)
