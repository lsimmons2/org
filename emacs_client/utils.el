
(defun format-header (text)
  "Format TEXT to be bold and styled like a programming keyword."
  (propertize text 'face '(:weight bold :inherit font-lock-keyword-face)))

(provide 'utils)
