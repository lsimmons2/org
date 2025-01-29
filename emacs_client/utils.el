
(defun format-header (text)
  "Format TEXT to be bold and styled like a programming keyword."
  (propertize text
	      'font-lock-face '(:weight bold :inherit font-lock-keyword-face)
	      'rear-nonsticky t
	      ;; 'line-prefix t
	      ))

(defun format-error (text)
  "Format TEXT to be styled like an e.g. Flycheck error"
  (propertize text 'face 'error))

(provide 'utils)
