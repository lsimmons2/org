
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

(defun format-for-tabulated-list-cell (str)
  "Replace all whitespace (including newlines, tabs, and multiple spaces) with a single space."
  (replace-regexp-in-string "[[:space:]]+" " " str))

(defun truncate-string (str max-length)
  "Truncate STR to MAX-LENGTH, adding ellipsis if necessary."
  (if (> (length str) max-length)
      (concat (substring str 0 (- max-length 3)) "…")
    str))

(provide 'utils)


