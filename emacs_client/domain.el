;; -*- lexical-binding: t -*-

(require 'json)
(require 'url)
(require 'cl-lib) ;; For handling closures

(defconst api-base-url "http://localhost:7777")

(defun fetch-things (callback)
  "Fetch a list of things from the API and call CALLBACK with the result."
  (message "Callback value: %S" callback) ;; Log the callback

  (request
    (concat api-base-url "/things")
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
		(message "Callback deep in here...") ;; Log the callback
		(message "%S" callback) ;; Log the callback
		(funcall callback (alist-get 'data data))))
    :error (cl-function
            (lambda (&key error-thrown &allow-other-keys)
              (message "Error fetching things: %s" error-thrown)))))


(defun fetch-tags ()
  "Fetch a list of tags (mocked for now)."
  `((id 1 name "Tag A") (id 2 name "Tag B") (id 3 name "Tag C")))

(defun fetch-thing (id)
  "Fetch a single thing by ID (mocked)."
  `((id ,id name "Thing 1" text "Description of Thing 1")))

(provide 'domain) ;; Makes the API functions available to other files
