;; -*- lexical-binding: t -*-

(require 'json)
(require 'url)
(require 'cl-lib) ;; For handling closures

(defconst api-base-url "http://localhost:7777")

(defun create-set (name text callback)
  " create set with NAME and TEXT and CALLBACK" 
  (request
    (concat api-base-url "/sets")
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode `(("name" . ,name) ("text" . ,text)))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (let ((response-body (request-response-data response)))
		(message "Error creating set: %s" error-thrown)
		(message "Server response: %s" response-body))))))

(defun fetch-things (callback)
  "Fetch a list of things from the API and call CALLBACK with the result."

  (request
    (concat api-base-url "/things")
    ;; this is a plist - keys and values alternate - nb don't see ((foo . bar)...) syntax
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching things: %s" error-thrown)))))


(defun fetch-tags (callback)

  (request
    (concat api-base-url "/tags")
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching tags: %s" error-thrown)))))


(defun fetch-thing (id callback)
  (request
    (concat api-base-url (format "/things/%d" id))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching thing: %s" error-thrown)))))


(defun fetch-tag (id callback)
  (request
    (concat api-base-url (format "/tags/%d" id))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching thing: %s" error-thrown)))))

(provide 'domain) ;; Makes the API functions available to other files
