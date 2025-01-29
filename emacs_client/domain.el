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

(defun create-thing (name text callback)
  " create thing with NAME and TEXT and CALLBACK" 
  (request
    (concat api-base-url "/things")
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
		(message "Error creating thing: %s" error-thrown)
		(message "Server response: %s" response-body))))))

(defun update-thing (thing-id name text callback)
  " create thing with NAME and TEXT and CALLBACK" 
  (request
    (concat api-base-url (format "/things/%d" thing-id))
    :type "PUT"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode `(("name" . ,name) ("text" . ,text)))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (let ((response-body (request-response-data response)))
		(message "Error updating thing: %s" error-thrown)
		(message "Server response: %s" response-body))))))

(defun update-set (set-id callback &rest args)
  "Update the set with the given SET-ID, SUCCESS-CB, and ARGS.
ARGS should be specified as keyword arguments:
  :name             The name of the set (optional).
  :text             The description or text for the set (optional).
  :yes-ids-to-add   List of IDs to add to 'yes' (optional).
  :yes-ids-to-remove List of IDs to remove from 'yes' (optional).
  :no-ids-to-add    List of IDs to add to 'no' (optional).
  :no-ids-to-remove List of IDs to remove from 'no' (optional)."
  (let* ((payload (delq nil
                        `(("name" . ,(plist-get args :name))
                          ("text" . ,(plist-get args :text))
                          ("yes_ids_to_add" . ,(plist-get args :yes-ids-to-add))
                          ("yes_ids_to_remove" . ,(plist-get args :yes-ids-to-remove))
                          ("no_ids_to_add" . ,(plist-get args :no-ids-to-add))
                          ("no_ids_to_remove" . ,(plist-get args :no-ids-to-remove))))))

    (request
      (concat api-base-url (format "/sets/%d" set-id))
      :type "PUT"
      :headers '(("Content-Type" . "application/json"))
      :data (json-encode payload)
      :parser 'json-read
      :success (cl-function
		(lambda (&key data &allow-other-keys)
                  (funcall callback (alist-get 'data data))))
      :error (cl-function
              (lambda (&key error-thrown response &allow-other-keys)
		(let ((response-body (request-response-data response)))
                  (message "Error updating set: %s" error-thrown)
                  (message "Server response: %s" response-body)))))))

(defun create-tag (name text callback)
  " create tag with NAME and TEXT and CALLBACK" 
  (request
    (concat api-base-url "/tags")
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
		(message "Error creating tag: %s" error-thrown)
		(message "Server response: %s" response-body))))))


(defun tag-thing (tag-id thing-id callback)
  " create tag-thing association with THING-ID and TAG-ID and CALLBACK" 
  (request
    (concat api-base-url "/tag-to-thing")
    :type "POST"
    :headers '(("Content-Type" . "application/json"))
    :data (json-encode `(("tag_id" . ,tag-id) ("thing_id" . ,thing-id)))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (let ((response-body (request-response-data response)))
		(message "Error tagging thing: %s" error-thrown)
		(message "Server response: %s" response-body))))))


(defun untag-thing (tag-id thing-id callback)
  " create tag-thing association with THING-ID and TAG-ID and CALLBACK" 
  (request
    (concat api-base-url (format "/things/%d/tags/%d" thing-id tag-id))
    :type "DELETE"
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
            (lambda (&key error-thrown response &allow-other-keys)
              (let ((response-body (request-response-data response)))
		(message "Error tagging thing: %s" error-thrown)
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


(defun fetch-sets (callback)
  (request
    (concat api-base-url "/sets")
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching sets: %s" error-thrown)))))


(defun fetch-set (id success-cb error-cb)
  (request
    (concat api-base-url (format "/sets/%d" id))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall success-cb (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching set: %s" error-thrown)
	      (funcall error-cb error-thrown)))))

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


(defun fetch-tags-available-for-thing (thing-id callback)
  (request
    (concat api-base-url (format "/things/%d/available-tags" thing-id))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching tags: %s" error-thrown)))))


(defun fetch-tags-available-for-set (set-id callback)
  (request
    (concat api-base-url (format "/sets/%d/available-tags" set-id))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall callback (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching tags: %s" error-thrown)))))

(defun fetch-thing (id success-cb error-cb)
  (request
    (concat api-base-url (format "/things/%d" id))
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall success-cb (alist-get 'data data))))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching thing: %s" error-thrown)
	      (funcall error-cb error-thrown)))))


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


(defun delete-thing (id callback)
  (request
    (concat api-base-url (format "/things/%d" id))
    :type "DELETE"
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys) (funcall callback)))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error fetching thing: %s" error-thrown)))))


(defun delete-tag (id callback)
  (request
    (concat api-base-url (format "/tags/%d" id))
    :type "DELETE"
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys) (funcall callback)))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error deleting tag: %s" error-thrown)))))

(defun delete-set (id callback)
  (request
    (concat api-base-url (format "/sets/%d" id))
    :type "DELETE"
    :headers '(("Content-Type" . "application/json"))
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys) (funcall callback)))
    :error (cl-function
	    (lambda (&key error-thrown &allow-other-keys)
	      (message "Error deleting set: %s" error-thrown)))))

(provide 'domain) ;; Makes the API functions available to other files
