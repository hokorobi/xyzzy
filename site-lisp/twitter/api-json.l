;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is not part of xyzzy.
;;;
; $Id: api.l 813 2008-07-02 04:11:30Z torihat $
;
; twitter/api-json.l
;
; by HATTORI Masashi

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "twitter/api")
  (require "json"))

(provide "twitter/api-json")

(in-package "twitter")

(setq *error-filename* "error.json")

(defun parse-file (file)
  (json:json-decode-file file))

(defun parse-twstatus (data)
  (let (status)
    (setq status (make-twstatus))
    (dolist (c data)
      (let ((cn (car c))
	    (cc (cdr c)))
	(cond ((equal cn "created_at")
	       (setf (twstatus-created_at status) (parse-twdate cc))
	       )
	      ((equal cn "id")
	       (when cc
		 (setf (twstatus-id status) cc))
	       )
	      ((equal cn "text")
	       (setf (twstatus-text status) cc)
	       )
	      ((equal cn "source")
	       (setf (twstatus-source status) cc)
	       (setf (twstatus-source_name status) (parse-twsourcename cc))
	       (setf (twstatus-source_url status) (parse-twsourceurl cc))
	       )
	      ((equal cn "truncated")
	       (setf (twstatus-truncated status) cc)
	       )
	      ((equal cn "user")
	       (setf (twstatus-user status) (parse-twuser cc))
	       )
	      )))
    status))

(defun parse-twdirect (data)
  (let (status)
    (setq status (make-twdirect))
    (dolist (c data)
      (let ((cn (car c))
	    (cc (cdr c)))
	(cond ((equal cn "created_at")
	       (setf (twdirect-created_at status) (parse-twdate cc))
	       )
	      ((equal cn "id")
	       (when cc
		 (setf (twdirect-id status) cc))
	       )
	      ((equal cn "sender_id")
	       (when cc
		 (setf (twdirect-sender_id status) cc))
	       )
	      ((equal cn "recipient_id")
	       (when cc
		 (setf (twdirect-recipient_id status) cc))
	       )
	      ((equal cn "text")
	       (setf (twdirect-text status) cc)
	       )
	      ((equal cn "sender_screen_name")
	       (setf (twdirect-sender_screen_name status) cc)
	       )
	      ((equal cn "recipient_screen_name")
	       (setf (twdirect-recipient_screen_name status) cc)
	       )
	      ((equal cn "sender")
	       (setf (twdirect-sender status) (parse-twuser cc cn))
	       )
	      ((equal cn "recipient")
	       (setf (twdirect-recipient status) (parse-twuser cc cn))
	       )
	      )))
    status))

(defun parse-twuser (data &optional (tagname "user"))
  (let (user)
    (setq user (make-twuser))
    (dolist (c data)
      (let ((cn (car c))
	    (cc (cdr c)))
	(cond ((equal cn "id")
	       (when cc
		 (setf (twuser-id user) cc))
	       )
	      ((equal cn "name")
	       (setf (twuser-name user) cc)
	       )
	      ((equal cn "screen_name")
	       (setf (twuser-screen_name user) cc)
	       )
	      ((equal cn "location")
	       (setf (twuser-location user) cc)
	       )
	      ((equal cn "description")
	       (setf (twuser-description user) cc)
	       )
	      ((equal cn "profile_image_url")
	       (setf (twuser-profile_image_url user) cc)
	       )
	      ((equal cn "url")
	       (setf (twuser-url user) cc)
	       )
	      ((equal cn "protected")
	       (setf (twuser-protected user) cc)
	       )
	      ((equal cn "status")
	       (setf (twuser-status user) (parse-twstatus cc)))
	      )))
    user))

(defun parse-twsearch (data)
  (let (obj)
    (setq obj (make-twsearch))
    (dolist (c data)
      (let ((cn (car c))
	    (cc (cdr c)))
	(cond ((equal cn "created_at")
	       (setf (twsearch-created_at obj) (parse-created_at cc))
	       )
	      ((equal cn "id")
	       (when cc
		 (setf (twsearch-id obj) cc))
	       )
	      ((equal cn "text")
	       (setf (twsearch-text obj) cc)
	       )
	      ((equal cn "source")
	       (setf (twsearch-source obj) cc)
	       )
	      ((equal cn "profile_image_url")
	       (setf (twsearch-profile_image_url obj) cc)
	       )
	      ((equal cn "to_user_id")
	       (setf (twsearch-to_user_id obj) cc)
	       )
	      ((equal cn "from_user")
	       (setf (twsearch-from_user obj) cc)
	       )
	      ((equal cn "from_user_id")
	       (setf (twsearch-from_user_id obj) cc)
	       )
	      ((equal cn "iso_language_code")
	       (setf (twsearch-iso_language_code obj) cc)
	       )
	      )))
    obj))

(defun parse-statuses (data)
  (let (statuses)
    (twdebug "~S" data)
    (when (or (stringp (car data))
	      (stringp (car (car data))))
      (setq data (list data)))
    (dolist (c data)
      (let ((s (parse-twstatus c)))
	(when s
	  (push s statuses))))
    (nreverse statuses)))

(defun parse-users (data)
  (let* (users)
    (twdebug "~S" data)
    (dolist (c data)
      (let ((s (parse-twuser c)))
	(when s
	  (push s users))))
    (nreverse users)))

(defun parse-direct_messages (data)
  (let* (msgs)
    (twdebug "~S" data)
    (when (or (stringp (car data))
	      (stringp (car (car data))))
      (setq data (list data)))
    (dolist (c data)
      (let ((s (parse-twdirect c)))
	(when s
	  (push s msgs))))
    (nreverse msgs)))

(defun parse-search-result (data)
  (let (msgs
	(tname (car (car data)))
	(results (cdr (car data)))
	(query (cdr data)))
    (twdebug "~S" data)
    (dolist (c results)
      (let ((s (parse-twsearch c)))
	(when s
	  (push s msgs))))
    (nreverse msgs)))

(defun parse-trends-result (data)
  (let (trends)
    (twdebug "~S" data)
    (dolist (d data)
      (let ((key (car d))
	    (val (cdr d)))
	(when (equal key "trends")
	  (dolist (c val)
	    (when (listp c)
	      (let ((name (cdr (assoc "name" c :test #'equal))))
		(when name
		  (push name trends)))))
	  )))
    (nreverse trends)))

(defun parse-error (data)
  (let ((msg "unknown."))
    (twdebug "~S" data)
    (dolist (c data)
      (let ((cname (car c))
	    (ccont (cdr c)))
	(when (equal cname "error")
	  (setq msg ccont))))
    msg))

(defun read-error ()
  (let ((file (error-file)))
    (when (file-exist-p file)
      (parse-error (parse-file file)))))

(defun fetch-statuses (url tmpfile headers &optional force)
  (multiple-value-bind (status response-headers)
      (http-download-file url tmpfile force :headers headers)
    (let (data)
      (twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
      (cond ((equal status "200")
	     (when (file-exist-p tmpfile)
	       (setq data (parse-file tmpfile)))
	     (values t status (parse-statuses data)))
	    (t
	     (values nil status (read-error)))))))

(defun fetch-search-result (url tmpfile headers &optional force)
  (multiple-value-bind (status response-headers)
      (http-download-file url tmpfile force :headers headers)
    (let (data)
      (twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
      (cond ((equal status "200")
	     (when (file-exist-p tmpfile)
	       (setq data (parse-file tmpfile)))
	     (values t status (parse-search-result data)))
	    (t
	     (values nil status (read-error)))))))

(defun fetch-trends-result (url tmpfile headers &optional force)
  (multiple-value-bind (status response-headers)
      (http-download-file url tmpfile force :headers headers)
    (let (data)
      (twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
      (cond ((equal status "200")
	     (when (file-exist-p tmpfile)
	       (setq data (parse-file tmpfile)))
	     (values t status (parse-trends-result data)))
	    (t
	     (values nil status (read-error)))))))

(defun url-public_timeline ()
  (concat *api-url-root*
	  "statuses/"
	  "public_timeline.json"))

(defun url-user_timeline (&optional target)
  (concat *api-url-root*
	  "statuses/"
	  (if target
	      (format nil "user_timeline/~A.json" target)
	    "user_timeline.json")))

(defun url-friends_timeline (&optional target count)
  (concat *api-url-root*
	  "statuses/"
	  (if target
	      (format nil "friends_timeline/~A.json?count=~A" target count)
            (format nil "friends_timeline.json?count=~A" count))))
(defun url-replies_timeline ()
  (concat *api-url-root*
	  "statuses/"
	  "replies.json"))

(defun url-direct_messages ()
  (concat *api-url-root*
	  "direct_messages.json"))

(defun url-friends (&optional target)
  (concat *api-url-root*
	  "statuses/"
	  (if target
	      (format nil "friends/~A.json" target)
	    "friends.json")))

(defun url-followers ()
  (concat *api-url-root*
	  "statuses/"
	  "followers.json"))

(defun url-update ()
  (concat *api-url-root*
	  "statuses/"
	  "update.json"))

(defun url-direct_message ()
  (concat *api-url-root*
	  "direct_messages/"
	  "new.json"))

(defun url-search (query)
  (concat *search-url-root*
	  ""
	  "search.json"
	  (format nil "?rpp=~D~@[&lang=~A~]&q="
		  (min *summary-search-max*
		       *search-max-number*)
		  *search-lang*)
	  (si:www-url-encode
	   (convert-encoding-from-internal *encoding-utf8n* query))
	  ))

(defun url-trends ()
  (concat *search-url-root*
	  ""
	  "trends.json"))
