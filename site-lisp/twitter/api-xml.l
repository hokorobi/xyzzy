;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is not part of xyzzy.
;;;
; $Id: api.l 813 2008-07-02 04:11:30Z torihat $
;
; twitter/api-xml.l
;
; by HATTORI Masashi

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "twitter/api")
  (require "xml-parser-modoki"))

(provide "twitter/api-xml")

(in-package "twitter")

(setq *error-filename* "error.xml")

(defun parse-file (file)
  (ed::xmlpm-parse-file file))

(defun parse-twstatus (xml)
  (let ((name (ed::xmlpm-tag-name xml))
	(contents (ed::xmlpm-tag-contents xml))
	status)
    (when (equal name "status")
      (setq status (make-twstatus))
      (dolist (c contents)
	(let ((cn (ed::xmlpm-tag-name c))
	      (cc (ed::xmlpm-tag-contents c)))
	  (cond ((equal cn "created_at")
		 (setf (twstatus-created_at status) (parse-twdate (car cc)))
		 )
		((equal cn "id")
		 (when (car cc)
		   (setf (twstatus-id status) (parse-integer (car cc))))
		 )
		((equal cn "text")
		 (setf (twstatus-text status) (car cc))
		 )
		((equal cn "source")
		 (setf (twstatus-source status) (car cc))
		 (setf (twstatus-source_name status) (parse-twsourcename (car cc)))
		 (setf (twstatus-source_url status) (parse-twsourceurl (car cc)))
		 )
		((equal cn "truncated")
		 (setf (twstatus-truncated status) (not (equal (car cc) "false")))
		 )
		((equal cn "user")
		 (setf (twstatus-user status) (parse-twuser c))
		 )
		))))
    status))

(defun parse-twdirect (xml)
  (let ((name (ed::xmlpm-tag-name xml))
	(contents (ed::xmlpm-tag-contents xml))
	status)
    (when (equal name "direct_message")
      (setq status (make-twdirect))
      (dolist (c contents)
	(let ((cn (ed::xmlpm-tag-name c))
	      (cc (ed::xmlpm-tag-contents c)))
	  (cond ((equal cn "created_at")
		 (setf (twdirect-created_at status) (parse-twdate (car cc)))
		 )
		((equal cn "id")
		 (when (car cc)
		   (setf (twdirect-id status) (parse-integer (car cc))))
		 )
		((equal cn "sender_id")
		 (when (car cc)
		   (setf (twdirect-sender_id status) (parse-integer (car cc))))
		 )
		((equal cn "recipient_id")
		 (when (car cc)
		   (setf (twdirect-recipient_id status) (parse-integer (car cc))))
		 )
		((equal cn "text")
		 (setf (twdirect-text status) (car cc))
		 )
		((equal cn "sender_screen_name")
		 (setf (twdirect-sender_screen_name status) (car cc))
		 )
		((equal cn "recipient_screen_name")
		 (setf (twdirect-recipient_screen_name status) (car cc))
		 )
		((equal cn "sender")
		 (setf (twdirect-sender status) (parse-twuser c cn))
		 )
		((equal cn "recipient")
		 (setf (twdirect-recipient status) (parse-twuser c cn))
		 )
		))))
    status))

(defun parse-twuser (xml &optional (tagname "user"))
  (let ((name (ed::xmlpm-tag-name xml))
	(contents (ed::xmlpm-tag-contents xml))
	user)
    (when (equal name tagname)
      (setq user (make-twuser))
      (dolist (c contents)
	(let ((cn (ed::xmlpm-tag-name c))
	      (cc (ed::xmlpm-tag-contents c)))
	  (cond ((equal cn "id")
		 (when (car cc)
		   (setf (twuser-id user) (parse-integer (car cc))))
		 )
		((equal cn "name")
		 (setf (twuser-name user) (car cc))
		 )
		((equal cn "screen_name")
		 (setf (twuser-screen_name user) (car cc))
		 )
		((equal cn "location")
		 (setf (twuser-location user) (car cc))
		 )
		((equal cn "description")
		 (setf (twuser-description user) (car cc))
		 )
		((equal cn "profile_image_url")
		 (setf (twuser-profile_image_url user) (car cc))
		 )
		((equal cn "url")
		 (setf (twuser-url user) (car cc))
		 )
		((equal cn "protected")
		 (setf (twuser-protected user) (not (equal (car cc) "false")))
		 )
		((equal cn "status")
		 (setf (twuser-status user) (parse-twstatus c)))
		))))
    user))

(defun parse-statuses (xml)
  (let* ((base (car xml))
	 (name (ed::xmlpm-tag-name base))
	 (contents (ed::xmlpm-tag-contents base))
	 statuses)
    (twdebug "~S" xml)
    (when (equal name "statuses")
      (dolist (c contents)
	(let ((s (parse-twstatus c)))
	  (when s
	    (push s statuses))))
      (nreverse statuses))))

(defun parse-users (xml)
  (let* ((base (car xml))
	 (name (ed::xmlpm-tag-name base))
	 (contents (ed::xmlpm-tag-contents base))
	 users)
    (twdebug "~S" xml)
    (when (equal name "users")
      (dolist (c contents)
	(let ((s (parse-twuser c)))
	  (when s
	    (push s users))))
      (nreverse users))))

(defun parse-direct_messages (xml)
  (let* ((base (car xml))
	 (name (ed::xmlpm-tag-name base))
	 (contents (ed::xmlpm-tag-contents base))
	 msgs)
    (twdebug "~S" xml)
    (when (equal name "direct-messages")
      (dolist (c contents)
	(let ((s (parse-twdirect c)))
	  (when s
	    (push s msgs))))
      (nreverse msgs))))

(defun parse-error (xml)
  (let* ((base (car xml))
	 (name (ed::xmlpm-tag-name base))
	 (contents (ed::xmlpm-tag-contents base))
	 (msg "unknown."))
    (twdebug "~S" xml)
    (when (equal name "hash")
      (dolist (c contents)
	(let ((cname (ed::xmlpm-tag-name c))
	      (ccont (ed::xmlpm-tag-contents c)))
	  (when (equal cname "error")
	    (setq msg (car ccont))))))
    msg))

(defun read-error ()
  (let ((file (error-file)))
    (when (file-exist-p file)
      (parse-error (ed::xmlpm-parse-file file)))))

(defun fetch-statuses (url tmpfile headers &optional force)
  (multiple-value-bind (status response-headers)
      (http-download-file url tmpfile force :headers headers)
    (let (data)
      (twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
      (cond ((equal status "200")
	     (when (file-exist-p tmpfile)
	       (setq data (ed::xmlpm-parse-file tmpfile)))
	     (values t status (parse-statuses data)))
	    (t
	     (values nil status (read-error)))))))

(defun fetch-search-result (url tmpfile headers &optional force)
  (error "Not implemented."))

(defun fetch-trends-result (url tmpfile headers &optional force)
  (error "Not implemented."))

(defun url-public_timeline ()
  (concat *api-url-root*
	  "statuses/"
	  "public_timeline.xml"))

(defun url-user_timeline (&optional target)
  (concat *api-url-root*
	  "statuses/"
	  (if target
	      (format nil "user_timeline/~A.xml" target)
	    "user_timeline.xml")))

(defun url-friends_timeline (&optional target count)
  (concat *api-url-root*
	  "statuses/"
	  (if target
	      (format nil "friends_timeline/~A.xml?count=~A" target count)
            (format nil "friends_timeline.xml?count=~A" count))))

(defun url-replies_timeline ()
  (concat *api-url-root*
	  "statuses/"
	  "replies.xml"))

(defun url-direct_messages ()
  (concat *api-url-root*
	  "direct_messages.xml"))

(defun url-friends (&optional target)
  (concat *api-url-root*
	  "statuses/"
	  (if target
	      (format nil "friends/~A.xml" target)
	    "friends.xml")))

(defun url-followers ()
  (concat *api-url-root*
	  "statuses/"
	  "followers.xml"))

(defun url-update ()
  (concat *api-url-root*
	  "statuses/"
	  "update.xml"))

(defun url-direct_message ()
  (concat *api-url-root*
	  "direct_messages/"
	  "new.xml"))
