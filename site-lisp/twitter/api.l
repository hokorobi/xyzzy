;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is not part of xyzzy.
;;;
; $Id: api.l 815 2009-03-23 05:00:54Z torihat $
;
; twitter/api.l
;
; by HATTORI Masashi

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "twitter/defs")
  (require "junk/http"))

(provide "twitter/api")

(in-package "twitter")

(defconstant *status-max-length* 140)

(defvar *api-url-root* "http://twitter.com/")
(defvar *web-url-root* "http://twitter.com/")

(defvar *search-url-root* "http://search.twitter.com/")
(defvar *search-max-number* 100)
(defvar *search-lang* nil)

(defvar *error-filename* nil)

(defmacro error-file ()
  `(merge-pathnames ,*error-filename* ,*base-directory*))

(defstruct twstatus
  created_at
  id
  text
  source
  source_name
  source_url
  truncated
  user)

(defstruct twuser
  id
  name
  screen_name
  location
  description
  profile_image_url
  url
  protected
  status)

(defstruct twdirect
  id
  text
  sender_id
  recipient_id
  created_at
  sender_screen_name
  recipient_screen_name
  sender
  recipient
  )

(defstruct twsearch
  created_at
  id
  text
  source
  profile_image_url
  to_user_id
  from_user
  from_user_id
  iso_language_code)

(defun struct-created_at (struct)
  (cond ((twstatus-p struct)
	 (twstatus-created_at struct))
	((twdirect-p struct)
	 (twdirect-created_at struct))
	(t
	 0)))

(defun url-user (user)
  (let ((screen-name
	 (cond ((twuser-p user)
		(twuser-screen_name user))
	       ((stringp user)
		user))))
    (format nil "~A~@[~A~]" *web-url-root* screen-name)))

(defun parse-twdate (str)
  "Parse date string (like \"Wed Jan 30 01:11:45 +0000 2008\")"
  (let ((timestamp 0))
    (when (and (stringp str)
	       (string-match "^[a-zA-Z]+ *\\([a-zA-Z]+\\) +\\([0-9]+\\)\\(.*\\) \\([0-9]+\\)$" str))
      (setq str (format nil "~A ~A ~A ~A"
			(match-string 2)
			(match-string 1)
			(match-string 4)
			(match-string 3)
			))
      (setq timestamp (junk::rfc822-parse-date str)))
    timestamp))

(defun parse-created_at (str)
  (let ((timestamp 0))
    (when (stringp str)
      (setq timestamp (junk::rfc822-parse-date str)))
    timestamp))

(defun parse-twsourcename (str)
  (if (stringp str)
      (substitute-string str
			 "<[^>]+>"
			 "")
    str))

(defun parse-twsourceurl (str)
  (and (stringp str)
       (string-match "href=\"\\([^\"]+\\)\"" str)
       (match-string 1)))

(defun create-request-headers (&optional username password)
  (let (headers)
    (junk::junk-http-add-header "X-Twitter-Client" *client-name* headers)
    (junk::junk-http-add-header "X-Twitter-Client-Version" *client-version* headers)
    (when *client-url*
      (junk::junk-http-add-header "X-Twitter-Client-URL" *client-url* headers))
    (when (and username password)
      (junk::junk-http-add-header "Authorization"
				  (concat "Basic "
					  (junk::junk-http-auth-encode-account username
									       password))
				  headers))
    headers))

(defun http-download-file (url file &optional force &key headers post)
  (let (sock
	status
	response-headers
	dir
	size
	wtime
	result)
    ; ローカルディレクトリがなければ作成
    (setq dir (directory-namestring file))
    (unless (file-exist-p dir)
      (create-directory dir))
    ; 既にファイルがある場合
    (when (file-exist-p file)
      (setq size (file-length file))
      (setq wtime (file-write-time file))
      (unless (or force post)
	(return-from http-download-file (values "200" nil)))
;	; If-Modified-Since: 更新されていれば取得
;	(push (cons "If-Modified-Since" (junk::rfc822-format-date wtime)) headers))
      )
    (push (cons "Accept" "*/*") headers)
    ; HTTP GET
    (handler-case
	(multiple-value-setq (sock status response-headers)
	  (junk::junk-http-get-url url :headers headers :post post))
      (socket-error (c)
	(when (socket-stream-p sock)
	  (close sock))
	(junk::junk-error "Socket error: ~A" (si:*condition-string c)))
      (quit (c)
	(when (socket-stream-p sock)
	  (close sock))
	(junk::junk-error "Interrupted: ~A" (si:*condition-string c)))
      )
    (unless (stringp status)
      (junk::junk-error "~S~%~S" status response-headers))
    ; 更新されていない
    (when (string-match "^304" status)
      (junk::junk-message "~A" status)
      (twdebug "~S~%~S~%~S" url status response-headers)
      (return-from http-download-file (values status response-headers)))
    ; Redirect
    (when (string-match "^302" status)
      (let ((location (junk::junk-http-get-header-value "location" response-headers)))
	(when location
	  (return-from http-download-file
	    (http-download-file location file force)))))
    ; ステータス
    (unless (and (string-match "^2[0-9][0-9]" status)
		 response-headers)
      ; エラーの場合はエラーファイルに書く
      (setq file (error-file)))
    (let ((clength (junk::junk-http-get-header-value "content-length" response-headers))
	  (last (junk::junk-http-get-header-value "last-modified" response-headers))
	  (cencoding (junk::junk-http-get-header-value "content-encoding" response-headers))
	  stime
	  (gzip-p nil))
      ; 更新日時の取得
      (when last
	(setq stime (junk::rfc822-parse-date last)))
      ; 受信バイト数の取得
      (if (and (stringp clength)
	       (string-match "^[0-9]+$" clength))
	  (setq clength (parse-integer clength))
	(setq clength nil))
      ; エンコーディング
      (when (and (stringp cencoding)
		 (string-match "gzip" cencoding))
	(setq gzip-p t))
      ; 受信
      (long-operation
	(if gzip-p
	    (junk::junk-http-gzip-to-file sock file clength)
	  (junk::junk-http-data-to-file sock file clength))
	(when stime
	  (set-file-write-time file stime))
	(junk::junk-message "saved:  ~A" file)))
    (twdebug "~S~%~S~%~S" url status response-headers)
    (values status response-headers)))

(defun local-directory (username &optional prefix)
  (let ((suffix (format nil "~@[~A/~]~@[~A/~]" prefix username)))
    (if (equal suffix "")
	*base-directory*
      (merge-pathnames suffix *base-directory*))))

(defun local-file (url &optional username)
  (let (file prefix)
    (multiple-value-bind (proto host path anchor port url)
	(junk::junk-http-url-study url)
      (when path
	(setq prefix host)
	(setq file (substitute-string (substitute-string path "^/" "") "?" "/"))))
    (merge-pathnames (or file "temp.xml") (local-directory username prefix))))

(defun statuses-public_timeline (&optional force)
  (let* ((url (url-public_timeline))
	 (tmpfile (local-file url))
	 (headers (create-request-headers)))
    (fetch-statuses url tmpfile headers force)))

(defun statuses-user_timeline (username password &optional force target)
  (let* ((url (url-user_timeline target))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password)))
    (fetch-statuses url tmpfile headers force)))

(defun statuses-friends_timeline (username password &optional force target)
  (let* ((url (url-friends_timeline target *summary-friends-timeline-count*))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password)))
    (fetch-statuses url tmpfile headers force)))

(defun statuses-replies_timeline (username password &optional force)
  (let* ((url (url-replies_timeline))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password)))
    (fetch-statuses url tmpfile headers force)))

(defun statuses-direct_messages (username password &optional force)
  (let* ((url (url-direct_messages))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password)))
    (multiple-value-bind (status response-headers)
	(http-download-file url tmpfile force :headers headers)
      (progn
	(twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
	(when (file-exist-p tmpfile)
	  (setq data
		(parse-file tmpfile)))
	(cond ((equal status "200")
	       (values t status (parse-direct_messages data)))
	      (t
	       (values nil status (read-error))))))))

(defun statuses-friends (username password &optional force target)
  (let* ((url (url-friends target))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password)))
    (multiple-value-bind (status response-headers)
	(http-download-file url tmpfile force :headers headers)
      (progn
	(twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
	(when (file-exist-p tmpfile)
	  (setq data
		(parse-file tmpfile)))
	(cond ((equal status "200")
	       (values t status (parse-users data)))
	      (t
	       (values nil status (read-error))))))))

(defun statuses-followers (username password &optional force)
  (let* ((url (url-followers))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password)))
    (multiple-value-bind (status response-headers)
	(http-download-file url tmpfile force :headers headers)
      (progn
	(twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
	(when (file-exist-p tmpfile)
	  (setq data
		(parse-file tmpfile)))
	(cond ((equal status "200")
	       (values t status (parse-users data)))
	      (t
	       (values nil status (read-error))))))))

(defun statuses-update (username password status)
  (let* ((url (url-update))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password))
	 (post (format nil "status=~A&source=~A"
		       (si::www-url-encode
			(map-internal-to-utf-8 status))
		       (si::www-url-encode *client-source*)))
	 data)
    (setq url (concat url "?" post))
    (multiple-value-bind (status response-headers)
	(http-download-file url tmpfile nil :headers headers :post post)
      (progn
	(twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
	(when (file-exist-p tmpfile)
	  (setq data
		(parse-file tmpfile)))
	(cond ((equal status "200")
	       (values t status (parse-statuses data)))
	      (t
	       (values nil status (read-error))))))))

(defun send-direct_message (username password target message)
  (let* ((url (url-direct_message))
	 (tmpfile (local-file url username))
	 (headers (create-request-headers username password))
	 (post (format nil "user=~A&text=~A"
		       target
		       (si::www-url-encode
			(map-internal-to-utf-8 message))))
	 data)
    (multiple-value-bind (status response-headers)
	(http-download-file url tmpfile nil :headers headers :post post)
      (progn
	(twdebug "~S~%~S~%~S~%~{~S~%~}" url tmpfile status response-headers)
	(when (file-exist-p tmpfile)
	  (setq data
		(parse-file tmpfile)))
	(cond ((equal status "200")
	       (values t status (parse-direct_messages data)))
	      (t
	       (values nil status (read-error))))))))

(defun statuses-search (query &optional force)
  (let* ((url (url-search query))
	 (tmpfile (local-file url))
	 (headers (create-request-headers)))
    (fetch-search-result url tmpfile headers force)))

(defun statuses-trends (&optional force)
  (let* ((url (url-trends))
	 (tmpfile (local-file url))
	 (headers (create-request-headers)))
    (fetch-trends-result url tmpfile headers force)))
