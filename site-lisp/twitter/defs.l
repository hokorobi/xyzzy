;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is not part of xyzzy.
;;;
; $Id: defs.l 815 2009-03-23 05:00:54Z torihat $
;
; twitter/defs.l
;
; by HATTORI Masashi

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "junk/lib"))

(defpackage "twitter"
  (:use "lisp" "editor"))

(provide "twitter/defs")

(in-package "twitter")

(defvar *username* nil)
(defvar *password* nil)

(defvar *client-name*    "twitter-mode")
(defvar *client-version* "0.0.0.5-alpha")
(defvar *client-source*  "twittermode")
(defvar *client-url*     "http://www7a.biglobe.ne.jp/~hat/xyzzy/twitter/twitter-mode.xml")

(defvar *debug-mode* nil)

(defvar *base-directory* "~/.twitter")
(defvar *config-file* (merge-pathnames "config.l" *base-directory*))

(defvar *buffer-prefix* "*Twitter ")

(defvar *pframe-name* "Twitter")
(defvar *pframe-old* nil)

(defvar *start-hook* nil)
(defvar *finish-hook* nil)

(defun twdebug (fmt &rest args)
  (and *debug-mode*
       (message-box (apply #'format nil fmt args)
		    *client-name*)))

(defun open-web (url)
  (shell-execute url t))

(defun load-config ()
  (junk::load-config-file *config-file*))

(defun frame-set ()
  (setq *pframe-old* (ed::pseudo-frame-name (selected-pseudo-frame)))
  (new-pseudo-frame *pframe-name*))

(defun frame-get ()
  (find-pseudo-frame *pframe-name*))

(defun hide ()
  (interactive)
  (let ((old (find-pseudo-frame *pframe-old*)))
    (when old
      (select-pseudo-frame old))))

(defun restore ()
  (interactive)
  (let ((frame (frame-get)))
    (if frame
	  (select-pseudo-frame frame)
      (user::twitter))))

(defun init()
  (while (not *username*)
    (setq *username* (read-string "Twitter Username: ")))
  (while (not *password*)
    (setq *password* (junk::read-password
		      (format nil "Twitter Password (~A): " *username*))))
  )

(defun finish ()
  (interactive)
  (summary-auto-reload-stop)
  (let ((frame (frame-get)))
    (dolist (buf (buffer-list))
      (let ((bufname (buffer-name buf)))
	(when (string-match (concat "^" (regexp-quote *buffer-prefix*)) bufname)
	  (delete-buffer buf))))
    (when frame
      (delete-pseudo-frame frame)))
  (run-hooks '*finish-hook*))

(defun user::twitter-toggle ()
  (interactive)
  (if (equal *pframe-name*
	     (ed::pseudo-frame-name (selected-pseudo-frame)))
      (hide)
    (restore)))

(defun user::twitter ()
  (interactive)
  (when (frame-get)
    (restore)
    (return-from user::twitter))
  (load-config)
  (frame-set)
  (handler-case
      (progn
	(init)
	(summary-buffer-prepare)
	(summary-load-statuses-friends)
	(run-hooks '*start-hook*))
    (error (c)
      (msgbox "Error: ~A" (si:*condition-string c))
      (finish))
    (condition (c)
      (message "~A" (si:*condition-string c))
      (finish))))

;;;
(require "twitter/api")
(require "twitter/api-json")
(require "twitter/summary")
