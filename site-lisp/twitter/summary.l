;;; -*- Mode: Lisp; Package: EDITOR -*-
;;;
;;; This file is not part of xyzzy.
;;;
; $Id: summary.l 816 2009-03-23 13:23:25Z torihat $
;
; twitter/summary.l
;
; by HATTORI Masashi

(provide "twitter/summary")

(in-package "twitter")

(defvar *summary-buffer* (concat *buffer-prefix* "Summary*"))
(defvar *summary-mode-name* "twitter-mode")

(defvar *summary-mode-hook* nil)

(defvar *summary-target-user* nil)

(defvar *summary-auto-reload-p* nil)
(defvar *summary-auto-reload-interval* 600)

(defvar *summary-search-max* 20)
(defvar *summary-search-last-query* nil)
;(defvar *summary-search-history* nil)
;(defvar *summary-search-histroy-max* 10)

(defvar *summary-regexp-keyword-list*
  (compile-regexp-keyword-list
   '(
     ("@[_a-zA-Z0-9]+"
      t (:keyword 1)))))

(defvar *attribute-item*
  (list (cons 'status (list (cons 'time     (list :foreground 10))
			    (cons 'user     (list :foreground 11))
			    (cons 'name     (list :foreground  9))
			    (cons 'location (list :foreground 14))
			    (cons 'text     nil)))
	(cons 'direct (list (cons 'time     (list :foreground 11))
			    (cons 'user     (list :foreground 12))
			    (cons 'name     (list :foreground 13))
			    (cons 'location (list :foreground 14))
			    (cons 'text     nil)))
	(cons 'user   (list (cons 'time     (list :foreground 14))
			    (cons 'user     (list :foreground 11))
			    (cons 'name     (list :foreground 12))
			    (cons 'location (list :foreground  9))
			    (cons 'text     nil)))
	))

(defvar *attribute-line* (list :foreground 15))

(defvar *summary-command-history* nil)
(defvar *summary-command-history-limit* 20)
(defvar *summary-command-history-forward*)

(defvar *summary-map* nil)
(unless *summary-map*
  (setq *summary-map* (make-sparse-keymap))
  (define-key *summary-map* #\j      'summary-next)
  (define-key *summary-map* #\k      'summary-previous)
  (define-key *summary-map* #\Left   'summary-history-back)
  (define-key *summary-map* #\Right  'summary-history-forward)
  (define-key *summary-map* #\f      'summary-load-statuses-friends)
  (define-key *summary-map* #\r      'summary-load-statuses-replies)
  (define-key *summary-map* #\d      'summary-load-statuses-direct)
  (define-key *summary-map* #\s      'summary-load-statuses-user)
  (define-key *summary-map* #\p      'summary-load-statuses-public)
  (define-key *summary-map* #\i      'summary-load-friends)
  (define-key *summary-map* #\l      'summary-load-followers)
  (define-key *summary-map* #\R      'summary-reload)
  (define-key *summary-map* #\c      'summary-target-set-reload)
  (define-key *summary-map* #\C      'summary-target-clear-reload)
  (define-key *summary-map* #\u      'summary-update)
  (define-key *summary-map* #\a      'summary-reply)
  (define-key *summary-map* #\m      'summary-direct)
  (define-key *summary-map* #\o      'summary-open-web)
  (define-key *summary-map* #\t      'summary-auto-reload-toggle)
  (define-key *summary-map* #\q      'finish)
  (define-key *summary-map* #\e      'summary-menu-popup)
  (define-key *summary-map* #\RBtnUp 'summary-mouse-right-press)
  (define-key *summary-map* #\/      'summary-search)
  (define-key *summary-map* #\T      'summary-trends-update)
  )

(defun summary-menu-create ()
  (let ((menu (create-popup-menu))
	(sender1 (summary-find-user))
	(sender2 (summary-find-user t))
	(current (or *summary-target-user* *username*))
	senders)
    (dolist (temp (list sender1 sender2))
      (when (and temp
		 (not (equal temp current)))
	(pushnew temp senders :test #'equal)))
    (add-popup-menu menu
		    (summary-menu-clear)
		    (concat "@" current))
    (add-menu-separator menu)
    (add-menu-item menu
		   'summary-reload
		   "Reload(&R)"
		   'summary-reload)
    (add-menu-item menu
		   'summary-load-statuses-friends
		   "Recent(&f)"
		   'summary-load-statuses-friends)
    (add-menu-item menu
		   'summary-load-statuses-replies
		   "Replies(&r)"
		   'summary-load-statuses-replies
		   #'(lambda ()
		       (if *summary-target-user* :disable)))
    (add-menu-item menu
		   'summary-load-statuses-user
		   "User's Recent(&s)"
		   'summary-load-statuses-user)
    (add-menu-item menu
		   'summary-load-statuses-direct
		   "Direct Messages(&d)"
		   'summary-load-statuses-direct
		   #'(lambda ()
		       (if *summary-target-user* :disable)))
    (add-menu-item menu
		   'summary-load-friends
		   "Followings(&i)"
		   'summary-load-friends)
    (add-menu-item menu
		   'summary-load-followers
		   "Followers(&l)"
		   'summary-load-followers
		   #'(lambda ()
		       (if *summary-target-user* :disable)))
    (add-menu-item menu
		   'summary-open-web
		   "Open in browser"
		   #'(lambda ()
		       (interactive)
		       (summary-open-web current)))
    (add-menu-separator menu)
    (add-menu-item menu
		   'summary-history-back
		   "Back"
		   'summary-history-back
		   #'(lambda ()
		       (or (< 1 (length *summary-command-history*)) :disable)))
    (add-menu-item menu
		   'summary-history-forward
		   "Forward"
		   'summary-history-forward
		   #'(lambda ()
		       (or *summary-command-history-forward* :disable)))
    (add-menu-separator menu)
    (add-menu-item menu
		   'summary-update
		   "Update..."
		   'summary-update)
    (add-menu-item menu
		   'summary-direct
		   "Direct Message..."
		   'summary-direct)
    (dolist (sender senders)
      (add-menu-separator menu)
      (add-popup-menu menu
		      (summary-menu-create-user sender)
		      (concat "@" sender)))
    (add-menu-separator menu)
    (add-menu-item menu
		   'summary-load-statuses-public
		   "Public Timeline(&p)"
		   'summary-load-statuses-public)
    (add-menu-item menu
		   'summary-open-web
		   "Open in browser this"
		   'summary-open-web)
    (add-menu-item menu
		   'summary-auto-reload-toggle
		   "Auto Reload"
		   'summary-auto-reload-toggle
		   #'(lambda ()
		       (if *summary-auto-reload-p* :check)))
    (add-menu-separator menu)
    (add-menu-item menu
		   'summary-search
		   "Search(&/)"
		   'summary-search)
    (add-popup-menu menu
		    (summary-menu-trends)
		    "Trends")
    (add-menu-separator menu)
    (add-menu-item menu
		   'finish
		   "Close"
		   'finish)
    menu))

(defun summary-menu-create-user (user)
  (let ((menu (create-popup-menu)))
    (add-menu-item menu
		   'summary-reply
		   "Reply...(&r)"
		   #'(lambda ()
		       (interactive)
		       (summary-update-reply user)))
    (add-menu-item menu
		   'summary-direct
		   "Direct Message...(&d)"
		   #'(lambda ()
		       (interactive)
		       (summary-direct user)))
    (add-menu-item menu
		   'summary-load-statuses-friends
		   "Recent(&f)"
		   #'(lambda ()
		       (interactive)
		       (summary-target-set user)
		       (summary-load-statuses-friends)))
    (add-menu-item menu
		   'summary-load-statuses-user
		   "User's Recent(&s)"
		   #'(lambda ()
		       (interactive)
		       (summary-target-set user)
		       (summary-load-statuses-user)))
    (add-menu-item menu
		   'summary-load-friends
		   "Followings(&i)"
		   #'(lambda ()
		       (interactive)
		       (summary-target-set user)
		       (summary-load-friends)))
    (add-menu-item menu
		   'summary-open-web
		   "Open in browser"
		   #'(lambda ()
		       (interactive)
		       (summary-open-web user)))
    menu))

(defun summary-menu-clear ()
  (let ((menu (create-popup-menu)))
    (when *summary-target-user*
      (add-menu-item menu
		     'summary-target-clear
		     (concat "switch to @" *username*)
		     'summary-target-clear-reload))
    menu))

(defun summary-menu-trends ()
  (let ((menu (create-popup-menu)))
    (add-menu-item menu
		   'summary-trends-update
		   "Update...(&T)"
		   'summary-trends-update)
    (when *summary-trends*
      (add-menu-separator menu)
      (dolist (trend *summary-trends*)
	(let ((query trend))
	  (add-menu-item menu
			 #'(lambda ()
			     (interactive)
			     (summary-search-query query))
			 trend
			 #'(lambda ()
			     (interactive)
			     (summary-search-query query))))))
    menu))

(defun summary-mouse-right-press ()
  (interactive)
  (set-window *last-mouse-window*)
  (summary-menu-popup))

(defun summary-menu-popup ()
  (interactive)
  (continue-pre-selection)
  (let ((menu (summary-menu-create)))
    (track-popup-menu menu)))

(defun summary-buffer-set ()
  (set-buffer (get-buffer-create *summary-buffer*)))

(defun summary-mode ()
  (kill-all-local-variables)
  (make-local-variable 'indent-tabs-mode)
  (summary-mode-line)
  (setq mode-name *summary-mode-name*)
  (setq indent-tabs-mode nil)
  (setq buffer-read-only t)
  (setq need-not-save t)
  (setq kept-undo-information nil)
  (setq auto-save nil)
  (toggle-ime nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-line-number* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-newline* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-eof* nil)
  (set-local-window-flags (selected-buffer)
			  *window-flag-cursor-line* t)
  (set-buffer-fold-width t)
  (make-local-variable 'regexp-keyword-list)
  (setq regexp-keyword-list *summary-regexp-keyword-list*)
  (use-keymap *summary-map*)
  (summary-title-line)
  (run-hooks '*summary-mode-hook*))

(defun summary-mode-line (&optional cmd)
  (setq cmd (or cmd
		(car (car *summary-command-history*))))
  (make-local-variable 'mode-line-format)
  (setq mode-line-format (format nil
				 "%b~@[ ~A~]~@[ (@~A)~] [Reload:~:[Off~;On~]]"
				 (summary-mode-line-cmd-name cmd)
				 (or *summary-target-user* *username*)
				 *summary-auto-reload-p*)))

(defun summary-mode-line-cmd-name (cmd)
  (case cmd
    ('summary-load-statuses-public  "Public Timeline")
    ('summary-load-statuses-user    "User's Recnet")
    ('summary-load-statuses-friends "Recent")
    ('summary-load-statuses-replies "Replies")
    ('summary-load-statuses-direct  "Direct Messages")
    ('summary-load-friends          "Followings")
    ('summary-load-followers        "Followeres")
    (t nil)
    ))

(defun summary-title-line ()
  (make-local-variable 'title-bar-format)
  (setq title-bar-format (format nil "~A/~A" *client-name* *client-version*)))

(defun summary-buffer-prepare ()
  (summary-buffer-set)
  (summary-mode)
  (erase-buffer (selected-buffer)))

(defun summary-print (items)
  (save-excursion
    (when items
      (junk::modify-read-only-buffer
	  (dolist (item items)
	    (summary-print-item item)
	    (summary-insert-line))))))

(defvar *summary-date-format-string* "%Y/%m/%d %H:%M:%S")

(defun summary-date-format (timestamp)
  (format-date-string *summary-date-format-string* timestamp))

(defun summary-print-item (struct)
    (multiple-value-bind (type time screen name location text)
	(summary-values struct)
      (let ((attrs (cdr (assoc type *attribute-item*))))
	(apply #'set-text-attribute
	       (point)
	       (progn
		 (insert (summary-date-format time))
		 (insert " ")
		 (point))
	       struct
	       (cdr (assoc 'time attrs)))
	(apply #'set-text-attribute
	       (point)
	       (progn
		 (insert (format nil "~@[@~15@A~]" screen))
		 (insert " ")
		 (point))
	       struct
	       (cdr (assoc 'user attrs)))
	(apply #'set-text-attribute
	       (point)
	       (progn
		 (insert (or name ""))
		 (insert " ")
		 (point))
	       struct
	       (cdr (assoc 'name attrs)))
	(apply #'set-text-attribute
	       (point)
	       (progn
		 (insert (format nil "~@[(~A)~]" location))
		 (point))
	       struct
	       (cdr (assoc 'location attrs)))
	(apply #'set-text-attribute
	       (point)
	       (progn
		 (insert "\n")
		 (insert (or text ""))
		 (insert "\n")
		 (point))
	       struct
	       (cdr (assoc 'text attrs)))
	)))

(defun summary-values (struct)
  (let (type time screen name location text)
    (cond ((twstatus-p struct)
	   (let ((user (or (twstatus-user struct) (make-twuser))))
	     (setq type 'status)
	     (setq time (twstatus-created_at struct))
	     (setq screen (twuser-screen_name user))
	     (setq name (twuser-name user))
	     (setq location (twuser-location user))
	     (setq text (twstatus-text struct))))
	  ((twdirect-p struct)
	   (let ((user (or (twdirect-sender struct) (make-twuser))))
	     (setq type 'direct)
	     (setq time (twdirect-created_at struct))
	     (setq screen (twuser-screen_name user))
	     (setq name (twuser-name user))
	     (setq location (twuser-location user))
	     (setq text (twdirect-text struct))))
	  ((twuser-p struct)
	   (let ((status (or (twuser-status struct) (make-twstatus)))
		 (user struct))
	     (setq type 'user)
	     (setq time (if (twstatus-p status) (twstatus-created_at status) 0))
	     (setq screen (twuser-screen_name user))
	     (setq name (twuser-name user))
	     (setq location (twuser-location user))
	     (setq text (twuser-description user))))
	  ((twsearch-p struct)
	   (let ()
	     (setq type 'status)
	     (setq time (twsearch-created_at struct))
	     (setq screen (twsearch-from_user struct))
	     ;(setq name (twsearch-from_user struct))
	     (setq location nil)
	     (setq text (twsearch-text struct))))
	  )
    (values type time screen name location text)))

(defun summary-insert-line ()
  (apply #'set-text-attribute
	 (point)
	 (progn
	   (insert "-" (- (window-width) 3))
	   (insert "\n")
	   (point))
	 'line
	 *attribute-line*))

(defun summary-error (http-status statuses)
  (error "ERROR: ~@A ~@A" http-status statuses))

(defun summary-load-statuses-public (&optional force (clear-forward t))
  (interactive "p")
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-public_timeline force)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-statuses-public clear-forward))
	(summary-error status statuses)))))

(defun summary-load-statuses-user (&optional force (clear-forward t))
  (interactive "p")
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-user_timeline *username* *password* force *summary-target-user*)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-statuses-user clear-forward))
	(summary-error status statuses)))))

(defun summary-load-statuses-friends (&optional force (clear-forward t))
  (interactive "p")
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-friends_timeline *username* *password* force *summary-target-user*)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-statuses-friends clear-forward))
	(summary-error status statuses)))))

(defun summary-load-statuses-replies (&optional force (clear-forward t))
  (interactive "p")
  (when *summary-target-user*
    (error "ユーザーを指定して実行することはできません"))
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-replies_timeline *username* *password* force)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-statuses-replies clear-forward))
	(summary-error status statuses)))))

(defun summary-load-statuses-direct (&optional force (clear-forward t))
  (interactive "p")
  (when *summary-target-user*
    (error "ユーザーを指定して実行することはできません"))
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-direct_messages *username* *password* force)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-statuses-direct clear-forward))
	(summary-error status statuses)))))

(defun summary-load-friends (&optional force (clear-forward t))
  (interactive "p")
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-friends *username* *password* force *summary-target-user*)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-friends clear-forward))
	(summary-error status statuses)))))

(defun summary-load-followers (&optional force (clear-forward t))
  (interactive "p")
  (when *summary-target-user*
    (error "ユーザーを指定して実行することはできません"))
  (long-operation
    (multiple-value-bind (result status statuses)
	(statuses-followers *username* *password* force)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-load-followers clear-forward))
	(summary-error status statuses)))))

(defun summary-update (&key (prompt "Twitter Update: ") default)
  (interactive)
  (let ((msg (read-string prompt :default default)))
    (when msg
      (long-operation
	(message "Sending ...")
	(multiple-value-bind (result http-status updated)
	    (statuses-update *username* *password* msg)
	  (if result
	      (message "Sending ... Done.")
	    (message "Sending ... Error ~S" http-status)))))))

(defun summary-update-reply (recipient)
  (summary-update :prompt "Twitter Reply: "
		  :default (concat "@" recipient " ")))

(defun summary-reply ()
  (interactive)
  (let ((recipient (summary-find-user)))
    (if recipient
	(summary-update-reply recipient)
      (summary-update))))

(defun summary-direct (&optional user)
  (interactive)
  (let (msg)
    (unless user
      (setq user (read-string "Direct Message To: "
			      :default (summary-find-user))))
    (when user
      (setq msg (read-string (format nil "Direct Message To ~A: " user)))
      (when msg
	(long-operation
	  (message "Sending ...")
	  (multiple-value-bind (result http-status updated)
	      (send-direct_message *username* *password* user msg)
	    (if result
		(message "Sending ... Done.")
	      (message "Sending ... Error ~S" http-status))))))))

(defun summary-find-user (&optional find-current-point)
  (let (recipient)
    (when find-current-point
      (save-excursion
	(skip-syntax-spec-backward "^ ")
	(when (looking-at "@\\([-_a-zA-Z0-9]+\\)")
	  (setq recipient (match-string 1)))))
    (unless recipient
      (long-operation
	(multiple-value-bind (from to tag foreground background bold underline strike-out prefix extend)
	    (find-text-attribute-point (point))
	  (cond ((twstatus-p tag)
		 (setq recipient (twuser-screen_name (twstatus-user tag))))
		((twdirect-p tag)
		 (setq recipient (twdirect-sender_screen_name tag)))
		((twuser-p tag)
		 (setq recipient (twuser-screen_name tag)))
		(t
		 (twdebug "~S" tag))))))
    recipient))

(defun summary-open-web (&optional recipient)
  (interactive)
  (unless recipient
    (setq recipient (summary-find-user t)))
  (when recipient
    (open-web (url-user recipient))))

(defvar *summary-recenter-line-number* 0)

(defun summary-next ()
  (interactive)
  (multiple-value-bind (beg end tag)
      (find-text-attribute 'line :start (point))
    (when end
      (goto-char end)
      (recenter *summary-recenter-line-number*))))

(defun summary-previous ()
  (interactive)
  (let (pos)
    (multiple-value-bind (beg end tag)
	(find-text-attribute 'line :end (point) :from-end t)
      (when (and beg (< 0 beg))
	(multiple-value-bind (beg2 end2 tag2)
	      (find-text-attribute 'line :end (1- beg) :from-end t)
	  (setq pos end2))))
    (goto-char (or pos (point-min)))
    (recenter *summary-recenter-line-number*)))

(defun summary-history-restore (history &optional (force nil) (clear-forward t))
  (let ((cmd (car history))
	(target (cdr history)))
    (if cmd
	(progn
	  (if target
	      (summary-target-set target)
	    (summary-target-clear))
	  (funcall cmd force clear-forward))
      (message "No history."))))

(defun summary-reload (&optional (clear-forward t))
  (interactive)
  (let* ((history (car *summary-command-history*)))
    (summary-history-restore history t clear-forward)))

(defun summary-target-clear ()
  (interactive)
  (setq *summary-target-user* nil)
  (summary-mode-line))

(defun summary-target-set (&optional user)
  (interactive)
  (unless user
    (setq user (read-string "Twitter User: "
			    :default (or user (summary-find-user)))))
  (when user
    (if (equal user *username*)
	(summary-target-clear)
      (progn
	(setq *summary-target-user* user)
	(summary-mode-line)))))

(defun summary-target-set-reload (&optional user)
  (interactive)
  (let ((cmd (car (car *summary-command-history*))))
    (summary-target-set user)
    (when cmd
      (summary-history-add cmd)
      (summary-reload))))

(defun summary-target-clear-reload (&optional user)
  (interactive)
  (let ((cmd (car (car *summary-command-history*))))
    (summary-target-clear)
    (when cmd
      (summary-history-add cmd)
      (summary-reload))))

(defun summary-history-add (cmd &optional (clear-forward t))
  (let ((history (if (consp cmd) cmd (cons cmd *summary-target-user*))))
    (unless (equal history (car *summary-command-history*))
      (push history *summary-command-history*))
    (twdebug "~{~S~%~}" *summary-command-history*)
    (when clear-forward
      (setq *summary-command-history-forward* nil))
    (when (< *summary-command-history-limit*
	     (length *summary-command-history*))
      (setq *summary-command-history*
	    (butlast *summary-command-history*
		     (- (length *summary-command-history*) *summary-command-history-limit*))))
    (when (equal (buffer-name (selected-buffer))
		 *summary-buffer*)
      (summary-mode-line cmd))
    ))

(defun summary-history-add-forward (history)
  (push history *summary-command-history-forward*))

(defun summary-history-back ()
  (interactive)
  (unless (cdr *summary-command-history*)
    (message "No previous history.")
    (return-from summary-history-back))
  (let ((history (pop *summary-command-history*)))
    (summary-history-add-forward history)
    (summary-reload nil)))

(defun summary-history-forward ()
  (interactive)
  (unless *summary-command-history-forward*
    (message "No next history.")
    (return-from summary-history-forward))
  (let ((history (pop *summary-command-history-forward*)))
    (summary-history-restore history nil nil)
    (when history
      (summary-history-add history nil))))

(defun summary-auto-reload ()
  (if *summary-auto-reload-p*
      (progn
	(setq *summary-auto-reload-p* nil)
	(when (equal (buffer-name (selected-buffer))
		     *summary-buffer*)
	  (handler-case
	      (progn
		(summary-reload)
		(refresh-screen))
	    (condition (c)
	      (message "~A" (si:*condition-string c))
	      (return-from summary-auto-reload)))
	  (summary-auto-reload-start)))
    (summary-mode-line)))

(defun summary-auto-reload-start-timer ()
  (start-timer *summary-auto-reload-interval*
	       'summary-auto-reload
	       t))

(defun summary-auto-reload-stop-timer ()
  (stop-timer 'summary-auto-reload))

(defun summary-auto-reload-status-message ()
  (message "Auto Reload: ~:[Off~;On~]" *summary-auto-reload-p*)
  (when (equal (buffer-name (selected-buffer))
	       *summary-buffer*)
    (summary-mode-line)))

(defun summary-auto-reload-start ()
  (interactive)
  (setq *summary-auto-reload-p* t)
  (summary-auto-reload-start-timer)
  (summary-auto-reload-status-message))

(defun summary-auto-reload-stop ()
  (interactive)
  (setq *summary-auto-reload-p* nil)
  (summary-auto-reload-stop-timer)
  (summary-auto-reload-status-message))

(defun summary-auto-reload-toggle ()
  (interactive)
  (if *summary-auto-reload-p*
      (summary-auto-reload-stop)
    (summary-auto-reload-start)))

(defun summary-search (&optional force (clear-forward t))
  (interactive "p")
  (let ((query *summary-search-last-query*))
    (when (or (not query)
	      (interactive-p))
      (setq query (read-string "Twitter Search: "
			       :default query)))
    (summary-search-query query force clear-forward)))

(defun summary-search-query (query &optional force (clear-forward t))
  (when query
    (setq *summary-search-last-query* query)
    (multiple-value-bind (result status statuses)
	(statuses-search query)
      (if result
	  (progn
	    (summary-buffer-prepare)
	    (summary-print statuses)
	    (summary-history-add 'summary-search clear-forward)
	    )
	(summary-error status statuses)))))

(defvar *summary-trends* nil)

(defun summary-trends-update (&optional force (clear-forward t))
  (interactive "p")
  (multiple-value-bind (result status statuses)
      (statuses-trends force)
    (if result
	(if (setq *summary-trends* statuses)
	    (progn
	      (message "Trends updated.")
	      (summary-menu-trends-popup)
	    )
	  (message "No trends."))
      (summary-error status statuses))))

(defun summary-menu-trends-popup ()
  (interactive)
  (continue-pre-selection)
  (let ((menu (summary-menu-trends)))
    (track-popup-menu menu)))
