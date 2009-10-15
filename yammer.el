;;; yammer.el -- Simple tool for accessing yammer.com

;; Copyright (C) 2009 Peter Sanford

;; Author: Peter Sanford <peter AT petersdanceparty.com>
;; Version: 1.01
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Simple mode for browsing and posting to yammer.com.

;; This mode requires oauth.el:
;; git clone git://github.com/psanford/emacs-oauth.git
;; and json.el: 
;; http://edward.oconnor.cx/2006/03/json.el

;; You will need to register for an oauth key/secret at
;; http://www.yammer.com/api_doc.html

;; Once you have a key and secrect, set yammer-consumer-key
;; and yammer-consumer-secret with those values.

;; Add the following to your emacs init file 
;; (require 'yammer)
;; (yammer-authenticate unix-user-name)

;; Useful functions:
;; yammer-list-messages
;; yammer-post-message
;; yammer-post-buffer-contents

;; set yammer-show-icons to enable mugshots 

;;; Code:

(require 'json)
(require 'oauth)
(require 'image-file)

(defvar yammer-consumer-key nil)
(defvar yammer-consumer-secret nil)

(defvar yammer-request-url "https://www.yammer.com/oauth/request_token")
(defvar yammer-access-url  "https://www.yammer.com/oauth/access_token") 
(defvar yammer-user-authorize "https://www.yammer.com/oauth/authorize")
(defvar yammer-list-url "https://yammer.com/api/v1/messages.json")
(defvar yammer-create-message-url "https://yammer.com/api/v1/messages")
(defvar yammer-delete-url-base "https://www.yammer.com/api/v1/messages/")

(defvar yammer-access-token nil)

(defvar yammer-timer nil)
(defvar yammer-timer-interval 60)

(defvar yammer-last-id 0)
(defvar yammer-new-tweets-hook nil)

(defvar yammer-uri-face 'yammer-uri-face)
(defvar yammer-username-face 'yammer-username-face)
(defvar yammer-in-reply-to-face 'yammer-in-reply-to-face)
(defvar yammer-date-face 'yammer-date-face)

(defun yammer-make-uri-clickable (text)
  (let ((regex-index 0))
    (while regex-index
      (setq regex-index
	    (string-match "\\(https?://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]+\\)"
			  text
			  regex-index))
      (when regex-index
	(let ((uri (match-string-no-properties 0 text)))
	  (add-text-properties
	   (match-beginning 0)
	   (match-end 0)
	   `(mouse-face highlight
	     face yammer-uri-face
	     uri-in-text ,uri)
	   text))
	(setq regex-index (match-end 0)))))
  text)

(defun yammer-username->string (req)
  (let ((user-name (format 
		    "%s"
		    (funcall get-username (hash-val 'sender_id req)))))
	(add-text-properties 0 (length user-name)
			     '(face yammer-username-face)
			     user-name)
	user-name))

(defun yammer-reply-to->string (req)
  (format 
   "%s:\n"
   (let ((reply-id (hash-val 'replied_to_id req))
	 (reply-tag " in reply to ")
	 (post))
     (or
      (when reply-id
	(setq post
	      (car (hash-val reply-id message-alist-by-thread)))
	(when post
	  (concat 
	   (progn
	     (add-text-properties 0 (length reply-tag)
				  `(face yammer-in-reply-to-face
				    reply-id ,reply-id)
				  reply-tag)
	     reply-tag)
	   (let ((post-name (funcall get-username (hash-val 'sender_id post))))
	     (add-text-properties 0 (length post-name)
				  '(face yammer-username-face)
				  post-name)
	     post-name))))
      ""))))

(defun yammer-date-and-client->string (req)
  (let ((text (format "\n\tAbout %s from %s\n"
		      (yammer-pretty-date 
		       (yammer-parse-date
			(hash-val 'created_at req)))
		      (hash-val 'client_type req))))
    (add-text-properties 0 (length text)
			 '(face yammer-date-face)
			 text)
    text))

(defun yammer-attachment->string (attachment)
  (let ((type (hash-val 'type attachment)))
    (cond
     ((equal type "image") 
      (yammer-make-uri-clickable 
       (format "%s" (hash-val 'url (hash-val 'image attachment)))))
     (t "Unknown attachment type"))))

(defun yammer-authenticate (username)
  "Get authentication token"
  (if (file-exists-p (format "/Users/%s/.yammer-token" username))
      (progn
        (save-excursion
          (find-file (format "/Users/%s/.yammer-token" username))
          (let ((str (buffer-substring (point-min) (point-max))))
            (if (string-match "\\([^:]*\\):\\(.*\\)"
                              (buffer-substring (point-min) (point-max)))
                (setq yammer-access-token
                      (make-oauth-access-token 
                       :consumer-key yammer-consumer-key
                       :consumer-secret yammer-consumer-secret
                       :auth-t (make-oauth-t
                                :token (match-string 1 str)
                                :token-secret (match-string 2 str))))))
          (save-buffer)
          (kill-this-buffer))))
  (unless yammer-access-token
    (let ((callback
           (lambda ()
             (let ((callback-token (read-string
                                    "Please enter the provided code: ")))
               (setq access-url
                     (concat access-url "?callback_token=" callback-token))))))
      (setq yammer-access-token
            (oauth-authorize-app yammer-consumer-key yammer-consumer-secret
                                 yammer-request-url yammer-access-url
                                 yammer-user-authorize
                                 callback)))
    (save-excursion
      (find-file (format "/Users/%s/.yammer-token" username))
      (end-of-buffer)
      (let ((token (oauth-access-token-auth-t yammer-access-token)))
        (insert (format "%s:%s\n" 
                        (oauth-t-token token)
                        (oauth-t-token-secret token))))
      (save-buffer)
      (kill-this-buffer)))
  yammer-access-token)
    
(defun yammer-internal-post-message (message &optional reply-to-id)
  "Post message to yammer"
  (let ((args `(("body" . ,message))))
    (if reply-to-id 
        (push `("replied_to_id" . ,(number-to-string reply-to-id)) args))
    (set-buffer (oauth-post-url yammer-access-token 
                                yammer-create-message-url args))
    (beginning-of-buffer)
    (let ((beg (point)) (line))
      (end-of-line)
      (setq line (buffer-substring beg (point)))
      (if (string-match "201 Created" line) (message "Message Created!")
        (error "Problem creating message: %s" line)))))

(defun yammer-post-buffer-contents ()
  "Posts the contents of the current buffer to yammer.

Useful when using a sperate buffer for composition, possibly with flyspell."
  (interactive)
  (yammer-internal-post-message (buffer-string)))

(defun yammer-post-message (message)
  "Posts to yammer"
  (interactive "sMessage: ")
  (yammer-internal-post-message message))

(defun yammer-reply-to-message (message)
  "Reply to message at point"
  (interactive "sReply to message: ")
  (yammer-internal-post-message message (yammer-current-id)))
  
(defun yammer-delete-message ()
  "Delete message at point"
  (interactive)
  (let ((beg) (line)
        (url-request-method "DELETE")
        (delete-url (format "%s%s" yammer-delete-url-base (yammer-current-id))))
    (set-buffer (oauth-url-retrieve yammer-access-token delete-url))
    (beginning-of-buffer)
    (setq beg (point))
    (end-of-line)
    (setq line (buffer-substring beg (point)))
    (if (string-match "200 OK" line) (message "Message Deleted!")
      (error "Problem deleting message: %s" line))))

(defun hash-val (key alist)
  (cdr (assq key alist)))

(defvar yammer-id-positions nil
  "Ordered list of (position . id) pairs")

(defvar yammer-show-icons nil)
(defvar yammer-user-list nil)
(defvar yammer-tmp-dir
  (expand-file-name (concat "yammer-images") temporary-file-directory))

(defun yammer-fetch-images ()
  (interactive)
  (let ((images (remove-if
                 (lambda (url)
                   (file-exists-p (concat yammer-tmp-dir "/" (yammer-image-name url))))
                 (mapcar
                  (lambda (user) (hash-val 'mugshot_url user))
                  yammer-user-list))))
    (if (not (file-directory-p yammer-tmp-dir))
        (make-directory yammer-tmp-dir))
    (apply 'call-process "wget" nil nil nil
           (format "--directory-prefix=%s" yammer-tmp-dir)
	   "--no-check-certificate"
           "--no-clobber"
           "--quiet"
           images)))

(defun yammer-image-name (url)
  (string-match ".*/\\(.*\\)$" url)
  (match-string 1 url))

(defun yammer-enter ()
  (interactive)
  (let ((uri-in-text (get-text-property (point) 'uri-in-text))
	(reply-id (get-text-property (point) 'reply-id)))
    (cond
     (uri-in-text (browse-url uri-in-text))
     (reply-id (yammer-move-to-id reply-id)))))

(defun yammer-list-messages () 
  "List recent posts"
  (interactive)
  (set-buffer (oauth-fetch-url yammer-access-token yammer-list-url))
  (goto-char (point-min))
  (replace-string "\r" "")
  (goto-char (point-min))  
  (delete-region (point-min) (search-forward "\n\n"))
  (let ((references) (messages)
        (find-user (lambda (id)
                        (find-if (lambda (user) (eq (hash-val 'id user) id))
                                 yammer-user-list)))
        (get-username (lambda (id)
                        (hash-val 'full_name (funcall find-user id))))
        (raw (json-read-from-string 
            (buffer-substring (point-min) (point-max)))))
    (setq references (assq 'references raw))
    (setq yammer-user-list 
          (remove-if-not (lambda (ref)
                           (equal (hash-val 'type ref) "user"))
                         (cdr references)))
    (if yammer-show-icons (yammer-fetch-images))
    (setq messages (assq 'messages raw))
    (switch-to-buffer (get-buffer-create "*yammer-messages*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (setq yammer-id-positions nil)
     (let ((message-alist-by-thread))
       (loop for yamm across (cdr messages) do
             (push `(,(hash-val 'id yamm) ,yamm)
                   message-alist-by-thread))
       (loop for yamm across (cdr messages) do
             (push `(,(point), (hash-val 'id yamm)) yammer-id-positions)
             (when yammer-show-icons 
               (let ((filename (yammer-image-name
                                (hash-val 
                                 'mugshot_url
                                 (funcall find-user 
                                          (hash-val 'sender_id yamm))))))
                 (insert-image-file (concat yammer-tmp-dir "/" filename))
                 (forward-char)
                 (insert "\n")))
	     (insert (yammer-username->string yamm))
	     (insert (yammer-reply-to->string yamm))
	     (insert (format " %s\n"
			     (yammer-make-uri-clickable 
			      (replace-regexp-in-string 
			       "\n"
			       "\n\t"
			       (hash-val 'plain (hash-val 'body yamm))))))
	     (let ((attachments (hash-val 'attachments yamm)))
		   (loop for attachment across attachments do
			 (insert (format "  %s\n"
					 (yammer-attachment->string attachment)))))
	     (insert (yammer-date-and-client->string yamm))
	     (insert "------------\n"))))
  (yammer-messages-mode)
  (beginning-of-buffer))

(defun yammer-display-current-id ()
  (interactive)
  (message "id: %s" (yammer-current-id)))

(defun yammer-current-id ()
  (save-excursion
    (beginning-of-line)
    (cadr
     (find-if
      (lambda (item) (<= (car item) (point))) yammer-id-positions))))

(defun yammer-move-to-id (id)
  (let ((pos
	 (find-if
	  (lambda (item)
	    (eq (car (cdr item)) id)) yammer-id-positions)))
    (if pos 
	(progn
	  (goto-char (car pos))
	  (recenter)))))

(defun yammer-parse-date (date-string)
  "Returns a emacs date for the given time string like what `encode-time' returns"
  (apply 'encode-time
         (mapcar (lambda (val)
                   (if (null val) 1 val))
                 (parse-time-string 
                  (replace-regexp-in-string "/" "-" date-string)))))

(defun yammer-pretty-date (date)
  "Pretty relative time"
  (let* ((now (float-time (current-time)))
        (post-time (float-time date))
        (time-diff (ftruncate (/ (- now post-time) 60))))
    (cond 
     ((< time-diff 1) "1 minute ago")
     ((< time-diff 60) (format "%d minutes ago" time-diff))
     ((< time-diff 120) "1 hour ago")
     ((< time-diff (* 24 60)) (format "%d hours ago" (/ time-diff 60)))
     ((< time-diff (* 48 60)) "1 day ago")
     ((< time-diff (* 7 24 60)) (format "%d days ago" (/ time-diff 60 24)))
     (t (format-time-string "%D %I:%M" date)))))

(define-derived-mode yammer-messages-mode fundamental-mode
  "YammerMessages"
  "Viewing Yammer messages."
  (setq buffer-read-only t)
  (font-lock-mode -1)
  (define-key yammer-messages-mode-map "i" 'yammer-display-current-id)
  (define-key yammer-messages-mode-map "p" 'yammer-post-message)
  (define-key yammer-messages-mode-map "r" 'yammer-reply-to-message)
  (define-key yammer-messages-mode-map "d" 'yammer-delete-message)
  (define-key yammer-messages-mode-map "R" 'yammer-list-messages)
  (define-key yammer-messages-mode-map "l" 'yammer-list-messages)
  (define-key yammer-messages-mode-map "\C-m" 'yammer-enter)
  (defface yammer-uri-face `((t nil)) "" :group 'faces)
  (set-face-attribute 'yammer-uri-face nil :underline t)
  (defface yammer-username-face `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-builtin-face 'yammer-username-face)
  (defface yammer-in-reply-to-face `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-type-face 'yammer-in-reply-to-face)
  (defface yammer-date-face `((t nil)) "" :group 'faces)
  (copy-face 'font-lock-string-face 'yammer-date-face))

(defun yammer-list-message-noninteractive ()
  (save-excursion
    (save-window-excursion
      (progn
	(yammer-list-messages)
	(let ((id (yammer-current-id)))
	  (if (/= id yammer-last-id)
	      (progn
		(setq yammer-last-id id)
		(run-hooks 'yammer-new-tweets-hook))))))))

(defun yammer-start ()
  (interactive)
  (if yammer-timer
      nil
    (progn
      (yammer-list-messages)
      (setq yammer-timer
	    (run-at-time "0 sec"
			 yammer-timer-interval
			 #'yammer-list-message-noninteractive)))))

(defun yammer-stop ()
  (interactive)
  (if yammer-timer
      (progn
	(cancel-timer yammer-timer)
	(setq yammer-timer nil))))

(provide 'yammer)

;;; yammer.el ends here