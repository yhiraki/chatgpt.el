(defun my-url-http-post (url args)
  "Send ARGS to URL as a POST request."
  (let ((url-request-method "POST")
		(url-request-extra-headers
		 `(("Content-Type" . "application/json")
		   ("Authorization" . ,(format "Bearer %s" org-babel-chatgpt-api-token))))
		(url-request-data (json-encode-alist `(:model "gpt-3.5-turbo" :stream t :messages [((:role . "user")(:content . "hi"))]))))
	;; if you want, replace `my-switch-to-url-buffer' with `my-kill-url-buffer'
	;; (url-retrieve url 'my-switch-to-url-buffer nil t)
	(url-retrieve url 'my-kill-url-buffer nil t)
	))

(defun my-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defun my-switch-to-url-buffer (status)
  "Switch to the buffer returned by `url-retreive'.
    The buffer contains the raw HTTP response sent by the server."
  (switch-to-buffer (current-buffer)))

(defun call-chatgpt-and-insert (buffer pos)
  (with-current-buffer (my-url-http-post "https://api.openai.com/v1/chat/completions" nil)
	(setq res-buffer buffer)
	(setq res-pos pos)
	(setq bbeg 0)
	(make-variable-buffer-local 'res-pos)
	(make-variable-buffer-local 'res-buffer)
	(make-variable-buffer-local 'bbeg)
	(add-hook
	 'after-change-functions
	 #'(lambda (_beg _end _len)
		 (save-excursion
		   (goto-char bbeg)
		   (let* ((beg (ignore-errors (re-search-forward "^data: ")))
				  (has-end (when beg
							 (goto-char beg)
							 (search-forward "\n\n")))
				  (end (when has-end
						 (goto-char beg)
						 (re-search-forward "$")))
				  (data (when end (buffer-substring beg end)))
				  )
			 (cond ((string= data "[DONE]")
					(message "DONE"))
				   (data
					(progn
					  (let* ((data-json (json-read-from-string data))
							 (res (mapconcat
								   #'(lambda (choice) (cdr (assq 'content (cdr (assq 'delta choice)))))
								   (cdr (assq 'choices data-json))
								   "")))
						(message "%s" res)
						(let ((res-to res-pos))
						  (with-current-buffer res-buffer
							(save-excursion
							  (message "%s" res-to)
							  (goto-char res-to)
							  (insert res))))
						(setq res-pos (+ (length res) res-pos))
						)
					  (setq bbeg end)
					  )))
			 )
		   )
		 )
	 nil t)
	)
  )

(call-chatgpt-and-insert (buffer-name) (point-max))
