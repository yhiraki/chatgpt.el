;;; chatgpt.el --- Use ChatGPT inside Emacs -*- lexical-binding: t; -*-)

;; Author: yhiraki <coffexpr@gmail.com>
;; URL: https://github.com/yhiraki/chatgpt.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.2"))
;; Keywords: openai
;; License: MIT

;;; Commentary:

;; Use ChatGPT inside Emacs

;;; Code:

(require 'json)

(defcustom chatgpt-api-token nil
  "OpenAPI token."
  :type 'string
  :group 'chatgpt-)

(defun chatgpt-add-request-message (role content &optional messages)
  (let* ((messages (if messages messages ()))
         (msg `((:role . ,role)(:content . ,content))))
    (push msg messages)))

(defun chatgpt-request-data (messages)
  `((:model . "gpt-3.5-turbo")
    (:stream . t)
    (:messages . ,messages)))

(defun chatgpt-encode-request-data (data)
  (encode-coding-string
   (json-encode-alist data)
   'utf-8))

(defun chatgpt-decode-response-data (data)
  (json-read-from-string
   (decode-coding-string data 'utf-8)))

(defun chatgpt-request-headers ()
  `(("Content-Type" . "application/json")
    ("Authorization" . ,(format "Bearer %s" chatgpt-api-token))))

(defun chatgpt-request (url data)
  "Send DATA to URL as a POST request."
  (let ((url-request-method "POST")
        (url-request-extra-headers (chatgpt-request-headers))
        (url-request-data (chatgpt-encode-request-data data)))
    (url-retrieve url 'chatgpt-kill-url-buffer nil t))
  )

(defun chatgpt-kill-url-buffer (status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(defvar chatgpt--response-done)

(defvar chatgpt--insert-buffer)

(defvar chatgpt--insert-position)

(defvar chatgpt--response-position)

(defvar chatgpt--response-lock)

(defun chatgpt-handle-response-stream (_beg _end _len)
  "Not documented."
  (when (and (not chatgpt--response-done)
             (not chatgpt--response-lock))
    (save-excursion
      (setq-local chatgpt--response-lock t)
      (goto-char chatgpt--response-position)
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
               (message "DONE")
               (setq-local chatgpt--response-done t))
              (data
               (progn
                 (let* ((data-json (chatgpt-decode-response-data data))
                        (res (mapconcat
                              #'(lambda (choice) (cdr (assq 'content (cdr (assq 'delta choice)))))
                              (cdr (assq 'choices data-json))
                              "")))
                   (let ((res-to chatgpt--insert-position))
                     (with-current-buffer chatgpt--insert-buffer
                       (save-excursion
                         (goto-char res-to)
                         (insert res))))
                   (setq chatgpt--insert-position (+ (length res) chatgpt--insert-position))
                   )
                 (setq chatgpt--response-position end)
                 ))))
      (setq-local chatgpt--response-lock nil)
      )))

(defconst chatgpt-url-chat "https://api.openai.com/v1/chat/completions")

(defun chatgpt-response-parse-and-insert (insert-buffer insert-pos chatgpt-buffer)
  (with-current-buffer chatgpt-buffer
    (setq-local chatgpt--response-done nil
                chatgpt--insert-buffer insert-buffer
                chatgpt--insert-position insert-pos
                chatgpt--response-position 0
                chatgpt--response-lock nil)
    (add-hook
     'after-change-functions
     #'chatgpt-handle-response-stream
     nil t)
    ))

;; main for testing
(defun chatgpt-test ()
  "Chatgpt testing function."
  (interactive)
  (let* ((m `(((:role . "user")(:content . ,(read-from-minibuffer "PROMPT: ")))))
         (d (chatgpt-request-data m)))
    (chatgpt-response-parse-and-insert
     (buffer-name) (point)
     (chatgpt-request chatgpt-url-chat d))
    ))

(provide 'chatgpt)

;;; chatgpt.el ends here
