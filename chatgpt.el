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

(コdefvar chatgpt--insert-position)

(defvar chatgpt--response-position)

(defun chatgpt-handle-response-stream (_beg _end _len)
  "Not documented."
  (save-excursion
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
             (setq chatgpt--response-done t))
            (data
             (progn
               (let* ((data-json (chatgpt-decode-response-data data))
                      (res (mapconcat
                            #'(lambda (choice) (cdr (assq 'content (cdr (assq 'delta choice)))))
                            (cdr (assq 'choices data-json))
                            "")))
                 (let ((res-to chatgpt--response-position))
                   (with-current-buffer chatgpt--insert-buffer
                     (save-excursion
                       (message "%s" res-to)
                       (goto-char res-to)
                       (insert res))))
                 (setq chatgpt--response-position (+ (length res) chatgpt--response-position))
                 )
               (setq chatgpt--response-position end)
               )))
      )
    )
  )

(defconst chatgpt-url-chat "https://api.openai.com/v1/chat/completions")

(defun chatgpt-response-parse-and-insert (insert-buffer insert-pos chatgpt-buffer)
  (with-current-buffer chatgpt-buffer
    (setq-local chatgpt--response-done nil
                chatgpt--insert-buffer insert-buffer
                chatgpt--response-position insert-pos
                chatgpt--response-position 0)
    (add-hook
     'after-change-functions
     #'chatgpt-handle-response-stream
     nil t)
    ))

;; main for testing
(defun chatgpt-test ()
  "chatgpt testing"
  (interactive)
  (let* ((m (chatgpt-add-request-message "system" "one"))
         (m (chatgpt-add-request-message "user" "two" m))
         (m (chatgpt-add-request-message "system" "three" m))
         (m (chatgpt-add-request-message "user" "3の倍数と3が含まれる時だけ馬鹿になるPythonのコードを書いてください" m))
         (d (chatgpt-request-data (reverse m))))

    (chatgpt-response-parse-and-insert
     (buffer-name) (point-max)
     (chatgpt-request chatgpt-url-chat d))
    )
  )

(provide 'chatgpt)

;;; chatgpt.el ends here
