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
    ;; (url-retrieve url 'chatgpt-kill-url-buffer nil t))
    (url-retrieve url #'(lambda (_s) (switch-to-buffer (current-buffer))) nil t))
  )

(defun chatgpt-kill-url-buffer (_status)
  "Kill the buffer returned by `url-retrieve'."
  (kill-buffer (current-buffer)))

(require 'generator)

(iter-defun chatgpt-handle-response-stream-gen (pos)
  "Handle chatgpt streaming response.
Start from POS"
  (goto-char pos)
  (let ((done nil)
        (data t))
    (while data
      (re-search-forward "^data: " nil t)
      (let* ((data-json (buffer-substring (point) (line-end-position))))
        (setq data (ignore-errors (chatgpt-decode-response-data data-json)))
        (when (string= data-json "[DONE]")
          (setq done t))
        (when data
          (iter-yield data))))
    (if done nil (line-beginning-position))))

(defun chatgpt-handle-response-stream (buffer pos handler timer-seconds)
  "Handle chatgpt streaming response, from chatgpt response BUFFER and POS ition.
HANDLER is function."
  (with-current-buffer buffer
    (save-excursion
      (let* ((gen (chatgpt-handle-response-stream-gen pos))
             (p (iter-do (i gen) (funcall handler i))))
        (when p
          (run-at-time
           timer-seconds nil
           'chatgpt-handle-response-stream
           buffer p handler timer-seconds))))))

(defun chatgpt-parse-response (data)
  "Extract message from chatgpt response DATA."
  (mapconcat
   #'(lambda (choice) (cdr (assq 'content (cdr (assq 'delta choice)))))
   (cdr (assq 'choices data))
   ""))

;; (let ((buf (current-buffer)))
;;   (chatgpt-handle-response-stream
;;    "hoge" 0
;;    `(lambda (data)
;;       (with-current-buffer ,buf
;;         (insert (chatgpt-parse-response data))))
;;    ))

(defconst chatgpt-url-chat "https://api.openai.com/v1/chat/completions")

(defun chatgpt-response-parse-and-insert (insert-buffer insert-pos chatgpt-buffer)
  "doc"
  (with-current-buffer insert-buffer
    (goto-char insert-pos)
    (chatgpt-handle-response-stream
     chatgpt-buffer 0
     #'(lambda (data)
        (insert (chatgpt-parse-response data)))
     )
    )
  )

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
     (current-buffer) (point-max)
     (chatgpt-request chatgpt-url-chat d))
    )
  )

(provide 'chatgpt)

;;; chatgpt.el ends here


;; (let* ((m (chatgpt-add-request-message "system" "one"))
;;        (m (chatgpt-add-request-message "user" "two" m))
;;        (m (chatgpt-add-request-message "system" "three" m))
;;        (m (chatgpt-add-request-message "user" "3の倍数と3が含まれる時だけ馬鹿になるPythonのコードを書いてください" m))
;;        (d (chatgpt-request-data (reverse m)))
;;        (buf (current-buffer)))

;;   (chatgpt-handle-response-stream
;;    (chatgpt-request chatgpt-url-chat d) 0
;;    `(lambda (data)
;;       (message "%s" (chatgpt-parse-response data))
;;       ;; (with-current-buffer ,buf
;;       ;;   (goto-char (point-max))
;;       ;;   (insert (chatgpt-parse-response data)))
;;       )
;;    1
;;    )
;;   )


;; これは動く
;; (let ((buf (current-buffer)))
;;   (chatgpt-handle-response-stream
;;    "hoge" 0
;;    `(lambda (data)
;;       ;; (message "%s" (chatgpt-parse-response data))
;;       (with-current-buffer ,buf
;;         (goto-char (point-max))
;;         (insert (chatgpt-parse-response data)))
;;       )
;;    1
;;    )
;;   )
