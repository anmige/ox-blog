;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mailcap)
(require 'subr-x)

(defvar org-blog--server-timer-seconds 60)
(defvar org-blog--server-livereload-script "
<script>
fetch(location.pathname, {method: 'POST'}).then(() => location.reload());
</script>")

(defun org-blog--server (project)
  (let* ((name "org-blog--server")
         (port (or (plist-get project :port) 8000))
         (directory (plist-get project :export-directory))
         (plist `(:directory ,directory :server t :ox-blog t))
         (server (or (car (org-blog--server-find directory :server))
                     (make-network-process :name name :filter 'org-blog--server-on-chunk
                                           :log 'org-blog--server-on-connect :plist plist
                                           :server t :service port :family 'ipv4 :noquery t))))
    (run-with-timer org-blog--server-timer-seconds nil 'org-blog--server-kill-on-idle)
    server))

(defun org-blog--kill-idle-servers ()
  (let ((servers (org-blog--server-find nil :server))
        (is-idle (lambda (p) (null (org-blog--server-find (plist-get p :directory) :client))))
        (idle-servers (cl-remove-if-not is-idle servers)))
    (mapc (lambda (server) (delete-process server)) idle-servers))
  (if (> (length servers) (length idle-servers))
      (run-with-timer org-blog--server-timer-seconds nil 'org-blog--server-kill-on-idle)))

(defun org-blog--server-refresh-clients (project)
  (mapcar (lambda (client)
            (with-temp-buffer (org-blog--server-send-response client "text/plain" 200)))
          (org-blog--server-find (plist-get project :export-directory) :client)))

(defun org-blog--server-find (directory property)
  (cl-remove-if-not
   (lambda (proc) (and (process-get proc :ox-blog)
                       (if directory (equal (process-get proc :directory) directory) t)
                       (if property (process-get proc property) t)))
   (process-list)))

(defun org-blog--server-on-connect (server client message)
  (set-process-sentinel client (apply-partially 'org-blog--server-sentinel server))
  (set-process-plist client (list :buffer (generate-new-buffer "*org-blog--server-client*")
                                  :directory (process-get server :directory)
                                  :ox-blog t :client t)))

(defun org-blog--server-sentinel (server client message)
  (unless (string-match-p "^open" message)
    (kill-buffer (process-get client :buffer))))

(defun org-blog--server-on-chunk (client chunk)
  (with-current-buffer (process-get client :buffer)
    (setf (point) (point-max))
    (insert chunk)
    (setf (point) (point-min))
    (if-let ((match (looking-at "\\([^ ]+\\) +\\([^ ]+\\) +[^\r]+\r\n"))
             (method (match-string 1))
             (path (url-unhex-string (match-string 2)))
             (sanitized-path
              (string-join (cl-set-difference (split-string path "/") '("" "..") :test 'equal) "/"))
             (export-path (expand-file-name sanitized-path (process-get client :directory)))
             (full-path (if (file-directory-p export-path)
                            (expand-file-name "index.html" export-path) export-path)))
        (progn
          (erase-buffer)
          (cond ((string= method "POST")) ; hold connection open until export calls refresh
                ((string= method "GET") (org-blog--server-send-file client full-path)))))))

(defun org-blog--server-send-file (client path)
  (with-temp-buffer
    (if (not (file-exists-p path))
        (org-blog--server-send-response client "" 400)
      (let ((mime (mailcap-extension-to-mime (file-name-extension path t))))
        (insert-file-contents path)
        (if (and (string= mime "text/html") (search-forward "<head>" nil t))
            (replace-match (concat "<head>\n" org-blog--server-livereload-script)))
        (org-blog--server-send-response client mime 200)))))

(defun org-blog--server-send-response (client mime status)
  (let ((content-length (buffer-size)))
    (with-temp-buffer
      (insert (format "HTTP/1.1 %d\r\n" status))
      (insert (format "Content-Type: %s\r\n" mime))
      (insert (format "Content-Length: %s\r\n\r\n" content-length))
      (process-send-region client (point-min) (point-max)))
    (process-send-region client (point-min) (point-max))))

(provide 'ox-blog-server)
