(require 'cl-lib)

(require 'ert-plus)
(require 'ox-blog)

(defvar org-blog-test--test-directory
  (expand-file-name "playground" (file-name-directory (if load-file-name
                                                          load-file-name
                                                        buffer-file-name))))

(defun org-blog-test--with-project ()
  (let* ((default-directory org-blog-test--test-directory)
         (source-directory (expand-file-name "source"))
         (export-directory (expand-file-name "export"))
         (project (list :with-toc nil
                        :source-directory source-directory
                        :export-directory export-directory)))
    (if (file-directory-p export-directory)
        (delete-directory export-directory 'recursive))
    (make-directory export-directory)
    cb
    (delete-directory export-directory 'recursive)))

(defun org-blog-test--with-server ()
  (let ((server (org-blog--server project)))
    (with-temp-file (expand-file-name "index.html" export-directory)
      (insert "html goes here"))
    cb
    (org-blog--server-exit project)))

(defun org-blog-test--with-cache ()
  (let* ((file (expand-file-name "leftover-file" export-directory))
         (file-node (org-blog--file-node project file)))
    (with-temp-file file)
    (org-blog--index-cache-update project (list file-node))
    cb))

(defun org-blog-test--with-index ()
  (let* ((post-file (expand-file-name "source/post/index.org"))
         (draft-file (expand-file-name "source/.draft/index.org"))
         (org-files (list post-file draft-file))
         (file-nodes (mapcar (apply-partially 'org-blog--file-node project) org-files))
         (org-nodes (mapcar (lambda (file-node)
                              (with-temp-buffer
                                (insert-file-contents (plist-get file-node :source-path))
                                (org-blog--org-node project file-node)))
                            file-nodes)))
    cb))

(describe "org-blog--file-node" (org-blog-test--with-project)
  (it "should return a plist with attributes of the file"
      (let* ((asset-file (expand-file-name "source/asset/main.css"))
             (asset (org-blog--file-node project asset-file)))
        (should (equal (plist-get asset :source-path)
                       (expand-file-name "source/asset/main.css")))
        (should (equal (plist-get asset :export-path)
                       (expand-file-name "export/asset/main.css")))
        (should (equal (plist-get asset :href) "asset/main.css"))
        (should (equal (plist-get asset :extension) ".css"))
        (should (equal (plist-get asset :modified) t))))

  (it "should correctly set the modified attribute"
      (make-directory (expand-file-name "asset" export-directory))
      (with-temp-file (expand-file-name "asset/main.css" export-directory))
      (let* ((asset-file (expand-file-name "source/asset/main.css"))
             (asset (org-blog--file-node project asset-file)))
        (should (equal (plist-get asset :modified) nil)))))


(describe "org-blog--org-node" (org-blog-test--with-project)
  (it "should return a plist with attributes of the org file"
      (let* ((post-file (expand-file-name "source/post/index.org"))
             (file-node (org-blog--file-node project post-file))
             (org-node (with-temp-buffer
                         (insert-file-contents post-file)
                         (org-blog--org-node project file-node))))
        (should (equal '() (cl-set-difference file-node org-node)))
        (should (equal (plist-get org-node :title) "Title"))
        (should (equal (plist-get org-node :subtitle) "Subtitle"))
        (should (equal (format-time-string "%Y-%m-%d" (plist-get org-node :date))
                       "2017-01-01"))
        (should (equal (plist-get org-node :type) "post"))
        (should (equal (plist-get org-node :categories) "a b c"))
        (should (equal (plist-get org-node :draft) nil))))

  (it "should correctly set the draft attribute"
      (let* ((draft-file (expand-file-name "source/.draft/index.org"))
             (file-node (org-blog--file-node project draft-file))
             (org-node (with-temp-buffer
                         (insert-file-contents draft-file)
                         (org-blog--org-node project file-node))))
        (should (equal (plist-get org-node :draft) t)))))

(describe "index-cache-update" (org-blog-test--with-project)
  (it "should update the cache and return both input and cached nodes"
      (should (equal '((:export-path "a"))
                     (org-blog--index-cache-update project '((:export-path "a")))))
      (should (equal '((:export-path "b") (:export-path "a"))
                     (org-blog--index-cache-update project '((:export-path "b")))))
      (should (equal '((:export-path "b") (:export-path "a"))
                     (org-blog--index-cache-update project '())))))


(describe "export-file" (org-blog-test--with-project)
  (it "should copy a non-org file from source-path to export-path and return a file-node"
      (let* ((file (expand-file-name "asset/main.css" source-directory))
             (file-node (org-blog--file-node project file)))
        (should (not (file-exists-p (plist-get file-node :export-path))))
        (should (equal file-node (org-blog--export-file project file-node)))
        (should (file-exists-p (plist-get file-node :export-path)))))

  (it "should export an org file to export-path and return an org-node"
      (let* ((file (expand-file-name "post/index.org" source-directory))
             (file-node (org-blog--file-node project file)))
        (should (not (file-exists-p (plist-get file-node :export-path))))
        (let ((org-node (org-blog--export-file project file-node)))
          (should (equal (plist-get org-node :title) "Title")))
        (should (file-exists-p (plist-get file-node :export-path))))))


(describe "server" (org-blog-test--with-project
                    org-blog-test--with-server)
  (it "should respond to GET"
      (let* ((url "http://localhost:8000/index.html")
             (response (with-current-buffer (url-retrieve-synchronously url) (buffer-string)))
             (lines (split-string response "\n")))
        (should (equal lines '("HTTP/1.1 200"
                               "Content-Type: text/html"
                               "Content-Length: 14"
                               ""
                               "html goes here")))))

  (it "should respond with index.html if path does not exist"
      (let* ((url "http://localhost:8000")
             (response (with-current-buffer (url-retrieve-synchronously url) (buffer-string)))
             (lines (split-string response "\n")))
        (should (equal lines '("HTTP/1.1 200"
                               "Content-Type: text/html"
                               "Content-Length: 14"
                               ""
                               "html goes here")))))

  (it "should not respond to POST until called explicitly"
      (let ((url "http://localhost:8000/index.html")
            (url-request-method "POST")
            (response-cb (lambda (_status) (setq resolve-time (current-time)
                                                 response (buffer-string))))
            (timer-cb (lambda (project) (setq refresh-time (current-time))
                        (org-blog--server-refresh-clients project)))
            refresh-time resolve-time response)
        (url-retrieve url response-cb)
        (run-with-timer 0.005 nil timer-cb project)
        (should (and (not refresh-time) (not resolve-time)))
        (sleep-for 0.01)
        (should (time-less-p refresh-time resolve-time))
        (should (equal (split-string response "\n") '("HTTP/1.1 200"
                                                      "Content-Type: text/plain"
                                                      "Content-Length: 0"
                                                      ""
                                                      "")))))

  (it "should close server and children on exit"
      (let ((url "http://localhost:8000/index.html")
            (url-request-method "POST")
            (response-cb (lambda (_status))))
        (mapcar (lambda (_) (url-retrieve url response-cb)) '(1 2 3 4 5))
        (sleep-for 0.005)
        (should (equal 5 (length (org-blog--server-find-process project t))))
        (org-blog--server-exit project)
        (should (equal 0 (length (org-blog--server-find-process project t))))
        (should (equal 0 (length (org-blog--server-find-process project nil))))))

  (it "should only start one server per project"
      (let ((serverA server)
            (serverB (org-blog--server project)))
        (should (equal serverA serverB)))))


(describe "org-blog--directory-files" (org-blog-test--with-project)
  (it "should find all files recursively"
        (let* ((files (org-blog--directory-files source-directory))
               (relative-files (mapcar (lambda (f) (file-relative-name f source-directory)) files)))
          (should (equal relative-files '("asset/main.css" "post/index.org" ".draft/index.org")))))

    (it "should exclude excluded files and directories"
        (let* ((files (org-blog--directory-files source-directory '(".draft/" "main.css")))
               (relative-files (mapcar (lambda (f) (file-relative-name f source-directory)) files)))
          (should (equal relative-files '("post/index.org"))))))

(describe "org-blog--export" (org-blog-test--with-project)
  (it "should only export modified"
      (let ((export-1-nodes (org-blog--export project))
            (export-2-nodes (org-blog--export project)))
        (should (not (equal 0 (length export-1-nodes))))
        (should (equal 0 (length export-2-nodes)))))

  (it "should export everything if force is truthy"
      (let ((export-1-nodes (org-blog--export project nil t))
            (export-2-nodes (org-blog--export project nil t)))
        (should (not (equal 0 (length export-1-nodes))))
        (should (not (equal 0 (length export-2-nodes))))))

  (it "should only export non-drafts to index if production is truthy"
      (org-blog--export project t t)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "index.html" export-directory))
        (keep-lines "</li>")
        (let ((lis (split-string (buffer-string) "\n" t)))
          (should (equal (length lis) 1))))))


(describe "org-blog--export-cleanup" (org-blog-test--with-project
                                      org-blog-test--with-cache)
  (it "should delete files that exist in export but not source from disk and cache"
      (let ((cache-nodes))
        (org-blog--index-cache-wrap project
          (maphash (lambda (node _) (push node cache-nodes)) cache))
        (should (equal (length cache-nodes) 1)))
      (org-blog--export-cleanup project '())
      (let ((cache-nodes))
        (org-blog--index-cache-wrap project
          (maphash (lambda (node _) (push node cache-nodes)) cache))
        (should (equal (length cache-nodes) 0)))))


(describe "org-blog--index" (org-blog-test--with-project
                             org-blog-test--with-index)
  (it "should list all posts (inluding drafts) by default"
      (org-blog--index project org-nodes nil)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "index.html" export-directory))
        (keep-lines "</li>")
        (let ((lis (split-string (buffer-string) "\n" t)))
          (should (equal (length lis) 2)))))

  (it "should only list posts (not drafts) if arg production is truthy"
      (org-blog--index project org-nodes t)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "index.html" export-directory))
        (keep-lines "</li>")
        (let ((lis (split-string (buffer-string) "\n" t)))
          (should (equal (length lis) 1))))))


(describe "org-blog--run" ()
  (it "should error with message"
      (let ((err (should-error (org-blog--run "foobar"))))
        (should (equal "Command failed: /bin/bash: foobar: command not found\n" (cadr err)))))

  (it "should succeed with return value"
      (should (equal "10\n" (org-blog--run "echo 10")))))


;; FIXME: wrong git is committed & pushed!
;; (describe "org-blog-export-publish" (org-blog-test--with-project)
;;   (it "should error if export-directory is not a git repository"
;;       (should-error (org-blog-publish project)))

;;   (it "should commit and push changes"
;;       (let ((default-directory export-directory)
;;             (org-blog-project project))
;;         (org-blog--run "git init --bare ../remote")
;;         (org-blog--run "git init")
;;         (org-blog--run (format "git remote add origin %s" (expand-file-name "../remote")))
;;         (unwind-protect
;;             (progn
;;               (should-error (org-blog--run "cd ../remote && git log"))
;;               (org-blog-publish)
;;               (should (string-match-p "commit" (org-blog--run "cd ../remote && git log"))))
;;           (org-blog--run "rm -rf ../remote")))))
