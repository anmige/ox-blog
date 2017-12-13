;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)

(require 'org)
(require 'ox)

(require 'ox-blog-server)
(require 'ox-blog-index)

(if (version< org-version "9") (error "org-blog requires org version >= 9"))

(defvar org-blog-project '())

(org-export-define-derived-backend 'blog 'html
  :options-alist '((:categories "CATEGORIES") (:type "TYPE"))
  :translate-alist '((inner-template . org-blog--html-inner-template)))

(defun org-blog--html-inner-template (contents-html info)
  (let* ((categories (split-string (or (plist-get info :categories) "") " " t))
         (href (plist-get (plist-get info :org-node) :href))
         (root (file-relative-name "/" (concat "/" (file-name-directory href))))
         (lis (mapcar (lambda (c) (format "<li><a href='%s#%s'>%s</a></li>" root c c)) categories))
         (categories-html (string-join `("<ul class='categories'>" ,@lis "</ul>") "\n")))
    (concat categories-html contents-html (org-html-footnote-section info))))

(defun org-blog--export-file (project file-node)
  (let* ((export-directory (file-name-directory (plist-get file-node :export-path))))
    (unless (file-directory-p export-directory) (mkdir export-directory t))
    (if (equal (plist-get file-node :extension) ".org")
        (org-blog--export-org-file project file-node)
      (org-blog--export-asset-file project file-node))))

(defun org-blog--export-asset-file (project file-node)
  (copy-file (plist-get file-node :source-path) (plist-get file-node :export-path) t)
  file-node)

(defun org-blog--export-org-file (project file-node)
  (let* ((org-inhibit-startup t)
         (org-babel-default-header-args (org-combine-plists
                                         org-babel-default-header-args
                                         (plist-get project :babel-header-args)))
         (org-export-babel-evaluate (plist-get project :babel-evaluate))
         (org-file (plist-get file-node :source-path))
         (default-directory (file-name-directory org-file))
         (visited (find-buffer-visiting org-file))
         (buffer (find-file-noselect org-file)))
    (with-current-buffer buffer
      (let* ((org-node (org-blog--org-node project file-node))
             (extended-project (append (list :org-node org-node) project))
             (html (org-export-as 'blog nil nil nil extended-project)))
        (with-temp-file (plist-get org-node :export-path) (insert html))
        (unless visited (kill-buffer buffer))
        org-node))))

(defun org-blog--org-node (project file-node)
  (let* ((plist (org-export-get-environment 'blog))
         (org-date (assq 'timestamp (plist-get plist :date)))
         (date (if org-date (date-to-time (org-timestamp-format org-date "%Y%m%dT%H%M%S"))))
         (draft (null date))
         (type (org-no-properties (plist-get plist :type)))
         (categories (org-no-properties (plist-get plist :categories)))
         (title (org-no-properties (car (plist-get plist :title))))
         (subtitle (org-no-properties (car (plist-get plist :subtitle)))))
    (org-combine-plists file-node (list :draft draft :date date :type type :title title
                                        :subtitle subtitle :categories categories))))

(defun org-blog--file-node (project source-path)
  (let* ((relative-path (file-relative-name source-path (plist-get project :source-directory)))
         (extension (file-name-extension source-path t))
         (relative-export-path (if (equal extension ".org")
                                   (concat (file-name-sans-extension relative-path) ".html")
                                 relative-path))
         (export-path (expand-file-name relative-export-path (plist-get project :export-directory)))
         (source-modified (nth 5 (file-attributes source-path)))
         (export-modified (nth 5 (file-attributes export-path)))
         (modified (or (not export-modified) (time-less-p export-modified source-modified))))
    (list :source-path source-path :export-path export-path
          :href relative-export-path :extension extension :modified modified)))

(defun org-blog--export (project &optional production force)
  (org-blog--validate-project project)
  (let* ((files (org-blog--directory-files (plist-get project :source-directory)))
         (file-nodes (mapcar (apply-partially 'org-blog--file-node project) files))
         (modified-p (lambda (node) (plist-get node :modified)))
         (export-nodes (mapcar (apply-partially 'org-blog--export-file project)
                               (if force file-nodes (cl-remove-if-not modified-p file-nodes)))))
    (org-blog--server project)
    (org-blog--export-cleanup project file-nodes)
    (org-blog--index project export-nodes production)
    (org-blog--server-refresh-clients project)
    export-nodes))

(defun org-blog--validate-project (project)
  (mapcar (lambda (key) (cl-assert (plist-member project key) t))
          '(:source-directory :export-directory)))

(defun org-blog--export-cleanup (project file-nodes)
  (let* ((export-directory (plist-get project :export-directory))
         (exported-files (org-blog--directory-files export-directory))
         (export-files (mapcar (lambda (node) (plist-get node :export-path)) file-nodes))
         (extra-files (mapcar (lambda (f) (expand-file-name f export-directory))
                              '("cache.el" ".gitignore")))
         (expected-files (append extra-files export-files))
         (leftover-files (cl-set-difference exported-files expected-files :test 'equal))
         (cleanup (lambda (cache file _) (if (member file leftover-files) (remhash file cache)))))
    (org-blog--index-cache-wrap project (maphash (apply-partially cleanup cache) cache))
    (mapcar (lambda (file) (delete-file file t)) leftover-files)))

(defun org-blog--directory-files (directory &optional excluded)
  (let ((files nil))
    (dolist (file (file-name-all-completions "" directory))
      (unless (member file (append '("./" "../") excluded))
        (let ((full-file (expand-file-name file directory)))
          (if (directory-name-p full-file)
              (setq files (nconc files (org-blog--directory-files full-file excluded)))
            (push full-file files)))))
    files))

(defun org-blog-publish ()
  (interactive)
  (let* ((default-directory (plist-get org-blog-project :export-directory))
         (export-nodes (org-blog--export org-blog-project t))
         (git-p (file-directory-p ".git")))
    (unless git-p (error "Please setup a .git repository inside the export directory."))
    (org-blog--run "git add --all")
    (org-blog--run "git commit -m \"$(date +%Y-%m-%d)\"")
    (org-blog--run "git push -u origin $(git rev-parse --abbrev-ref HEAD)")))

(defun org-blog-export (&optional production force)
  (interactive)
  (org-blog--export org-blog-project production force))

(defun org-blog--run (command)
  (let ((stdout (generate-new-buffer "stdout"))
        (stderr (generate-new-buffer "stderr")))
    (if (eq 0 (shell-command command stdout stderr))
        (with-current-buffer stdout (buffer-string))
      (error "Command failed: %s" (with-current-buffer stderr (buffer-string))))))

(define-minor-mode org-blog-mode
  "export on save"
  :lighter " ox-blog"
  :global t
  (if org-blog-mode
      (add-hook 'after-save-hook 'org-blog-export)
    (remove-hook 'after-save-hook 'org-blog-export)
    (org-blog--server-exit org-blog-project)))

(provide 'ox-blog)
