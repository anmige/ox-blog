;;; -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'org)
(require 'ox)

(defvar org-blog--template-index "
#+TITLE: Home
#+BEGIN_EXPORT html
<ul class='index'>
%i
</ul>
%j
#+END_EXPORT")

(defvar org-blog--template-index-item "
<li class='%s' data-categories='%c'>
  <a href='%h'>
    <date>%d</date>
    <span>%t</span>
  </a>
</li>
")

(defvar org-blog--template-category-js "
<script>
let originalTitle = document.querySelector('.title').textContent;
function updateIndex() {
  const category = window.location.hash.slice(1);
  if (category) {
    document.querySelector('.title').textContent =
      category.charAt(0).toUpperCase() + category.slice(1);
    document.querySelectorAll('.index li')
      .forEach(li => {
        const categories = li.dataset.categories.split(' ');
        if (categories.includes(category)) {
          li.style.display = 'block';
        } else {
          li.style.display = 'none';
        }
      });
  } else {
    document.querySelector('.title').textContent = originalTitle;
    document.querySelectorAll('.index li').forEach(li => {
      li.style.display = 'block';
    });
  }
}

window.addEventListener('hashchange', updateIndex);
updateIndex();
</script>")


(defun org-blog--index (project nodes production)
  (let* ((html-file (expand-file-name "index.html" (plist-get project :export-directory)))
         (html-head (or (plist-get project :index-head) (plist-get project :html-head)))
         (project (plist-put (copy-tree project) :html-head html-head))
         (nodes (cl-remove-if-not (lambda (x) (equal (plist-get x :type) "post")) nodes))
         (nodes (org-blog--index-cache-update project nodes))
         (nodes (cl-remove-if (lambda (x) (and production (plist-get x :draft))) nodes))
         (nodes (sort nodes (lambda (a b) (time-less-p (plist-get b :date) (plist-get a :date))))))
    (with-temp-buffer
      (insert (org-blog--index-format nodes))
      (let ((html (org-export-as 'html nil nil nil project)))
        (with-temp-file html-file (insert html))))))

(defun org-blog--index-format (org-nodes)
  (format-spec org-blog--template-index
               `((?i . ,(mapconcat 'org-blog--index-format-item org-nodes "\n"))
                 (?j . ,org-blog--template-category-js))))

(defun org-blog--index-format-item (org-node)
  (format-spec org-blog--template-index-item
               `((?s . ,(if (plist-get org-node :draft) "draft" "post"))
                 (?h . ,(plist-get org-node :href))
                 (?d . ,(format-time-string "%Y-%m-%d" (plist-get org-node :date)))
                 (?t . ,(plist-get org-node :title))
                 (?c . ,(plist-get org-node :categories)))))

(defmacro org-blog--index-cache-wrap (project &rest body)
  (declare (indent 1))
  `(let* ((cache-file (expand-file-name "cache.el" (plist-get project :export-directory)))
          (cache (ignore-errors (with-temp-buffer (insert-file-contents cache-file)
                                                  (read (buffer-string)))))
          (cache (or cache (make-hash-table :test 'equal)))
          print-level
          print-length)
     (let ((result (progn ,@body)))
       (with-temp-file cache-file (insert (prin1-to-string cache)))
       result)))

(defun org-blog--index-cache-update (project nodes)
  (org-blog--index-cache-wrap project
    (let* ((cache-nodes '())
           (update-cache (lambda (node) (puthash (plist-get node :export-path) node cache)))
           (push-cache-node (lambda (file node) (push node cache-nodes))))
      (mapcar update-cache nodes)
      (maphash push-cache-node cache)
      cache-nodes)))

(provide 'ox-blog-index)
