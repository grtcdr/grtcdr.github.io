;; site-spec.el defines the various components that consitute this
;; website and should be used in conjunction with a build system.

(normal-top-level-add-subdirs-to-load-path)

(require 'forgecast)
(require 'htmlize)
(require 'project)
(require 'ox-publish)

(setq default-directory
      (expand-file-name
       (project-root (project-current))))

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil
      org-export-time-stamp-file nil
      org-publish-timestamp-directory ".cache/"
      org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type 'inline-css
      org-src-fontify-natively t
      org-src-preserve-indentation t)

(defun publish-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date."
  (let ((prefix (file-name-concat (project-root (project-current)) "posts")))
    (format "%s - [[file:%s][%s]] %s"
	    (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	    entry
	    (org-publish-find-title entry project)
	    (org-publish-find-property entry :filetags project 'site-html))))

(defun publish-format-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself."
  ;; Copyright: Toon Claes
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (if id
                   (format "<a href=\"#%s\">%s</a>" id text)
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(defun publish-read-template (slug)
  "Read a template from the templates directory."
    (with-temp-buffer
      (insert-file-contents
       (file-name-concat "src" "templates" slug))
      (buffer-string)))

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?d . ,(org-export-data (org-export-get-date info timestamp-format) info))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?l . ,(forgecast-get-resource-url 'log)))))

(defun publish-html-head ()
  (string-join
   '("<link rel=\"stylesheet\" href=\"/css/main.css\" />"
     "<link rel=\"icon\" type=\"image/x-icon\" href=\"/assets/favicon.ico\" />")
   "\n"))

(setq org-publish-project-alist
      (let ((posts-postamble   (publish-read-template "postamble/posts.html"))
	    (posts-preamble    (publish-read-template "preamble/posts.html"))
	    (content-preamble  (publish-read-template "preamble/content.html"))
	    (dotfiles-preamble (publish-read-template "preamble/dotfiles.html"))
	    (html-head         (publish-html-head)))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :section-numbers nil
	       :with-toc nil
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "posts"
	       :base-extension "org"
	       :base-directory "src/posts"
	       :publishing-directory "public/posts"
	       :publishing-function 'org-html-publish-to-html
	       :auto-sitemap t
	       :sitemap-title "Posts"
	       :sitemap-sort-files 'anti-chronologically
	       :sitemap-format-entry 'publish-sitemap-format-entry
	       :with-title t
	       :with-toc nil
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-format-headline-function 'publish-format-headline-function
	       :html-preamble posts-preamble
	       :html-postamble posts-postamble
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "dotfiles"
	       :recursive t
	       :base-extension "org"
	       :base-directory "src/dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org"))
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-format-headline-function 'publish-format-headline-function
	       :html-preamble dotfiles-preamble
	       :html-postamble nil
	       :html-head-extra html-head
	       :html-head-include-default-style nil)
	 (list "data"
	       :base-extension (regexp-opt '("ico" "txt" "pdf" "asc"))
	       :base-directory "assets"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment)
	 (list "images"
	       :base-extension (regexp-opt '("png" "jpg" "jpeg" "svg"))
	       :base-directory "assets/images"
	       :publishing-directory "public/assets/images"
	       :publishing-function 'org-publish-attachment
	       :recursive t)
	 (list "stylesheets"
	       :base-extension "css"
	       :base-directory "src/less" 
	       :publishing-directory "public/css"
	       :publishing-function 'org-publish-attachment)
	 (list "javascripts"
	       :base-extension "js"
	       :base-directory "src/js"
	       :exclude "grunt.js"
	       :publishing-directory "public/js"
	       :publishing-function 'org-publish-attachment)
	 (list "all"
	       :components (list "content"
				 "posts"
				 "dotfiles"
				 "stylesheets"
				 "javascripts"
				 "data"
				 "images")))))

(provide 'site-spec)
