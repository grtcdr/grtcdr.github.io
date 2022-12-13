;; publish.el defines the various components that consitute this
;; website and should be used in conjunction with a build system.

;; Add neighboring directories (and their subdirectories) to the load
;; path, i.e. the dependencies of publish.el.
(normal-top-level-add-subdirs-to-load-path)

;; Import the necessary libraries
(require 'ox-publish)
(require 'shr)
(require 'liaison)

;;; My information:

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn")

;;; Emacs configuration:

(setq make-backup-files nil)

;;; Org mode configuration:

(setq org-export-time-stamp-file nil
      org-publish-list-skipped-files nil
      org-publish-timestamp-directory ".cache/"
      org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil
      org-src-fontify-natively nil
      org-src-preserve-indentation t)

(defun site/sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date."
  (format "%s - [[file:%s][%s]] %s"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)
	  (org-publish-find-property entry :filetags project 'site-html)))

(defun site/get-template (path)
  "Read a template from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src" "templates" path))
    (buffer-string)))

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?d . ,(org-export-data (org-export-get-date info timestamp-format) info))
      (?t . ,(org-export-data (plist-get info :title) info))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?l . ,(liaison-get-resource-url 'log)))))

(defun site/stylesheet (filename)
  "Format filename as a stylesheet."
  (shr-dom-to-xml `(link ((rel . "stylesheet")
			  (href . ,filename)))))

(defvar site/html-head
  (concat
   (site/stylesheet "/css/common.css")
   (site/stylesheet "/css/heading.css")
   (site/stylesheet "/css/nav.css")
   (site/stylesheet "/css/org.css")
   (site/stylesheet "/css/source.css")
   (site/stylesheet "/css/table.css")
   (shr-dom-to-xml '(link ((rel . "icon")
			   (type . "image/x-icon")
			   (href . "/assets/favicon.ico")))))
  "HTML headers shared across projects.")

(setq org-publish-project-alist
      (let ((posts-postamble   (site/get-template "postamble/posts.html"))
	    (posts-preamble    (site/get-template "preamble/main.html"))
	    (content-preamble  (site/get-template "preamble/main.html"))
	    (dotfiles-preamble (site/get-template "preamble/dotfiles.html")))
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
	       :html-head-extra site/html-head
	       :html-preamble content-preamble
	       :html-postamble nil)
	 (list "posts"
	       :base-extension "org"
	       :base-directory "src/posts"
	       :publishing-directory "public/posts"
	       :publishing-function 'org-html-publish-to-html
	       :auto-sitemap t
	       :sitemap-title "Posts"
	       :sitemap-sort-files 'anti-chronologically
	       :sitemap-format-entry 'site/sitemap-format-entry
	       :with-title t
	       :with-toc nil
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-preamble posts-preamble
	       :html-postamble posts-postamble
	       :html-head-extra site/html-head)
	 (list "dotfiles"
	       :recursive t
	       :base-extension "org"
	       :base-directory "src/dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org"))
	       :makeindex t
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-preamble dotfiles-preamble
	       :html-postamble nil
	       :html-head-extra site/html-head)
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
	       :base-directory "src/css" 
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
				 "images"
				 "data")))))
