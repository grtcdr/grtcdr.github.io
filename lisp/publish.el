;; publish.el defines the various components that consitute this
;; website and should be used in conjunction with a build system.

;; Add neighboring directories (and their subdirectories) to the load
;; path, i.e. the dependencies of publish.el.
(normal-top-level-add-subdirs-to-load-path)

;; Import the necessary libraries
(require 'ox-publish)
(require 'shr)
(require 'liaison)
(require 'project)

;;; My information:

;;; Emacs configuration:

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil)

;;; Org mode configuration:

(defun site/should-lang-confirm? (lang body)
  "Returns non-nil if LANG needs to confirm before babel may evaluate BODY."
  (not (member lang '("dot" "plantuml"))))

(org-babel-do-load-languages 'org-babel-load-languages
                             '((plantuml . t)
			       (dot      . t)))

(setq org-publish-list-skipped-files nil
      org-publish-timestamp-directory ".cache/"
      org-export-time-stamp-file nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-plantuml-jar-path (format "/usr/share/%s/plantuml.jar"
				    (if (string= user-login-name "runner")
					"plantuml"
				      "java/plantuml")) 
      org-confirm-babel-evaluate #'site/should-lang-confirm?)

(defun site/posts-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
posts project."
  (format "%s - [[file:%s][%s]] %s"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)
	  (org-publish-find-property entry :filetags project)))

(defun site/dotfiles-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
dotfiles project."
  (let ((title (org-publish-find-title entry project))
	(description (org-publish-find-property entry :description project))
	(filetags (org-publish-find-property entry :filetags project)))
    (string-trim
     (format-spec "[[file:%e][%t]] %d %f"
		  `((?e . ,entry)
		    (?t . ,title)
		    (?d . ,(if description (format "- %s" description) ""))
		    (?f . ,(or filetags "")))))))

(defun site/get-template (path)
  "Read a template from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src/templates" path))
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
   (site/stylesheet "/css/figure.css")
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
	       :sitemap-format-entry 'site/posts-sitemap-format-entry
	       :with-title t
	       :with-toc nil
	       :html-preamble posts-preamble
	       :html-postamble posts-postamble
	       :html-head-extra site/html-head)
	 (list "dotfiles"
	       :base-extension "org"
	       :base-directory "src/dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org"))
	       :recursive t
	       :auto-sitemap t
	       :sitemap-title "Dotfiles"
	       :sitemap-style 'list
	       :sitemap-format-entry 'site/dotfiles-sitemap-format-entry
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-preamble dotfiles-preamble
	       :html-postamble nil
	       :html-head-extra site/html-head)
	 (list "data"
	       :base-extension ".*"
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
