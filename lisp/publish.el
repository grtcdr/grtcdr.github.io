;; publish.el defines the various components that consitute this
;; website and should be used in conjunction with a build system.

;; This publishing script recognizes the following environment variables:
;;
;; *  WITH_PDF:  Enable PDF export of dotfiles
;; *  CI:        Inform this script that it is being run in a CI context

(normal-top-level-add-subdirs-to-load-path)

(require 'ox-publish)
(require 'shr)
(require 'liaison)
(require 'project)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (package-install 'ini-mode))

(defun env/enabled? (env)
  (not (null (member env '("yes" "true")))))

(setq env/ci (getenv "CI")
      env/with-pdf (getenv "WITH_PDF"))

(defun site/get-plantuml-jar-path ()
  "Determine the path of the PlantUML JAR file."
  (format "/usr/share/%s/plantuml.jar"
	  (if env/ci "plantuml" "java/plantuml")))

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil)

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
      org-confirm-babel-evaluate #'site/should-lang-confirm?
      org-plantuml-jar-path (site/get-plantuml-jar-path))

(setq org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-html-html5-fancy t)

(setq org-latex-pdf-process '("tectonic %f"))

(defun site/posts-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
posts publishing project."
  (format "%s - [[file:%s][%s]] %s"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)
	  (org-publish-find-property entry :filetags project)))

(defun site/dotfiles-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
dotfiles publishing project."
  (let* ((title (org-publish-find-title entry project))
	(description (org-publish-find-property entry :description project 'html))
	(link (format "[[file:%s][%s]]" entry title)))
    (if description
	(concat link " -- " description "\n")
      entry)))

(defvar site/dotfiles-sitemap-title
  "Peek through my =~/.dotfiles= and into the inner workings of my system")

(defun site/dotfiles-sitemap-function (title list)
  "Custom sitemap function for the dotfiles publishing project."
  (concat "#+OPTIONS: html-postamble:nil\n"
	  (org-publish-sitemap-default title list)))

(defun site/posts-sitemap-function (title list)
  "Custom sitemap function for the posts publishing project."
  (concat "#+OPTIONS: html-postamble:nil html-preamble:nil\n"
	  (org-publish-sitemap-default title list)))

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
    `((?C . ,(let ((file (plist-get info :input-file)))
	       (format-time-string timestamp-format
				   (and file (file-attribute-modification-time
					      (file-attributes file))))))
      (?d . ,(org-export-data (org-export-get-date info timestamp-format) info))
      (?t . ,(org-export-data (plist-get info :title) info))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(plist-get info :email))
      (?E . ,(liaison-get-resource-url 'edit))
      (?l . ,(liaison-get-resource-url 'log))
      (?b . ,(liaison-get-resource-url 'blob)))))

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
  "HTML headers shared across publishing projects.")

(setq org-publish-project-alist
      (let* ((footer (site/get-template "footer.html"))
	     (posts-postamble (site/get-template "postamble/posts.html"))
	     (posts-preamble (site/get-template "preamble/main.html"))
	     (content-preamble (site/get-template "preamble/main.html"))
	     (dotfiles-preamble (site/get-template "preamble/dotfiles.html"))
	     (dotfiles-postamble (site/get-template "postamble/dotfiles.html")))
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
	       :sitemap-title "Read some of my writings"
	       :sitemap-sort-files 'anti-chronologically
	       :sitemap-format-entry 'site/posts-sitemap-format-entry
	       :sitemap-function 'site/dotfiles-sitemap-function
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
	       :sitemap-title site/dotfiles-sitemap-title
	       :sitemap-style 'list
	       :sitemap-format-entry 'site/dotfiles-sitemap-format-entry
	       :sitemap-function 'site/dotfiles-sitemap-function
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-preamble dotfiles-preamble
	       :html-postamble dotfiles-postamble
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
	 (list "cv"
	       :base-extension "pdf"
	       :base-directory "src/cv"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment)
	 (unless (not env/with-pdf)
	   (list "dotfiles--pdf"
		 :base-extension "org"
		 :base-directory "src/dotfiles"
		 :publishing-directory "public/dotfiles"
		 :publishing-function 'org-latex-publish-to-pdf
		 :exclude "sitemap.org"
		 :recursive t
		 :with-date nil))
	 (list "all"
	       :components (list "content"
				 "posts"
				 "dotfiles"
				 (unless (not env/with-pdf)
				   "dotfiles--pdf")
		       		 "stylesheets"
				 "javascripts"
				 "images"
				 "data"
				 "cv")))))
