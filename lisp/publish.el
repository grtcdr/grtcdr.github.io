;; publish.el defines the various components that consitute this
;; website and should be used in conjunction with a build system.

;; This publishing script recognizes the following environment variables:
;;
;; *  CI:   Inform this script that it is being run in a CI context.

(normal-top-level-add-subdirs-to-load-path)

(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(setq package-list
      '(ini-mode toml-mode citeproc))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Publishing
(require 'ox-publish)
(require 'oc)
(require 'oc-csl)
(require 'citeproc)
(require 'org-id)
;; XML templating
(require 'shr)
;; URL generation
(require 'liaison)

(defun env/enabled? (env)
  (length> (member env '("yes" "true")) 0))

(setq env/ci (getenv "CI"))

(defun s/get-plantuml-jar-path ()
  "Determine the path of the PlantUML JAR file."
  (format "/usr/share/%s/plantuml.jar"
	  (if (env/enabled? env/ci)
	      "plantuml"
	    "java/plantuml")))

(defun s/should-lang-confirm? (lang body)
  "Return non-nil if LANG needs to confirm before babel may evaluate BODY."
  (not (member lang '("dot" "plantuml"))))

(defun s/asset-global-macro (filename)
  (concat default-directory "assets" filename))

(defun s/posts-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
posts publishing project."
  (format "%s - [[file:%s][%s]]"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)))

(defun s/dotfiles-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
dotfiles publishing project."
  (let* ((title (org-publish-find-title entry project))
	(description (org-publish-find-property entry :description project 'html))
	(link (format "[[file:%s][%s]]" entry title)))
    (if description
	(concat link " -- " description "\n")
      entry)))

(defvar s/dotfiles-sitemap-title
  "Peek into the inner workings of my system")

(defun s/dotfiles-sitemap-function (title list)
  "Custom sitemap function for the dotfiles publishing project."
  (concat "#+OPTIONS: html-postamble:nil\n"
	  (org-publish-sitemap-default title list)))

(defun s/posts-sitemap-function (title list)
  "Custom sitemap function for the posts publishing project."
  (concat "#+OPTIONS: html-postamble:nil html-preamble:nil\n"
	  (org-publish-sitemap-default title list)))

(defun s/get-template (path)
  "Read a template from the templates directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "src/templates" path))
    (buffer-string)))

(defun s/stylesheet (filename)
  "Format filename as a stylesheet."
  (shr-dom-to-xml `(link ((rel . "stylesheet")
			  (href . ,filename)))))

(defvar s/html-head
  (concat
   (s/stylesheet "/css/def.css")
   (s/stylesheet "/css/common.css")
   (s/stylesheet "/css/heading.css")
   (s/stylesheet "/css/nav.css")
   (s/stylesheet "/css/org.css")
   (s/stylesheet "/css/source.css")
   (s/stylesheet "/css/table.css")
   (s/stylesheet "/css/figure.css")
   (shr-dom-to-xml '(link ((rel . "icon")
			   (type . "image/x-icon")
			   (href . "/assets/favicon.ico")))))
  "HTML headers shared across publishing projects.")

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  `((?a . ,(org-export-data (plist-get info :author) info))
    (?t . ,(org-export-data (plist-get info :title) info))
    (?m . ,(plist-get info :email))
    (?e . ,(liaison-get-resource-url 'edit))
    (?l . ,(liaison-get-resource-url 'log))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (dot . t)
   (elisp . nil)))

(setq org-export-global-macros
      '(("asset" . "(s/asset-global-macro $1)")))

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil)

(setq org-publish-list-skipped-files nil
      org-publish-timestamp-directory ".cache/"
      org-export-time-stamp-file nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-confirm-babel-evaluate #'s/should-lang-confirm?
      org-plantuml-jar-path (s/get-plantuml-jar-path)
      org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type nil
      org-html-head-include-default-style nil
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-latex-pdf-process '("latexmk -f -pdf %f")
      org-id-files ".org-id-locations")

(setq org-cite-global-bibliography (list (expand-file-name "assets/refs.bib"))
      org-cite-csl-styles-dir (expand-file-name "assets/csl/styles")
      org-cite-csl-locales-dir (expand-file-name "assets/csl/locales")
      org-cite-export-processors
      '((html . (csl "apa.csl"))
	(latex . biblatex)
	(t . simple)))

(setq org-publish-project-alist
      (let* ((posts-postamble (concat (s/get-template "postamble/posts.html")
				      (s/get-template "footer.html")))
	     (posts-preamble (s/get-template "preamble/main.html"))
	     (content-preamble (s/get-template "preamble/main.html"))
	     (dotfiles-preamble (s/get-template "preamble/dotfiles.html"))
	     (dotfiles-postamble (s/get-template "footer.html")))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :section-numbers nil
	       :with-toc nil
	       :with-title t
	       :html-head-extra s/html-head
	       :html-preamble content-preamble
	       :html-postamble nil)
	 (list "posts"
	       :base-extension "org"
	       :base-directory "src/posts"
	       :publishing-directory "public/posts"
	       :publishing-function 'org-html-publish-to-html
	       :auto-sitemap t
	       :sitemap-sort-files 'anti-chronologically
	       :sitemap-format-entry 's/posts-sitemap-format-entry
	       :sitemap-function 's/dotfiles-sitemap-function
	       :with-title t
	       :with-toc nil
	       :html-preamble posts-preamble
	       :html-postamble posts-postamble
	       :html-head-extra (concat s/html-head
					(s/stylesheet "/css/blog.css")
					(s/stylesheet "/css/bib.css")))
	 (list "dotfiles"
	       :base-extension "org"
	       :base-directory "src/dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org"))
	       :recursive t
	       :auto-sitemap t
	       :sitemap-title s/dotfiles-sitemap-title
	       :sitemap-style 'list
	       :sitemap-format-entry 's/dotfiles-sitemap-format-entry
	       :sitemap-function 's/dotfiles-sitemap-function
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-preamble dotfiles-preamble
	       :html-postamble dotfiles-postamble
	       :html-head-extra s/html-head)
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
	       :publishing-function 'org-publish-attachment
	       :sitemap-title "Posts")
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
	 (list "all"
	       :components (list "content"
				 "posts"
				 "dotfiles"
			       	 "stylesheets"
				 "javascripts"
				 "images"
				 "data"
				 "cv")))))
