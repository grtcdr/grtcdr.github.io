;; publish.el defines the various components that consitute this
;; website and should be used in conjunction with a build system.

(normal-top-level-add-subdirs-to-load-path)

(defun site/install-packages (packages)
  "Install the necessary packages. PACKAGES is a list of packages."
  (with-eval-after-load 'package
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
    (package-initialize)
    (unless package-archive-contents
      (package-refresh-contents)))
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(site/install-packages '(ini-mode toml-mode citeproc htmlize))

(require 'project)

;; Publishing
(require 'ox-publish)
(require 'org-id)

;; Citations
(require 'oc)
(require 'oc-csl)
(require 'citeproc)

;; Templating
(require 'templates)

;; URLs
(require 'liaison)

(defun site/post-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
posts publishing project."
  (format "%s - [[file:%s][%s]]"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)))

(defun site/dotfile-sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date within the context of the
dotfiles publishing project."
  (let* ((title (org-publish-find-title entry project))
	(description (org-publish-find-property entry :description project 'html))
	(link (format "[[file:%s][%s]]" entry title)))
    (if description
	(concat link " -- " description "\n")
      entry)))

(defun site/dotfile-sitemap-function (title list)
  "Custom sitemap function for the dotfiles publishing project."
  (concat "#+OPTIONS: html-postamble:nil" "\n"
	  (org-publish-sitemap-default title list)))

(defun site/post-sitemap-function (title list)
  "Custom sitemap function for the posts publishing project."
  (concat "#+OPTIONS: html-postamble:nil html-preamble:nil" "\n"
	  (org-publish-sitemap-default title list)))

(defun site/should-lang-confirm? (lang body)
  "Return non-nil if LANG is to be evaluated without confirmation."
  (not (member lang '("dot" "elisp" "plantuml"))))

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  `((?a . ,(org-export-data (plist-get info :author) info))
    (?t . ,(org-export-data (plist-get info :title) info))
    (?m . ,(org-export-data (plist-get info :email) info))
    (?e . ,(liaison-get-resource-url 'edit))
    (?l . ,(liaison-get-resource-url 'log))))

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil
      org-publish-list-skipped-files nil
      org-publish-timestamp-directory ".cache/"
      org-html-doctype "html5"
      org-html-footnotes-section (templates/footnote-section)
      org-export-time-stamp-file nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-confirm-babel-evaluate #'site/should-lang-confirm?
      org-plantuml-exec-mode 'plantuml
      org-html-htmlize-output-type 'css
      org-html-head-include-default-style nil
      org-html-html5-fancy t
      org-cite-global-bibliography (list (expand-file-name "assets/refs.bib"))
      org-cite-csl-styles-dir (expand-file-name "assets/csl/styles")
      org-cite-csl-locales-dir (expand-file-name "assets/csl/locales")
      org-cite-export-processors
      '((html . (csl "apa.csl"))
	(latex . biblatex)
	(t . simple)))

(let ((main-navbar (templates/main-navbar)))
  (setq org-publish-project-alist
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "src"
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :section-numbers nil
	       :with-toc nil
	       :with-title t
	       :html-head-extra (templates/metadata)
	       :html-preamble main-navbar
	       :html-postamble nil)
	 (list "posts"
	       :base-extension "org"
	       :base-directory "src/posts"
	       :publishing-directory "public/posts"
	       :publishing-function 'org-html-publish-to-html
	       :auto-sitemap t
	       :sitemap-sort-files 'anti-chronologically
	       :sitemap-format-entry 'site/post-sitemap-format-entry
	       :sitemap-function 'site/dotfile-sitemap-function
	       :with-title t
	       :with-toc nil
	       :html-preamble main-navbar
	       :html-postamble
	       (concat (templates/post-footer)
		       (templates/main-footer))
	       :html-head-extra
	       (concat (templates/metadata)
		       (templates/stylesheet "/css/blog.css")
		       (templates/stylesheet "/css/highlight.css")))
	 (list "dotfiles"
	       :base-extension "org"
	       :base-directory "src/dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org"))
	       :recursive t
	       :auto-sitemap t
	       :sitemap-title "Peek into the inner workings of my system"
	       :sitemap-style 'list
	       :sitemap-format-entry 'site/dotfile-sitemap-format-entry
	       :sitemap-function 'site/dotfile-sitemap-function
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-preamble (templates/dotfile-navbar)
	       :html-postamble (templates/main-footer)
	       :html-head-extra (concat (templates/metadata)
					(templates/stylesheet "/css/highlight.css")))
	 (list "images"
	       :base-extension (regexp-opt '("png" "jpg" "jpeg" "svg"))
	       :base-directory "assets/images"
	       :publishing-directory "public/assets/images"
	       :publishing-function 'org-publish-attachment
	       :recursive t)
	 (list "data"
	       :base-extension ".*"
	       :base-directory "assets"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment)
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
	 (list "all" :components '("content" "posts" "dotfiles" "stylesheets" "javascripts" "images" "data" "cv")))))
