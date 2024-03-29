;;; +publish.el  -*- lexical-binding:t -*-

;; Prevent excessively large backtraces
(setq debug-on-error nil
      no-byte-compile t)

(load-file "lisp/+package.el")
(load-file "lisp/+redefun.el")
(load-file "lisp/+template.el")

(require 'oc)
(require 'oc-csl)
(require 'ox-publish)
(require 'citeproc)

(defun +publish-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (or (and id (format "<a href=\"#%s\">%s</a>" id text) text))))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(defun +publish-posts-sitemap-formatter (entry style project)
  "Format a sitemap entry with its date within the context of the
posts publishing project."
  (format "%s - [[file:%s][%s]]"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)))

(defun +publish-dotfiles-sitemap-formatter (entry style project)
  "Format a sitemap entry with its date within the context of the
dotfiles publishing project."
  (let* ((title (org-publish-find-title entry project))
	 (description (org-publish-find-property entry :description project 'html))
	 (link (format "[[file:%s][%s]]" entry title)))
    (if description
	(concat link " -- " description "\n")
      entry)))

(defun +publish-dotfiles-sitemap-function (title list)
  "Custom sitemap function for the dotfiles publishing project."
  (concat "#+OPTIONS: html-postamble:nil" "\n"
	  (org-publish-sitemap-default title list)))

(defun +publish-posts-sitemap-function (title list)
  "Custom sitemap function for the posts publishing project."
  (concat "#+OPTIONS: html-postamble:nil html-preamble:nil" "\n"
	  (org-publish-sitemap-default title list)))

(defun +publish-should-lang-confirm? (lang body)
  "Return non-nil if LANG is to be evaluated without confirmation."
  (not (member lang '("dot" "emacs-lisp" "plantuml"))))

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil
      ;; org-fold-core-style 'overlays
      org-publish-list-skipped-files nil
      org-publish-timestamp-directory ".cache/"
      org-export-time-stamp-file nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-confirm-babel-evaluate #'+publish-should-lang-confirm?
      org-plantuml-exec-mode 'plantuml
      org-plantuml-args '("-headless")
      org-html-doctype "html5"
      org-html-html5-fancy t
      org-html-htmlize-output-type 'css
      org-html-footnotes-section +template-footnotes-section
      org-html-head-include-default-style nil
      org-html-prefer-user-labels t
      org-cite-global-bibliography (list (expand-file-name "assets/bibliography.bib"))
      org-cite-csl-styles-dir "assets/csl/styles"
      org-cite-csl-locales-dir "assets/csl/locales"
      org-cite-export-processors '((html . (csl "ieee.csl"))
				   (latex . biblatex)
				   (t . simple)))

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
	     :html-head-extra +template-metadata
	     :html-preamble +template-main-navbar
	     :html-postamble nil)
       (list "posts"
	     :base-extension "org"
	     :base-directory "src/posts"
	     :publishing-directory "public/posts"
	     :publishing-function 'org-html-publish-to-html
	     :auto-sitemap t
	     :sitemap-sort-files 'anti-chronologically
	     :sitemap-format-entry '+publish-posts-sitemap-formatter
	     :sitemap-function '+publish-dotfiles-sitemap-function
	     :sitemap-title "Posts"
	     :with-title t
	     :with-toc nil
	     :html-preamble +template-main-navbar
	     :html-postamble
	     (concat +template-posts-footer
		     +template-main-footer)
	     :html-head-extra
	     (concat +template-metadata
		     (+template-stylesheet "/css/blog.css")))
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
	     :sitemap-format-entry '+publish-dotfiles-sitemap-formatter
	     :sitemap-function '+publish-dotfiles-sitemap-function
	     :section-numbers t
	     :with-title t
	     :with-toc t
	     :html-preamble +template-dotfiles-navbar
	     :html-postamble +template-main-footer
	     :html-head-extra +template-metadata)
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
	     :publishing-function 'org-publish-attachment)
       (list "javascripts"
	     :base-extension "js"
	     :base-directory "src/js"
	     :exclude "grunt.js"
	     :publishing-directory "public/js"
	     :publishing-function 'org-publish-attachment)
       (list "all"
	     :components
	     '("content" "posts" "dotfiles" "stylesheets"
	       "javascripts" "images" "data"))))
