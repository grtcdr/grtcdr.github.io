;;; publish.el ---  Org project specification. -*- lexical-binding: t; -*-

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; URL: https://github.com/grtcdr/grtcdr.tn

;; Publish is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Publish is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with publish.  If not, see <https://www.gnu.org/licenses/>.

;; This file contains code taken from GNU Emacs, which is
;; Copyright (C) 1976-2022 Free Software Foundation, Inc.

;;; Commentary:

;; Publish defines the various components that consitute this website
;; and should be used in conjunction with a build system.

;;; Code:

(require 'ox-publish)
(require 'project)

(let ((default-directory (file-name-concat (project-root (project-current)) "lisp")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'forgecast)
(require 'htmlize)

;;; Settings:
(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil
      org-export-time-stamp-file nil
      org-publish-timestamp-directory ".cache/"
      org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type 'inline-css
      org-src-fontify-natively t
      org-src-preserve-indentation t)

;;; General functions:
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
       (file-name-concat "templates" slug))
      (buffer-string)))

;;; Redefinition of built-in org-html-format-spec
(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?d . ,(org-export-data (org-export-get-date info timestamp-format) info))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?b . ,(forgecast-get-resource-url :github "grtcdr/grtcdr.github.io" 'blob))
      (?l . ,(forgecast-get-resource-url :github "grtcdr/grtcdr.github.io" 'log))
      (?B . ,(forgecast-get-resource-url :sourcehut "grtcdr/dotfiles" 'blob))
      (?L . ,(forgecast-get-resource-url :sourcehut "grtcdr/dotfiles" 'log)))))

;;; Project specification:
(setq org-publish-project-alist
      (let ((posts-postamble (publish-read-template "postamble/posts.html"))
	    (posts-preamble (publish-read-template "preamble/posts.html"))
	    (content-preamble (publish-read-template "preamble/content.html"))
	    (dotfiles-preamble (publish-read-template "preamble/dotfiles.html")))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "."
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org" "setup.org"))
	       :section-numbers nil
	       :with-toc nil
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-include-default-style nil)
	 (list "projects"
	       :base-extension "org"
	       :base-directory "projects"
	       :publishing-directory "public/p"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("setup.org"))
	       :section-numbers nil
	       :with-toc nil
	       :with-title t
	       :html-doctype "html5"
	       :html-html5-fancy t
	       :html-preamble content-preamble
	       :html-postamble nil
	       :html-head-include-default-style nil)
	 (list "posts"
	       :base-extension "org"
	       :base-directory "posts"
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
	       :html-head-include-default-style nil)
	 (list "dotfiles"
	       :recursive t
	       :base-extension "org"
	       :base-directory "dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org" "COPYING.org" "setup.org"))
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-format-headline-function 'publish-format-headline-function
	       :html-preamble dotfiles-preamble
	       :html-postamble nil
	       :html-head-include-default-style nil)
	 (list "data"
	       :base-extension (regexp-opt '("txt" "pdf" "asc"))
	       :base-directory "data"
	       :publishing-directory "public/data"
	       :publishing-function 'org-publish-attachment)
	 (list "stylesheets"
	       :base-extension "css"
	       :base-directory "stylesheets" 
	       :publishing-directory "public/stylesheets"
	       :publishing-function 'org-publish-attachment)
	 (list "javascripts"
	       :base-extension "js"
	       :base-directory "javascripts"
	       :exclude "grunt.js"
	       :publishing-directory "public/javascripts"
	       :publishing-function 'org-publish-attachment)
	 (list "images"
	       :recursive t
	       :base-extension (regexp-opt '("png" "jpg" "jpeg" "svg"))
	       :base-directory "images"
	       :publishing-directory "public/images" 
	       :publishing-function 'org-publish-attachment)
	 (list "favicon"
	       :base-extension "ico"
	       :base-directory "images"
	       :publishing-directory "public"
	       :publishing-function 'org-publish-attachment)
	 (list "all"
	       :components (list "content"
				 "projects"
				 "posts"
				 "dotfiles"
				 "stylesheets"
				 "javascripts"
				 "data"
				 "images"
				 "favicon")))))
