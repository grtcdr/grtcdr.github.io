;;; publish.el --- prepares websites for publishing -*- lexical-binding: t; -*-

;; Author: Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; URL: https://github.com/grtcdr/grtcdr.github.io

;; publish.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; publish.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with publish.el.  If not, see <https://www.gnu.org/licenses/>.

;; This file contains code taken from GNU Emacs, which is
;; Copyright (C) 1976-2022 Free Software Foundation, Inc.

;;; Commentary:

;; publish.el specifies the structure of a website built atop
;; org-publish-project-alist.  It should also specify any other
;; options that affect the building of the website.

(require 'ox-publish)

;;; Configurations:

;; Org caching:

(setq org-export-time-stamp-file nil
      org-publish-timestamp-directory ".timestamps/")

;; Org content metadata:

(setq org-html-metadata-timestamp-format "%B %d, %Y")

;; Org source blocks:

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-html-htmlize-output-type nil)

;; Emacs:

(setq make-backup-files nil)

;; Metadata:

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "ba.tahaaziz@gmail.com")

;;; Functions:

(defun sitemap-format-entry (entry style project)
  "Format a sitemap entry with its date."
  (format "%s - [[file:%s][%s]]"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)))

(defun org-html-format-headline-function (todo todo-type priority text tags info)
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

(defun read-snippet (directory file)
  "Read a snippet from the snippets directory."
  (with-temp-buffer
    (insert-file-contents
     (file-name-concat "snippets" directory file))
    (buffer-string)))

;;; Project specification:

(setq org-publish-project-alist
      (let ((posts-postamble (read-snippet "postamble" "posts.html"))
	    (posts-preamble (read-snippet "preamble" "posts.html"))
	    (content-preamble (read-snippet "preamble" "content.html"))
	    (dotfiles-preamble (read-snippet "preamble" "dotfiles.html")))
	(list
	 (list "content"
	       :base-extension "org"
	       :base-directory "."
	       :publishing-directory "public"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org" "_setup.org"))
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
	       :sitemap-format-entry 'sitemap-format-entry
	       :with-title t
	       :with-toc t
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-format-headline-function 'org-html-format-headline-function
	       :html-preamble posts-preamble
	       :html-postamble posts-postamble
	       :html-head-include-default-style nil)
	 (list "dotfiles"
	       :recursive t
	       :base-extension "org"
	       :base-directory "dotfiles"
	       :publishing-directory "public/dotfiles"
	       :publishing-function 'org-html-publish-to-html
	       :exclude (regexp-opt '("README.org" "COPYING.org" "_setup.org"))
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-html5-fancy t
	       :html-doctype "html5"
	       :html-format-headline-function 'org-html-format-headline-function
	       :html-preamble dotfiles-preamble
	       :html-postamble nil
	       :html-head-include-default-style nil)
	 (list "data"
	       :base-extension (regexp-opt '("txt" "pdf"))
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
	       :publishing-directory "public/javascripts"
	       :publishing-function 'org-publish-attachment)
	 (list "images"
	       :recursive t
	       :base-extension (regexp-opt '("png" "jpg" "jpeg" "ico" "svg"))
	       :base-directory "images"
	       :publishing-directory "public/images" 
	       :publishing-function 'org-publish-attachment)
	 (list "all"
	       :components (list "content"
				 "posts"
				 "dotfiles"
				 "stylesheets"
				 "javascripts"
				 "data"
				 "images")))))
