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
(require 'project)
(require 'vc)

(defun load-package (pkg)
  "Add a local PKG to ’load-path’ and load it."
  (normal-top-level-add-to-load-path (list (symbol-name pkg)))
  (require pkg))

(let ((default-directory (file-name-concat default-directory "lisp")))
  (load-package 'htmlize))

;;; Settings:

;; vc:

(setq vc-follow-symlinks t)

;; org:

(setq org-export-time-stamp-file nil
      org-publish-timestamp-directory ".cache/"
      org-html-metadata-timestamp-format "%B %d, %Y"
      org-html-htmlize-output-type 'inline-css
      org-src-fontify-natively t
      org-src-preserve-indentation t)

;; emacs:

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil)

;;; General functions:

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

;;; Resource source/history functions:

(defvar forges
  '(:github "github.com" :sourcehut "git.sr.ht")
  "A map of various git forges and their respective domain.")

(defun build-forge-url (forge slug type)
  "Construct the standard URL of a given FORGE by specifying
the REPO and the TYPE of information to access.

FORGE is a property from the ’forges’ variable.

SLUG is a string and the combination of your username and the
name of your repository, e.g. \"octopus/website\".

TYPE can take a value of ’log’ or ’tree’."
  (cond ((equal forge :github)
	 (format "https://%s/%s/%s/"
		 (plist-get forges :github)
		 slug
		 (cond ((eq type 'log) "commits/main")
		       ((eq type 'tree) "blob/main")
		       (t (error "Invalid type.")))))
	((equal forge :sourcehut)
	 (format "https://%s/%s/%s/"
		 (plist-get forges :sourcehut)
		 (concat "~" slug)
		 (cond ((eq type 'log) "log/main/item")
		       ((eq type 'tree) "tree/main/item")
		       (t (error "Invalid type.")))))))

(defun get-resource-slug ()
  "Determines the path of a resource relative to the value returned by ’forge-construct-resource-url'"
  (let* ((buffer (buffer-file-name))
	 (root (vc-find-root buffer ".git")))
    (string-remove-prefix
     (expand-file-name root) buffer)))

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  (let ((timestamp-format (plist-get info :html-metadata-timestamp-format)))
    `((?t . ,(org-export-data (plist-get info :title) info))
      (?s . ,(org-export-data (plist-get info :subtitle) info))
      (?d . ,(org-export-data (org-export-get-date info timestamp-format)
			      info))
      (?T . ,(format-time-string timestamp-format))
      (?a . ,(org-export-data (plist-get info :author) info))
      (?e . ,(mapconcat
	      (lambda (e) (format "<a href=\"mailto:%s\">%s</a>" e e))
	      (split-string (plist-get info :email)  ",+ *")
	      ", "))
      (?c . ,(plist-get info :creator))
      (?C . ,(let ((file (plist-get info :input-file)))
	       (format-time-string timestamp-format
				   (and file (file-attribute-modification-time
					      (file-attributes file))))))
      (?v . ,(or (plist-get info :html-validation-link) ""))
      (?w . ,(format
	      "<a href=%s>source</a>"
	      (concat
	       (build-forge-url :github "grtcdr/grtcdr.tn" 'tree)
	       (get-resource-slug))))
      (?x . ,(format
	      "<a href=%s>history</a>"
	      (concat
	       (build-forge-url :github "grtcdr/grtcdr.tn" 'log)
	       (get-resource-slug))))
      (?y . ,(format
	      "<a href=%s>source</a>"
	      (concat
	       (build-forge-url :sourcehut "grtcdr/dotfiles" 'tree)
	       (get-resource-slug))))
      (?z . ,(format
	      "<a href=%s>history</a>"
	      (concat
	       (build-forge-url :sourcehut "grtcdr/dotfiles" 'log)
	       (get-resource-slug)))))))

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
	       :exclude (regexp-opt '("README.org" "COPYING.org" "setup.org"))
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
				 "projects"
				 "posts"
				 "dotfiles"
				 "stylesheets"
				 "javascripts"
				 "data"
				 "images")))))
