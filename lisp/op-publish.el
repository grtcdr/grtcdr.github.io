;;; op-publish.el  -*- lexical-binding:t -*-

;; Copyright (C) 2022 Aziz Ben Ali

;; This file is not part of GNU Emacs.

;; Permission to use, copy, modify, and/or distribute this software for
;; any purpose with or without fee is hereby granted, provided that the
;; above copyright notice and this permission notice appear in all
;; copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;; WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
;; AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
;; DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
;; PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
;; TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
;; PERFORMANCE OF THIS SOFTWARE.

;;; Commentary:

;; op-publish.el defines the various components that consitute
;; https://grtcdr.tn and should be used in conjunction with a build
;; system.

;;; Code:

(add-to-list 'load-path (file-name-concat default-directory "lisp"))

(require 'op-package)
(require 'op-redefun)
(require 'op-template)
(require 'oc)
(require 'oc-csl)
(require 'ox-publish)
(require 'citeproc)

;; Prevent excessively large backtraces
(setq debug-on-error t)

(defun op-publish-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (or (and id (format "<a href=\"#%s\">%s</a>" id text) text))))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(defun op-publish-posts-sitemap-formatter (entry style project)
  "Format a sitemap entry with its date within the context of the
posts publishing project."
  (format "%s - [[file:%s][%s]]"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)))

(defun op-publish-dotfiles-sitemap-formatter (entry style project)
  "Format a sitemap entry with its date within the context of the
dotfiles publishing project."
  (let* ((title (org-publish-find-title entry project))
	 (description (org-publish-find-property entry :description project 'html))
	 (link (format "[[file:%s][%s]]" entry title)))
    (if description
	(concat link " -- " description "\n")
      entry)))

(defun op-publish-dotfiles-sitemap-function (title list)
  "Custom sitemap function for the dotfiles publishing project."
  (concat "#+OPTIONS: html-postamble:nil" "\n"
	  (org-publish-sitemap-default title list)))

(defun op-publish-posts-sitemap-function (title list)
  "Custom sitemap function for the posts publishing project."
  (concat "#+OPTIONS: html-postamble:nil html-preamble:nil" "\n"
	  (org-publish-sitemap-default title list)))

(defun op-publish-should-lang-confirm? (lang body)
  "Return non-nil if LANG is to be evaluated without confirmation."
  (not (member lang '("dot" "emacs-lisp" "plantuml"))))

(setq user-full-name "Aziz Ben Ali"
      user-mail-address "tahaaziz.benali@esprit.tn"
      make-backup-files nil
      org-publish-list-skipped-files nil
      org-publish-timestamp-directory ".cache/"
      org-html-doctype "html5"
      org-html-footnotes-section (op-template-footnotes-section)
      org-export-time-stamp-file nil
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-plantuml-exec-mode 'plantuml
      org-plantuml-args '("-headless")
      org-confirm-babel-evaluate #'op-publish-should-lang-confirm?
      org-html-htmlize-output-type 'css
      org-html-head-include-default-style nil
      org-html-html5-fancy t
      org-html-prefer-user-labels t
      org-cite-global-bibliography (list (expand-file-name "assets/refs.bib"))
      org-cite-csl-styles-dir (expand-file-name "assets/csl/styles")
      org-cite-csl-locales-dir (expand-file-name "assets/csl/locales")
      org-cite-export-processors
      '((html . (csl "ieee.csl"))
	(latex . biblatex)
	(t . simple)))

(let ((main-navbar (op-template-main-navbar)))
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
	       :html-head-extra (op-template-metadata)
	       :html-preamble (op-template-main-navbar)
	       :html-postamble nil)
	 (list "posts"
	       :base-extension "org"
	       :base-directory "src/posts"
	       :publishing-directory "public/posts"
	       :publishing-function 'org-html-publish-to-html
	       :auto-sitemap t
	       :sitemap-sort-files 'anti-chronologically
	       :sitemap-format-entry 'op-publish-posts-sitemap-formatter
	       :sitemap-function 'op-publish-dotfiles-sitemap-function
	       :sitemap-title "Posts"
	       :with-title t
	       :with-toc nil
	       :html-preamble (op-template-main-navbar)
	       :html-postamble
	       (concat (op-template-posts-footer)
		       (op-template-main-footer))
	       :html-head-extra
	       (concat (op-template-metadata)
		       (op-template-stylesheet "/css/blog.css")))
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
	       :sitemap-format-entry 'op-publish-dotfiles-sitemap-formatter
	       :sitemap-function 'op-publish-dotfiles-sitemap-function
	       :section-numbers t
	       :with-title t
	       :with-toc t
	       :html-preamble (op-template-dotfiles-navbar)
	       :html-postamble (op-template-dotfiles-footer)
	       :html-head-extra (op-template-metadata))
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
	 (list "cv"
	       :base-extension "pdf"
	       :base-directory "src/cv"
	       :publishing-directory "public/assets"
	       :publishing-function 'org-publish-attachment)
	 (list "all" :components '("content" "posts" "dotfiles" "stylesheets"
				   "javascripts" "images" "data" "cv")))))
