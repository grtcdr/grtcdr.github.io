;;; +template.el  -*- lexical-binding:t -*-

(require 'shr)

(defalias 'sexp->xml #'shr-dom-to-xml)

(defun +template-stylesheet (filename)
  "Format FILENAME as a stylesheet."
  (sexp->xml
   `(link ((rel . "stylesheet")
	   (href . ,filename)))))

(defvar +template-footnotes-section
  (sexp->xml
   '(div ((id . "footnotes"))
	 (h2 ((class . "footnotes"))
	     "%s")
	 (div ((id . "text-footnotes"))
	      "%s")))
  "HTML snippet representing the footnotes section.")

(defvar +template-main-footer
  (sexp->xml
   '(footer nil
	    (p nil "&alefsym;")
	    (p nil
	       (a ((href . "%l"))
		  "What's changed?"))))
  "HTML snippet representing the footer section.")

(defvar +template-posts-footer
  (sexp->xml
   '(div ((class . "blog-footer"))
	 (p nil
	    "Got something to share?")
	 (p nil
	    (a ((href . "mailto:%m"))
	       "I'd love to hear it!"))))
  "HTML snippet representing the postamble of a post.")

(defvar +template-dotfiles-navbar
  (sexp->xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/index.html"))
		    "Home"))
	     (li nil
		 (a ((href . "/dotfiles/sitemap.html"))
		    "Top")))))
  "HTML snippet representing the preamble of the dotfiles publishing
project.")

(defvar +template-main-navbar
  (sexp->xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/index.html"))
		    "Home"))
	     (li nil
		 (a ((href . "/dotfiles/sitemap.html"))
		    "Dotfiles"))
	     (li nil
		 (a ((href . "/contact.html"))
		    "Contact")))))
  "HTML snippet representing the preamble used across the different publishing projects.")

(defvar +template-metadata
  (concat
   (+template-stylesheet "/css/common.css")
   (sexp->xml '(link ((rel . "icon")
		      (type . "image/x-icon")
		      (href . "/assets/favicon.ico")))))
  "HTML headers shared across publishing projects.")
