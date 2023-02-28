;;; op-template.el  -*- lexical-binding:t -*-

;; Copyright (C) 2023 Aziz Ben Ali

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

;; op-template.el provides the HTML templates of https://grtcdr.tn.

;;; Code:

(require 'shr)

(defalias 'sexp->xml #'shr-dom-to-xml)

(defun op-template-stylesheet (filename)
  "Format FILENAME as a stylesheet."
  (sexp->xml
   `(link ((rel . "stylesheet")
	   (href . ,filename)))))

(defvar op-template-footnotes-section
  (sexp->xml
   '(div ((id . "footnotes"))
	 (h2 ((class . "footnotes"))
	     "%s")
	 (div ((id . "text-footnotes"))
	      "%s")))
  "HTML snippet representing the footnotes section.")

(defvar op-template-main-footer
  (sexp->xml
   '(footer nil
	    (p nil "&alefsym;")
	    (p nil
	       (a ((href . "%l"))
		  "What's changed?"))))
  "HTML snippet representing the footer section.")

(defvar op-template-dotfiles-footer
  (sexp->xml
   '(footer nil
	    (p nil "&alefsym;")
	    (p nil
	       (a ((href . "%l"))
		  "What's changed?")
	       "See the"
	       (a ((href . "%b"))
		  "raw")
	       "file.")))
  "HTML snippet representing the footer section.")

(defvar op-template-posts-footer
  (sexp->xml
   '(div ((class . "blog-footer"))
	 (p nil
	    (b nil
	       "Got something to share?"))
	 (p nil
	    (a ((href . "mailto:%m"))
	       "Tell me about it")
	    ". If you found a problem in this page, consider"
	    (a ((href . "%e"))
	       "proposing an edit")
	    ".")))
  "HTML snippet representing the postamble of a post.")

(defvar op-template-dotfiles-navbar
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

(defvar op-template-main-navbar
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
		    "Contact"))
	     (li nil
		 (a ((href . "/now.html"))
		    "Now")))))
  "HTML snippet representing the preamble used across the different publishing projects.")

(defvar op-template-metadata
  (concat
   (op-template-stylesheet "/css/common.css")
   (sexp->xml '(link ((rel . "icon")
		      (type . "image/x-icon")
		      (href . "/assets/favicon.ico")))))
  "HTML headers shared across publishing projects.")

(provide 'op-template)
;;; op-template.el ends here
