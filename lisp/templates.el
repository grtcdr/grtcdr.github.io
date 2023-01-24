;;; templates.el

;; Copyright (C) 2023  Aziz Ben Ali

;; Author: Aziz Ben Ali <tahaaziz.benali@esprit.tn>
;; Homepage: https://github.com/grtcdr/darkman.el

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

;; templates.el provides the XML templates of http://grtcdr.tn/darkman.el.

;;; Code:

(require 'shr)

(defalias 'sexp->xml #'shr-dom-to-xml)

(defun templates/footnotes ()
  "HTML snippet representing the footnotes section."
  (sexp->xml
   '(div ((id . "footnotes"))
	 (h2 ((class . "footnotes"))
	     "%s")
	 (div ((id . "text-footnotes"))
	      "%s"))))

(defun templates/footer ()
  "HTML snippet representing the footer section."
  (sexp->xml
   '(footer nil
	    (p nil "&alefsym;")
	    (p nil
	       (a ((href . "%l"))
		  "What's changed?")))))

(defun templates/now-footer ()
  "HTML snippet representing the footer section."
  (sexp->xml
   '(footer nil
	    (p nil
	       "This is a "
	       (a ((href . "https://nownownow.com/about"))
		  "now")
	       " page."))))

(defun templates/posts-postamble ()
  "HTML snippet representing the postamble of a post."
  (sexp->xml
   '(div ((class . "blog-footer"))
	 (p nil
	    (b nil
	       "Got something to share?"))
	 (p nil
	    (a ((href . "mailto:%m?subject=[REPLY] %t"))
	       "Tell me about it")
	    ". If you found a problem with this page, don't hesitate to"
	    (a ((href . "%e"))
	       "propose an edit")
	    "."))))

(defun templates/dotfiles-preamble ()
  "HTML snippet representing the preamble of the dotfiles publishing project."
  (sexp->xml
   '(nav nil
	 (ul nil
	     (li nil
		 (a ((href . "/index.html"))
		    "Home"))
	     (li nil
		 (a ((href . "/dotfiles/sitemap.html"))
		    "Top"))))))

(defun templates/main-preamble ()
  "HTML snippet representing the preamble used across the different publishing projects."
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
		 (a ((href . "/now.html"))
		    "Now"))
	     (li nil
		 (a ((href . "/contact.html"))
		    "Contact"))))))

(provide 'site/templates)
