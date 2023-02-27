;;; op-publish.el  -*- lexical-binding:t -*-

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

;; op-redefun.el redefines core functions from ‘ox-publish’ and
;; related libraries.

;;; Code:

(require 'liaison)

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  `((?a . ,(org-export-data (plist-get info :author) info))
    (?t . ,(org-export-data (plist-get info :title) info))
    (?m . ,(plist-get info :email))
    (?e . ,(liaison-get-resource-url 'edit))
    (?l . ,(liaison-get-resource-url 'log))
    (?b . ,(liaison-get-resource-url 'blob))))

(defun op-publish-listing (block info)
  (let ((number (org-export-get-ordinal block info nil #'org-html--has-caption-p)))
    (sexp->xml
     `(span ((class . "listing-number"))
	    ,(format (org-html--translate "Listing %d: " info)
		     number)))))

(defun op-publish-caption-block (listing caption info)
  (let ((caption (org-trim (org-export-data caption info))))
    (sexp->xml
     `(label ((class . "org-example-name"))
	     ,(concat listing caption)))))

(defun org-html-example-block (example-block _contents info)
  "Transcode an EXAMPLE-BLOCK element from Org to HTML.
CONTENTS is nil. INFO is a plist holding contextual
information."
  (let ((attributes (org-export-read-attribute :attr_html example-block)))
    (if (plist-get attributes :textarea)
	(org-html--textarea-block example-block)
      (format "<div class=\"org-example-container\">%s%s</div>"
	      (if-let* ((caption (org-export-get-caption example-block))
			(listing (op-publish-listing example-block info)))
		  (op-publish-caption-block listing caption info) "")
	      (format "<pre class=\"example\"%s>%s</pre>"
		      (let* ((reference (org-html--reference example-block info))
			     (a (org-html--make-attribute-string
				 (if (or (not reference) (plist-member attributes :id))
				     attributes
				   (plist-put attributes :id reference)))))
			(if (org-string-nw-p a) (concat " " a) ""))
		      (org-html-format-code example-block info))))))

(provide 'op-redefun)
;;; op-redefun.el ends here
