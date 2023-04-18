;;; +redefun.el  -*- lexical-binding:t -*-

(require 'liaison)

(defalias 'sxml #'shr-dom-to-xml)

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  `((?a . ,(org-export-data (plist-get info :author) info))
    (?t . ,(org-export-data (plist-get info :title) info))
    (?m . ,(plist-get info :email))
    (?l . ,(liaison-get-resource-url 'log))
    (?b . ,(liaison-get-resource-url 'blob))))

(defun +html-listing (block info)
  (let ((number (org-export-get-ordinal block info nil #'org-html--has-caption-p)))
    (sxml
     `(span ((class . "listing-number"))
	    ,(format (org-html--translate "Listing %d: " info)
		     number)))))

(defun +html-caption-block (listing caption info)
  (let ((caption (org-trim (org-export-data caption info))))
    (sxml
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
			(listing (+html-listing example-block info)))
		  (+html-caption-block listing caption info) "")
	      (format "<pre class=\"example\"%s>%s</pre>"
		      (let* ((reference (org-html--reference example-block info))
			     (a (org-html--make-attribute-string
				 (if (or (not reference) (plist-member attributes :id))
				     attributes
				   (plist-put attributes :id reference)))))
			(if (org-string-nw-p a) (concat " " a) ""))
		      (org-html-format-code example-block info))))))
