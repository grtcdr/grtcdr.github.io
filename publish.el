(require 'project)
(require 'ox-publish)
(require 'ox-html)

(defun publisher-site-name ()
  (file-name-nondirectory
   (directory-file-name
    (project-root (project-current)))))

(defun publisher-site-root ()
  (project-root (project-current)))

(defun publisher-site-build-directory (context &optional directory)
  "Prefixes the provided DIRECTORY with the ’grt/site-base-directory’ given the CONTEXT."
  (let ((root (publisher-site-root)))
    (cond ((equal context 'base) (file-name-concat root directory))
	  ((equal context 'publish) (file-name-concat root "public" directory)))))

(defun publisher--sitemap-format-entry (entry style project)
  (format "%s - [[file:%s][%s]]"
	  (format-time-string "%Y-%m-%d" (org-publish-find-date entry project))
	  entry
	  (org-publish-find-title entry project)))

;; Copyright: Toon Claes
;; Source: https://gitlab.com/to1ne/blog/-/tree/master/elisp
(defun publisher--org-html-format-headline-function (todo todo-type priority text tags info)
  "Format a headline with a link to itself."
  (let* ((headline (get-text-property 0 :parent text))
         (id (or (org-element-property :CUSTOM_ID headline)
                 (org-export-get-reference headline info)
                 (org-element-property :ID headline)))
         (link (if id
                   (format "<a href=\"#%s\">%s</a>" id text)
                 text)))
    (org-html-format-headline-default-function todo todo-type priority link tags info)))

(defun publisher-project-spec ()
  (let* ((site-name (publisher-site-name))
	 ;; Names of specifications
	 (content-spec (concat site-name ":content"))
	 (dotfiles-spec (concat site-name ":dotfiles"))
	 (posts-spec (concat site-name ":posts"))
	 (images-spec (concat site-name ":images"))
	 (data-spec (concat site-name ":data"))
	 (stylesheets-spec (concat site-name ":stylesheets"))
	 (javascripts-spec (concat site-name ":javascripts"))
	 ;; Paths of published files
	 (public-images (publisher-site-build-directory 'publish "images"))
	 (public-data (publisher-site-build-directory 'publish "data"))
	 (public-stylesheets (publisher-site-build-directory 'publish "stylesheets"))
	 (public-dotfiles (publisher-site-build-directory 'publish "dotfiles"))
	 (public-posts (publisher-site-build-directory 'publish "posts"))
	 (public-javascripts (publisher-site-build-directory 'publish "javascripts"))
	 (public-content (publisher-site-build-directory 'publish))
	 ;; Paths of original files
	 (base-images (publisher-site-build-directory 'base "images"))
	 (base-data (publisher-site-build-directory 'base "data"))
	 (base-stylesheets (publisher-site-build-directory 'base "stylesheets"))
	 (base-dotfiles (publisher-site-build-directory 'base "dotfiles"))
	 (base-posts (publisher-site-build-directory 'base "posts"))
	 (base-javascripts (publisher-site-build-directory 'base "javascripts"))
	 (base-content (publisher-site-build-directory 'base))
	 ;; Contents of snippets
	 (read-snippet (lambda (filename)
			 (with-temp-buffer
			   (insert-file-contents (file-name-concat base-content "snippets" filename))
			   (buffer-string))))
	 (preamble-site (funcall read-snippet "preamble-site.html"))
	 (preamble-dotfiles (funcall read-snippet "preamble-dotfiles.html")))
    (list
     (list content-spec
	   :base-extension "org"
	   :base-directory base-content
	   :publishing-directory public-content
	   :publishing-function 'org-html-publish-to-html
	   :exclude "\\(README\\|_setup\\).org"
	   :section-numbers nil
	   :with-toc nil
	   :with-title t
	   :html-doctype "html5"
	   :html-html5-fancy t
	   :html-preamble preamble-site
	   :html-head-include-default-style nil)
     (list posts-spec
	   :base-extension "org"
	   :base-directory base-posts
	   :publishing-directory public-posts
	   :publishing-function 'org-html-publish-to-html
	   :auto-sitemap t
	   :sitemap-title "Posts"
	   :sitemap-sort-files 'anti-chronologically 
	   :sitemap-format-entry 'publisher--sitemap-format-entry
	   :section-numbers nil
	   :with-title t
	   :with-toc t
	   :html-html5-fancy t
	   :html-doctype "html5"
	   :html-format-headline-function 'publisher--org-html-format-headline-function
	   :html-preamble preamble-site
	   :html-head-include-default-style nil)
     (list dotfiles-spec
	   :recursive t
	   :base-extension "org"
	   :base-directory base-dotfiles
	   :publishing-directory public-dotfiles
	   :publishing-function 'org-html-publish-to-html
	   :exclude "\\(README\\|COPYING\\|.setup\\).org"
	   :section-numbers t
	   :with-title t
	   :with-toc t
	   :html-html5-fancy t
	   :html-doctype "html5"
	   :html-format-headline-function 'publisher--org-html-format-headline-function
	   :html-preamble preamble-dotfiles
	   :html-head-include-default-style nil)
     (list data-spec
	   :base-extension "txt\\|pdf"
	   :base-directory base-data
	   :publishing-directory public-data
	   :publishing-function 'org-publish-attachment)
     (list stylesheets-spec
	   :base-extension "css"
	   :base-directory base-stylesheets
	   :publishing-directory public-stylesheets
	   :publishing-function 'org-publish-attachment)
     (list javascripts-spec
	   :base-extension "js"
	   :base-directory base-javascripts
	   :publishing-directory public-javascripts
	   :publishing-function 'org-publish-attachment)
     (list images-spec
	   :recursive t
	   :base-extension "png\\|jpe?g\\|ico\\|svg"
	   :base-directory base-images
	   :publishing-directory public-images
	   :publishing-function 'org-publish-attachment)
     (list site-name
	   :components
	   (list content-spec
		 posts-spec
		 dotfiles-spec
		 stylesheets-spec
		 javascripts-spec
		 data-spec
		 images-spec)))))

(defun publisher-publish (&optional force)
  "Publish website."
  (interactive)
  (let ((org-publish-project-alist (publisher-project-spec)))
    (org-publish-project (publisher-site-name) force nil)))

(provide 'publisher)
;;; grt-publish.el ends here
