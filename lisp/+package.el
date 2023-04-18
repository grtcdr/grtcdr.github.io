;;; +package.el  -*- lexical-binding:t -*-

(require 'package)
(require 'package-vc)

(defvar +package--vc-providers
  '(:github "https://github.com" :sourcehut "https://git.sr.ht")
  "Property list of version control providers and their associated domains.")

(defun +package--vc-repo (provider slug)
  "Construct the repository name from PROVIDER and SLUG."
  (let ((domain (plist-get +package--vc-providers provider)))
    (cond ((eq provider :sourcehut) (format "%s/~%s" domain slug))
	  (t (format "%s/%s" domain slug)))))

(defun +package--initialize ()
  "Initialize the package manager and its archives."
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))

(defun +package-install (packages)
  "Install the list of PACKAGES."
  (+package--initialize)
  (dolist (package packages)
    (if (plistp package)
	(let ((provider (car package))
	      (slug (cadr package)))
	  (when (plist-member +package--vc-providers provider)
	    (let* ((base (+package--vc-repo provider slug))
		   (package (intern (file-name-base slug))))
	      (unless (package-installed-p package)
		(package-vc-install base :last-release)))))
      (unless (package-installed-p package)
	(package-install package)))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(+package-install '(citeproc htmlize plantuml-mode toml-mode ox-rss (:github "grtcdr/liaison")))
