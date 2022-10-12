;;; forgecast.el --- Helper functions for linking resources to their forges.

;; Copyright (C) 2022 Aziz Ben Ali

;; Author: Aziz Ben Ali <ba.tahaaziz@gmail.com>
;; Homepage: https://github.com/grtcdr/grtcdr.tn

;; Forgecast is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; Forgecast is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Stack. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Forgecast provides a set of helper functions that allow the user to
;; link various resources to the forge that hosts them.

;;; Code:

(require 'project)
(require 'vc)

(defvar forgecast-forge-plist
  '(:github "github.com"
    :github-usercontent "raw.githubusercontent.com"
    :sourcehut "git.sr.ht")
  "A property list mapping forges to their respective domain.")

(defun forgecast-build-url (forge slug type)
  "Construct the standard URL of a given FORGE by specifying
the repository SLUG and the TYPE of information to access.

FORGE is a property from the ’forges’ variable.

SLUG is a string and the combination of your username and the
name of your repository, e.g. \"octopus/website\".

TYPE can take a value of ’log’, ’tree’ or ’blob’.

RESOURCE is a filename relative to the root of the project."
  (let ((branch "main"))
    (cond ((equal forge :github)
	    (format-spec
	     "https://%d/%s/%t/%b/%r"
	     `((?d . ,(plist-get forgecast-forge-plist :github))
	       (?s . ,slug)
	       (?t . ,(cond ((eq type 'log) "commits")
			    ((eq type 'tree) "blob")
			    (t (error "Type is invalid or does not apply to this backend."))))
	       (?b . ,branch)
	       (?r . ,(forgecast-get-resource-slug)))))
	  ((equal forge :sourcehut)
	    (format-spec
	     "https://%d/%s/%t/%b/%x/%r"
	     `((?d . ,(plist-get forgecast-forge-plist :sourcehut))
	       (?s . ,(concat "~" slug))
	       (?t . ,(cond ((eq type 'log) "log")
			    ((eq type 'tree) "tree")
			    ((eq type 'blob) "blob")
			    (t (error "Type is invalid or does not apply to this backend."))))
	       (?b . ,branch)
	       (?x . ,(cond ((eq type 'blob) "")
			    (t "item")))
	       (?r . ,(forgecast-get-resource-slug)))))
	  (t (error "Could not find forge from known list of forges.")))))

(defun forgecast-get-resource-slug ()
  "Determines the slug i.e. path of a resource (the current buffer)
relative to the value returned by ’forgecast-build-prefix-url'."
  (let* ((buffer (buffer-file-name))
	 (root (vc-find-root buffer ".git")))
    (string-remove-prefix
     (expand-file-name root) buffer)))

(defun forgecast-get-url-as-html (forge slug type text)
  "Return the URL of a resource (the current buffer) as an HTML link."
  (format "<a href=%s>%s</a>"
	  (forgecast-build-url forge slug type) text))

(provide 'forgecast)
