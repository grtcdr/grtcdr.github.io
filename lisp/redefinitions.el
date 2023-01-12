(require 'ox-html)
(require 'liaison)

(defun org-html-format-spec (info)
  "Return format specification for preamble and postamble.
INFO is a plist used as a communication channel."
  `((?a . ,(org-export-data (plist-get info :author) info))
    (?t (org-export-data (plist-get info :title) info))
    (?m . ,(plist-get info :email))
    (?e . ,(liaison-get-resource-url 'edit))
    (?l . ,(liaison-get-resource-url 'log))))

(provide 'redefinitions)
