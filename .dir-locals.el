((less-css-mode . ((less-css-lessc-command . "npm exec lessc")
		   (less-css-compile-at-save . t)
		   (eval . (progn (setq less-css-output-directory (grt/site-content-directory 'base "stylesheets")))))))
