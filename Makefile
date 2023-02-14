pub_dir   = public
css_dir   = $(pub_dir)/css
less_dir  = src/less
js_dir    = src/js
lisp_dir  = lisp
cache_dir = .cache
emacs_dir = .emacs

less_files := $(wildcard $(less_dir)/*.less)
css_files   = $(patsubst $(less_dir)/%.less, $(css_dir)/%.css, $(less_files))

less    = npm exec -- lessc --verbose --no-color
grunt   = npm exec -- grunt --gruntfile $(js_dir)/grunt.js --no-color
emacs   = emacs --quick --init-directory=$(emacs_dir)

all: less build

less: $(css_files)

$(css_dir)/%.css: $(less_dir)/%.less
	@$(less) $< $@

build:
	@rm -rf $(cache_dir)
	$(emacs) --script $(lisp_dir)/op-publish.el --funcall org-publish-all

cssmin:
	@$(grunt) cssmin --base .

htmlmin:
	@$(grunt) htmlmin --base .

serve: less build
	@miniserve $(pub_dir)

clean:
	@rm -rf $(css_files)
	@rm -rf $(pub_dir)
	@rm -rf $(cache_dir)
