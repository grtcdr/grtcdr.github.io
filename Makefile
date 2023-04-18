pub_dir   = public
less_dir  = src/less
css_dir   = $(pub_dir)/css
js_dir    = src/js
lisp_dir  = lisp
cache_dir = .cache
emacs_dir = .emacs

less_files  = blog.less common.less indent.less
less_files := $(addprefix $(less_dir)/,$(less_files))
css_files   = $(patsubst $(less_dir)/%.less, $(css_dir)/%.css, $(less_files))

less    = npm exec -- lessc --verbose --no-color
grunt   = npm exec -- grunt --gruntfile $(js_dir)/grunt.js --no-color
emacs   = emacs --batch --quick --init-directory=$(emacs_dir)

all: less build

less: $(css_files)

$(css_dir)/%.css: $(less_dir)/%.less
	@$(less) $< $@

build:
	@rm -rf $(cache_dir)
	$(emacs) -l $(lisp_dir)/+publish.el -f org-publish-all

cssmin:
	@$(grunt) cssmin --base .

serve: all
	@python -m http.server --directory $(pub_dir)

clean:
	@rm -rf $(css_files)
	@rm -rf $(pub_dir)
	@rm -rf $(cache_dir)
