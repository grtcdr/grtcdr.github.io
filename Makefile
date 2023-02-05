LESSC       = npm exec -- lessc
LESS_DIR    = src/less
LESS_FILES := $(wildcard $(LESS_DIR)/*.less)

CSS_DIR     = public/css
CSS_FILES   = $(patsubst $(LESS_DIR)/%.less, $(CSS_DIR)/%.css, $(LESS_FILES))

LISP_DIR    = lisp
JS_DIR      = src/js

GRUNT       = npm exec -- grunt
GRUNTFILE   = $(JS_DIR)/grunt.js

all: less optimize build

less: $(CSS_FILES)

$(CSS_DIR)/%.css: $(LESS_DIR)/%.less
	@$(LESSC) $< $@

build:
	@rm -rf .cache
	@emacs -Q --script $(LISP_DIR)/op-publish.el --funcall org-publish-all

optimize:
	@$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

serve: less build
	@miniserve public

clean:
	@rm -rf $(CSS_FILES)
	@rm -rf public
	@rm -rf .cache
