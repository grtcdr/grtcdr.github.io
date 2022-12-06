LESSC       = npm exec -- lessc
CSS_DIR     = src/css
LESS_FILES := $(wildcard $(CSS_DIR)/*.less)
LESS_FILES := $(filter-out $(CSS_DIR)/def.less, $(LESS_FILES))
CSS_FILES   = $(LESS_FILES:.less=.css)

GRUNT       = npm exec -- grunt
JS_DIR      = src/js
GRUNTFILE   = $(JS_DIR)/grunt.js

LISP_DIR    = lisp

all: less build optimize

less: $(CSS_FILES)

%.css: %.less
	@echo "Publishing file $< to $@"
	@$(LESSC) $< $@

build:
	@emacs --quick --batch --load $(LISP_DIR)/publish.el --funcall org-publish-all t t

optimize:
	@$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

serve: all
	@miniserve public

clean:
	@rm -rvf public/
	@rm -rvf .cache/
