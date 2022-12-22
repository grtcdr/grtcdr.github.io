LESSC       = npm exec -- lessc
LESS_DIR    = src/less
LESS_IGNORE = def.less
LESS_FILES := $(wildcard $(LESS_DIR)/*.less)
LESS_FILES := $(filter-out $(LESS_IGNORE:%=$(LESS_DIR)/%), $(LESS_FILES))
CSS_DIR     = src/css
CSS_FILES   = $(patsubst $(LESS_DIR)/%.less, $(CSS_DIR)/%.css, $(LESS_FILES))

GRUNT       = npm exec -- grunt
JS_DIR      = src/js
GRUNTFILE   = $(JS_DIR)/grunt.js

LISP_DIR    = lisp

all: less optimize build

less: $(CSS_FILES)

$(CSS_DIR)/%.css: $(LESS_DIR)/%.less
	@echo "Publishing file $< to $@"
	@$(LESSC) $< $@

build:
	@emacs --quick --batch --load $(LISP_DIR)/publish.el --funcall org-publish-all t t

optimize:
	@$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

serve: all
	@miniserve public

clean:
	@rm -rvf $(CSS_FILES)
	@rm -rvf public/
	@rm -rvf .cache/
