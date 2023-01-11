LESSC       = npm exec -- lessc
LESS_DIR    = src/less
LESS_FILES := $(wildcard $(LESS_DIR)/*.less)
# Ignore a (list of) file(s):
# LESS_IGNORE = def.less
# LESS_FILES := $(filter-out $(LESS_IGNORE:%=$(LESS_DIR)/%), $(LESS_FILES))

CSS_DIR     = src/css
CSS_FILES   = $(patsubst $(LESS_DIR)/%.less, $(CSS_DIR)/%.css, $(LESS_FILES))

LISP_DIR    = lisp
JS_DIR      = src/js

GRUNT       = npm exec -- grunt
GRUNTFILE   = $(JS_DIR)/grunt.js

CI?=false

all: less optimize build

less: $(CSS_FILES)

$(CSS_DIR)/%.css: $(LESS_DIR)/%.less
	@$(LESSC) $< $@

build:
	@emacs -Q --script $(LISP_DIR)/publish.el --funcall org-publish-all t t

optimize:
	@$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

serve: less build
	@miniserve public

cv:
ifeq ($(CI),false)
	cd src/cv && make
endif

clean:
	@rm -rf $(CSS_FILES)
	@rm -rf public/
	@rm -rf .cache/
