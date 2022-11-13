LESSC = npx lessc
GRUNT = npx grunt
GRUNTFILE = src/js/grunt.js
LESS_DIR = src/less
CSS_DIR = public/css
LESS_FILES = $(wildcard $(LESS_DIR)/*.less)
CSS_FILES = $(patsubst $(LESS_DIR)/%.less, $(CSS_DIR)/%.css, $(LESS_FILES))

all: less build optimize

less: $(CSS_FILES)

$(CSS_FILES): $(LESS_FILES)
	@echo "Publishing file $< to $@"
	@$(LESSC) $< $@

build:
	@emacs --quick --batch --load lisp/publish.el --funcall org-publish-all t t

optimize:
	@$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

clean:
	@rm -rvf public/
	@rm -rvf .cache/
