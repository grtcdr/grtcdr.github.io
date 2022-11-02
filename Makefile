# ---- Variables ----

LESSC = npx lessc
GRUNT = npx grunt

GRUNTFILE = javascripts/grunt.js

LESS_DIR = stylesheets
CSS_DIR = public/stylesheets

LESS_FILES = $(wildcard $(LESS_DIR)/*.less)
CSS_FILES = $(patsubst $(LESS_DIR)/%.less, $(CSS_DIR)/%.css, $(LESS_FILES))

# ---- Recipes ----

all: submodule less build optimize

submodule:
	@echo "Updating submodules..."
	@git submodule update --remote --merge

serve:
	miniserve public/

build: publish.el
	@emacs --quick --batch --load publish.el --funcall org-publish-all t t

less: $(CSS_FILES)

$(CSS_FILES): $(LESS_FILES)
	@echo "Publishing file $< to $@"
	@$(LESSC) $< $@

optimize:
	@$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

clean:
	@rm -rvf public/
	@rm -rvf .cache/
