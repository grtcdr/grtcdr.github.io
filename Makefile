# ---- Variables ----

# Behind the scenes, `npm {x|exec} lessc` locates the `lessc`
# executable and runs it
LESSC = npx lessc
GRUNT = npx grunt

GRUNTFILE = javascripts/grunt.js
COMPOSITE = lisp/composite/composite.el

# These two variables indicate the source and destination directories
LESS_DIR = stylesheets
CSS_DIR = public/stylesheets

# Get a list of all less and css files
LESS_FILES = $(wildcard $(LESS_DIR)/*.less)
CSS_FILES = $(patsubst $(LESS_DIR)/%.less, $(CSS_DIR)/%.css, $(LESS_FILES))

# ---- Recipes ----

all: less publish

less: $(CSS_FILES)

# Compile `.less` files into `.css` files
$(CSS_FILES): $(LESS_FILES)
	@echo "Publishing file $< to $@"
	@$(LESSC) $< $@

# Publish the website
publish: $(COMPOSITE)
	@emacs --quick --batch --load $(COMPOSITE) --funcall org-publish-all t t

optimize:
	$(GRUNT) cssmin --no-color --gruntfile $(GRUNTFILE) --base .

# Recipe to clean the artifacts produced by the `publish` recipe.
clean:
	@rm -rvf public/
	@rm -rvf .cache/
