# ---- Variables ----

# Behind the scenes, `npm {x|exec} lessc` locates the `lessc`
# executable and runs it
LESSC = npm exec lessc

# Convert and store filenames from `.less` to `.css`
CSS_FILES := $(patsubst %.less, %.css, $(wildcard ./stylesheets/*.less))

# ---- Recipes ----

all: less publish

less: $(CSS_FILES)

# Compile `.less` files into `.css` files
./stylesheets/%.css: stylesheets/%.less
	@echo "$< -> $@"
	$(LESSC) $< $@

# Publish the website
publish: publish.el
	@echo "Publishing..."
	emacs --quick --batch --load publish.el --funcall org-publish-all t t
	@rm $(CSS_FILES)

# Recipe to clean the artifacts produced by the `publish` recipe.
clean:
	@echo "Cleaning up..."
	@rm -rvf public
	@rm -rvf .timestamps
