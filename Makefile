CSS_FILES := $(patsubst %.less, %.css, $(wildcard ./stylesheets/*.less))
LESSC = npm x lessc

all: css publish

css: $(CSS_FILES)

./stylesheets/%.css: stylesheets/%.less
	@echo "$< -> $@"
	$(LESSC) $< $@

publish: publish.el
	@echo "Publishing..."
	emacs --batch --load publish.el --funcall org-publish-all

clean:
	@echo "Cleaning up..."
	@rm -rvf public/
	@rm -rvf .timestamps
	rm -f $(DIAGRAMS_SVG)
