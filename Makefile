CSS_FILES := $(patsubst %.less, %.css, $(wildcard ./stylesheets/*.less))
LESSC = npm x lessc

all: css publish

.PHONY: all

./stylesheets/%.css: stylesheets/%.less
	@echo "$< -> $@"
	$(LESSC) $< $@

css: $(CSS_FILES)

publish: publish.el
	@echo "Publishing..."
	emacs --batch --load publish.el --funcall org-publish-all

clean:
	@echo "Cleaning up..."
	@rm -rvf public
	@rm .timestamps/grtcdr.tn/*
