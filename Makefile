elpa_dir := elpa
el_files := $(wildcard *.el)

$(elpa_dir):
	emacs -Q -L $(elpa_dir) \
		--eval "(setq package-user-dir \"$$PWD/$(elpa_dir)\")" \
		--eval "(unless (require 'transient nil t) \
			(require 'package) (package-initialize) (package-refresh-contents) (package-install 'transient))" \
		--batch

.PHONY: deps
deps: $(elpa_dir)

.PHONY: test
test: ## Run regular test (full dependencies)
test: $(el_files) $(elpa_dir)
	emacs -Q -L . -L $(elpa_dir) \
		--eval '(setq byte-compile-error-on-warn t)' \
		--batch -f batch-byte-compile $(el_files)

.PHONY: clean
clean: ## Clean files
	rm *.elc

.PHONY: clobber
clobber: ## Remove all generated files
clobber: clean
	rm -rf $(elpa_dir)

# Hooks

hooks := $(filter-out %~,$(wildcard hooks/*))
git_dir := $(shell git rev-parse --git-dir)

.PHONY: hooks
hooks: ## Install helpful git hooks
hooks: $(foreach _,$(hooks),$(git_dir)/hooks/$(notdir $(_)))

$(git_dir)/hooks/%: hooks/%
	ln -s $(PWD)/$(<) $(@)

.PHONY: help
help: ## Show this help text
	$(info usage: make [target])
	$(info )
	$(info Available targets:)
	@awk -F ':.*?## *' '/^[^\t].+?:.*?##/ \
         {printf "  %-24s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
