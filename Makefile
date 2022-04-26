emacs := emacs
run_emacs = $(emacs) -Q -L . -L $(elpa_dir) \
	--eval "(setq package-user-dir (expand-file-name \"$(elpa_dir)\"))" \
	--eval "(progn (require 'package) (package-initialize))"
elpa_dir := elpa
el_files := $(wildcard *.el)

$(elpa_dir):
	$(run_emacs) \
		--eval "(unless (require 'transient nil t) \
			(package-refresh-contents) (package-install 'transient))" \
		--batch

.PHONY: deps
deps: $(elpa_dir)

.PHONY: test
test: ## Compile and run unit tests
test: test-compile test-unit

.PHONY: test-unit
test-unit:
	$(run_emacs) --batch \
		-l ert -l test/macports-test.el -f ert-run-tests-batch-and-exit

.PHONY: test-compile
test-compile: $(el_files) $(elpa_dir)
	$(run_emacs) \
		--eval '(setq byte-compile-error-on-warn t)' \
		--batch -f batch-byte-compile $(el_files)

.PHONY: clean
clean: ## Clean files
	rm -f *.elc

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
