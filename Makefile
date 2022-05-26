# Run an arbitrary Emacs version like
#   make test emacs="docker run --rm -it -v $PWD:/work -w /work silex/emacs:26 emacs"
emacs := emacs
run_emacs = $(emacs) -Q -L . -L $(elpa_dir) -l package \
	--eval "(setq package-user-dir (expand-file-name \"$(elpa_dir)\"))" \
	--eval "(package-initialize)"
elpa_dir := elpa

dependencies := transient

.PHONY: test
test: ## Compile and run unit tests
test: test-compile test-unit

$(elpa_dir):
	$(run_emacs) \
		--eval "(unless (seq-every-p (lambda (e) (require e nil t)) '($(dependencies))) \
			(package-refresh-contents) (mapc #'package-install '($(dependencies))))" \
		--batch

.PHONY: deps
deps: $(elpa_dir)

.PHONY: test-unit
test-unit:
	$(run_emacs) --batch \
		-l ert $(foreach _,$(wildcard test/*.el),-l $(_)) -f ert-run-tests-batch-and-exit

.PHONY: test-compile
test-compile: | $(elpa_dir)
	$(run_emacs) \
		--eval '(setq byte-compile-error-on-warn t)' \
		--batch -f batch-byte-compile *.el

.PHONY: prettify
prettify: ## Auto-format code
prettify: el_files := find . -name '*.el' -print0
prettify: | $(elpa_dir)
	$(el_files) | xargs -P 0 -0 -I {} \
		$(run_emacs) \
		$(foreach _,$(dependencies),-l $(_)) \
		--eval '(setq-default indent-tabs-mode nil tab-width 4 require-final-newline t)' \
		{} \
		--eval '(indent-region (point-min) (point-max))' \
		--eval '(whitespace-cleanup)' \
		--eval '(save-buffer)' \
		--batch > /dev/null

.PHONY: prettify-staged
prettify-staged: staged_el_files := git diff -z --cached --name-only --diff-filter=ACMR | grep -z '\.el'
prettify-staged:
	modified_el=$$($(staged_el_files) | xargs -0); \
	for file in $$modified_el; do \
		git show ":$$file" >"$$file.tmp.el"; \
	done; \
	$(MAKE) prettify el_files="($(staged_el_files); find . -name '*.tmp.el' -print0)"; \
	for file in $$modified_el; do \
		hash=$$(git hash-object -w "$$file.tmp.el"); \
		git update-index --add --cacheinfo 100644 "$$hash" "$$file"; \
	done; \
	find . -name '*.tmp.el' -delete; \
	if [ -n "$$modified_el" ] && [ -z "$$(git diff --cached --name-only)" ]; then \
		echo "No files left after formatting" 1>&2; exit 1; \
	fi

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
