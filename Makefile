# Run an arbitrary Emacs version like
#   make test emacs="docker run --rm -it -v $PWD:/work -w /work silex/emacs:26 emacs"
emacs := emacs
elpa_dir := elpa
run_emacs = $(emacs) -Q --batch -L . -L $(elpa_dir) -l package \
	--eval '(setq package-user-dir (expand-file-name "$(elpa_dir)"))' \
	--eval '(package-initialize)'

dependencies := transient

.PHONY: test
test: ## Compile and run unit tests
test: test-compile test-unit

$(elpa_dir):
	$(run_emacs) \
		--eval '(make-directory "$(@)")' \
		--eval "(unless (seq-every-p (lambda (e) (require e nil t)) '($(dependencies))) \
			(package-refresh-contents) (mapc #'package-install '($(dependencies))))"

.PHONY: deps
deps: $(elpa_dir)

.PHONY: test-unit
test-unit:
	$(run_emacs) \
		-l ert \
		$(addprefix -l ,$(wildcard test/*.el)) \
		-f ert-run-tests-batch-and-exit

.PHONY: test-compile
test-compile: | $(elpa_dir)
	$(run_emacs) \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile *.el

.PHONY: prettify
prettify: ## Auto-format code
prettify: | $(elpa_dir)
	find . -name '*.el' -print0 | xargs -P 0 -0 -I {} \
		$(run_emacs) \
		$(addprefix -l ,$(dependencies)) \
		{} \
		--eval '(setq indent-tabs-mode nil tab-width 4 require-final-newline t)' \
		--eval '(indent-region (point-min) (point-max))' \
		--eval '(whitespace-cleanup)' \
		--eval '(save-buffer)' \
		> /dev/null

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
hooks: $(addprefix $(git_dir)/,$(hooks))

$(git_dir)/hooks/%: hooks/%
	ln -s $(PWD)/$(<) $(@)

.PHONY: help
help: ## Show this help text
	$(info usage: make [target])
	$(info )
	$(info Available targets:)
	@awk -F ':.*?## *' '/^[^\t].+?:.*?##/ \
         {printf "  %-24s %s\n", $$1, $$2}' $(MAKEFILE_LIST)
