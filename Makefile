
BSB=yarn bsb
BSC=bsc
RETEST=yarn retest
MKDOCS=mkdocs

ALL_RES=$(wildcard src/*.res*) $(wildcard tests/*.res*)

.PHONY: build
build:
	$(BSB) -make-world -clean-world

.PHONY: dev
dev:
	$(BSB) -make-world -clean-world -w

.PHONY: dev_docs
dev_docs:
	$(MKDOCS) serve --dev-addr "localhost:8880"

.PHONY: test
test: build
	$(RETEST) tests/*.bs.js

.PHONY: prettify
prettify: $(ALL_RES)
	@echo "Prettifying..."
	@for file in $^ ; do \
	  $(BSC) -format $${file} > $${file}.fmt ; \
	  mv $${file}.fmt $${file} ; \
	  echo $${file} ; \
	done

.PHONY: bump_version
bump_version: test prettify
	@echo -n "Is CHANGELOG.md actual? [y/N] " && \
	    read CHANGELOG_ACTUAL && \
	    [ $${CHANGELOG_ACTUAL:-N} = y ] || \
	    (echo "Update CHANGELOG.md, commit, then try again" && exit 1)
	@echo -n "Which semver component to bump? [major/minor/patch] " && \
	    read VERSION_TO_BUMP && \
	    npm version $${VERSION_TO_BUMP} --message "chore: bump version to %s"
	@echo "Version bumped. Now run:"
	@echo ""
	@echo "make publish"
	@echo ""

.PHONY: publish
publish:
	git push
	git push --tags
	npm publish
	$(MKDOCS) gh-deploy
