
RESCRIPT=yarn rescript
BSC=bsc
RETEST=yarn retest
EJS=yarn ejs
MKDOCS=mkdocs

ALL_RES=$(wildcard src/*.res*) $(wildcard tests/*.res*)

.PHONY: build
build:
	$(RESCRIPT)

.PHONY: dev
dev:
	$(RESCRIPT) -w

.PHONY: dev_docs
dev_docs:
	$(MKDOCS) serve --dev-addr "localhost:8880"

.PHONY: test
test: build
	$(RETEST) tests/*.bs.js

.PHONY: prettify
prettify:
	$(RESCRIPT) format -all

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

.PHONY: objectn.res
objectn.res:
	$(EJS) ./templates/objectN.res.ejs

.PHONY: objectn.resi
objectn.resi:
	$(EJS) ./templates/objectN.resi.ejs
