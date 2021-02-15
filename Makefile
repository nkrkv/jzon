
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
	for file in $^ ; do \
	  $(BSC) -format $${file} > $${file}.fmt ; \
	  mv $${file}.fmt $${file} ; \
	done
