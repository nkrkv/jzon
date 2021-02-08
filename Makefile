
BSB=yarn bsb
BSC=bsc
RETEST=yarn retest

ALL_RES=$(wildcard src/*.res*) $(wildcard tests/*.res*)

.PHONY: build
build:
	$(BSB) -make-world -clean-world

.PHONY: dev
dev:
	$(BSB) -make-world -clean-world -w

.PHONY: test
test: build
	$(RETEST) tests/*.bs.js

.PHONY: prettify
prettify: $(ALL_RES)
	for file in $^ ; do \
	  $(BSC) -format $${file} > $${file}.fmt ; \
	  mv $${file}.fmt $${file} ; \
	done
