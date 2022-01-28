# SPDX-FileCopyrightText: 2022 Tezos Commons
#
# SPDX-License-Identifier: LicenseRef-MIT-TC

.PHONY: homebase-lite test haddock haddock-no-deps stylish lint clean all

# Build target from the common utility Makefile
MAKEU = $(MAKE) -C make/

MAKE_PACKAGE = $(MAKEU) PACKAGE=homebase-lite

homebase-lite:
	$(MAKE_PACKAGE) dev
test:
	$(MAKE_PACKAGE) test
test-dumb-term:
	$(MAKE_PACKAGE) test-dumb-term
test-hide-successes:
	$(MAKE_PACKAGE) test-hide-successes
haddock:
	$(MAKE_PACKAGE) haddock
haddock-no-deps:
	$(MAKE_PACKAGE) haddock-no-deps
clean:
	$(MAKE_PACKAGE) clean

stylish:
	find src/ test/ app/ -name '*.hs' -exec stylish-haskell -i '{}' \;

all:
	$(MAKEU) PACKAGE=""
test-all:
	$(MAKEU) test PACKAGE=""
