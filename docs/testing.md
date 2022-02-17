<!--
   - SPDX-FileCopyrightText: 2022 Tezos Commons
   -
   - SPDX-License-Identifier: LicenseRef-MIT-TC
   -->

## Running the tests

If you want to run the included automated test suite, you'll need to first [build from source](tool.md#build-instructions).

There are two kind of tests: emulated tests and network tests.

To run emulated tests only, in the root of the repository run `stack test`. This will build the tests if needed and run them, printing the report to standard output.

To run the network tests, you will need to setup an implicit account with plenty of XTZs, as repeated originations can get quite costly (so you probably want to run those on the testnet). You will also need `tezos-client` binary.

To run network tests only, use the following command:

```bash
$ make network-test \
    NODE_ENDPOINT='https://testnet.endpoint.url/' \
    MONEYBAG_SECRET_KEY='unencrypted:edsk...'
```

This uses `stack test` under the hood, so you might as well just run the command
directly, however using `make` removes some boilerplate.

Note that Makefile assumes `mktemp` from [GNU Coreutils](https://www.gnu.org/software/coreutils/) is available, in particular that `mktemp -d` creates a temporary directory and prints a full path to it.
