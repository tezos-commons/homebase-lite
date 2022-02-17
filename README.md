<!--
   - SPDX-FileCopyrightText: 2022 Tezos Commons
   -
   - SPDX-License-Identifier: LicenseRef-MIT-TC
   -->

# Homebase Lite

[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT) [![Build status](https://badge.buildkite.com/680d303398dd927c109b9c0324e7eacf9c7e2e422210ffb746.svg?branch=master)](https://buildkite.com/serokell/homebase-lite)

Homebase Lite is a Tezos smart contract for a decentralized voting system, based
on “snapshots” of the blockchain.

## Documentation

[The contract specification](docs/specification.md) clarifies the intent,
requirements, and choices used for the development of this smart contract.

An automatically generated documentation, reflecting how the contract is
actually implemented, can instead be obtained by from [the latest
release](https://github.com/tezos-commons/homebase-lite/releases/latest)
or produced with [the `homebase-lite` tool](docs/tool.md#getting-the-contract-documentation).

## The `homebase-lite` tool

This repository includes a [`homebase-lite` utility tool](docs/tool.md), which
can be used to deploy, compile and document the smart contract easily.

See its dedicated documentation on [how to obtain it](docs/tool.md#how-to-get)
and [how to use it](docs/tool.md#usage).

## Manual deployment

If you prefer not to use the `homebase-lite` tool and instead want to originate
the smart contract yourself, you can obtain the pre-compiled Michelson source
code from [the latest release](https://github.com/tezos-commons/homebase-lite/releases/latest) and originate it using existing software, such as
`tezos-client`.

## Testing

This smart contract is covered by a test suite, built using
[`cleveland`](https://gitlab.com/morley-framework/morley/-/tree/master/code/cleveland).

To learn more about the tests and how to run them, see [their documentation](docs/testing.md).

## For Contributors

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for more information.

## License

[MIT License](./LICENSES/LicenseRef-MIT-TC.txt) Copyright (c) 2022 Tezos Commons
