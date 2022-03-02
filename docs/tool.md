<!--
   - SPDX-FileCopyrightText: 2022 Tezos Commons
   -
   - SPDX-License-Identifier: LicenseRef-MIT-TC
   -->

# The `homebase-lite` utility tool

The `homebase-lite` utility tool, can be used to deploy, compile and document
the smart contract easily.

<!-- TOC START min:2 max:3 link:true asterisk:false update:true -->
- [Prerequisites](#prerequisites)
  - [Operating system](#operating-system)
  - [Executables](#executables)
- [How to get](#how-to-get)
  - [Build instructions](#build-instructions)
- [Usage](#usage)
  - [Getting the Michelson code of the compiled contract](#getting-the-michelson-code-of-the-compiled-contract)
  - [Getting the initial storage for the contract](#getting-the-initial-storage-for-the-contract)
  - [Getting the contract documentation](#getting-the-contract-documentation)
  - [Deploying the contract](#deploying-the-contract)
<!-- TOC END -->

## Prerequisites

### Operating system

Please note that only Linux is officially supported.
If you want to use `homebase-lite` on MacOS, you can try [building it from source](#build-instructions) using `stack`.

### Executables

In order to use the `homebase-lite` tool or run network tests, you will need to obtain the `tezos-client` executable, which is used for key storing and operation signing.

Various forms of distribution for `tezos-client` are presented in the [tezos-packaging repo](https://github.com/serokell/tezos-packaging).

## How to get

You can obtain `homebase-lite` tool either by building from source, or downloading an automatically built static executable from [the latest release](https://github.com/tezos-commons/homebase-lite/releases/latest).

### Build instructions

This project is using the [Haskell Tool Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/), please follow the link to find instructions on obtaining the `stack` tool.

Once you have `stack`, building should be as easy as cloning the repository and running `stack install` in the root of the cloned tree:

```bash
$ git clone https://github.com/tezos-commons/homebase-lite.git
$ cd homebase-lite
$ stack install
```

## Usage

Once you've obtained the `homebase-lite` executable,
use `homebase-lite --help` to get a list of available commands.

You can also get help for a specific command by running `homebase-lite COMMAND --help`.

### Getting the Michelson code of the compiled contract

Use `homebase-lite print` to get the Michelson code of the contract.

This will save the contract code to `homebase-lite.tz` in the current working directory. Alternatively, add `--output` option to change the output path, or use `-` to print to standard output:

```bash
$ homebase-lite print --output -
```

### Getting the initial storage for the contract

Use `homebase-lite storage` to get the initial storage for the contract.

You'll need to at least specify the admin keyhash using the `--admin` option and the address of the FA2 contract that will be used to check for governance token balance with `--fa2-address` option, for example:

```bash
$ homebase-lite storage --admin tz1Znvao83anTh654Uu44Kb9atTp5yTK93A8 --fa2-address KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9
```

You can also specify other initial values. Consult `homebase-lite storage --help` for more information.

### Getting the contract documentation

Use `homebase-lite document`. That will produce `homebase-lite.md` file in the current working directory. This documentation is also attached to binary releases.

You can use `--output` option to specify a different output path, or use `-` to print to standard output.

### Deploying the contract

You could print the contract's Michelson code and initial storage and then just use `tezos-client originate` if you wanted to, but the `homebase-lite` tool provides a more convenient command: `homebase-lite originate`. You will need to have `tezos-client` available on your system.

You'll need to specify at least the FA2 contract address and administrator's keyhash. You also might need to specify Tezos node endpoint URL with `--endpoint` (but if you don't, the one from `tezos-client` config will be used):

```bash
$ homebase-lite originate --fa2-address KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9 --admin tz1Znvao83anTh654Uu44Kb9atTp5yTK93A8
```

If `tezos-client` is not in PATH, you will need to specify the full path to it with `--client-path` option, e.g.

```bash
$ homebase-lite originate --fa2-address KT1AbgvM5D1wAVZbsB3WTCB54E94p2hn1Rb9 --admin tz1Znvao83anTh654Uu44Kb9atTp5yTK93A8 --client-path /path/to/tezos-client
```

Note that the origination is performed as the admin address, so you can not specify the admin address for an account you don't have the private key for. Also, obviously, the admin account needs enough tz for the origination.

Please also note that there is no way to change the fa2 contract address after the contract is originated, so double-check if it's correct.

Upon successful origination, the contract's address is printed to stdout and stored in `tezos-client`'s alias store with the alias `homebase-lite`. Note that if the alias already exists it will be overwritten. You can specify a different alias with the `--name` option.

You can also specify FA2 token id and other initial storage parameters. See `homebase-lite originate --help` for more information.
