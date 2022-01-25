<!--
   - SPDX-FileCopyrightText: 2022 Tezos Commons
   -
   - SPDX-License-Identifier: LicenseRef-MIT-TC
   -->

# Homebase Lite Spec

This spec is for the Tezos smart contract to be used as the on-chain component of a new lightweight Homebase tool.

The contract is under active development, hence changes to the spec are possible.

<!-- TOC START min:1 max:3 link:true asterisk:false update:true -->
- [Homebase Lite Spec](#homebase-lite-spec)
- [Requirements](#requirements)
    - [Roles](#roles)
    - [Configuration](#configuration)
    - [Governance token](#governance-token)
    - [Proposals and voting](#proposals-and-voting)
    - [Off-chain outcome calculation](#off-chain-outcome-calculation)
- [Entrypoints](#entrypoints)
  - [Administration](#administration)
    - [`set_admin`](#set_admin)
    - [`accept_admin`](#accept_admin)
    - [`add_maintainers`](#add_maintainers)
    - [`remove_maintainers`](#remove_maintainers)
    - [`configure`](#configure)
  - [Proposals](#proposals)
    - [`propose`](#propose)
    - [`vote`](#vote)
<!-- TOC END -->


# Requirements

This is intended to be a simpler option than [baseDAO], both in complexity and usage, for communities to request feedback, run polls, etc.

[baseDAO]: https://github.com/tezos-commons/baseDAO

In summary, the smart contract must support:

- Non-binding proposals based on an IPFS document.
- Voting from a set of choices per proposal.
- Proposal's life-cycles based on time and submitted votes.
- Multiple roles: admin, maintainer(s) and participant(s)
- Configuration options that can be modified by maintainers.
- An external “governance token” in the form of an existing FA2-compliant contract.

more details can be found below.

### Roles

This contract supports 3 roles:

1. `admin`, which is unique and must always be assigned.
The `admin` can transfer its role to someone else and can add or remove `maintainer`s.
An address for this role needs to be provided at origination.
2. `maintainer`s, which can be multiple, including none.
Any `maintainer` can change the configuration options at any time, which will affect proposals submitted from that moment until the next change.
Zero or more `maintainer`s can be assigned at origination.
3. `participant`s, which can vote and submit proposals.
This role requires no assignment, as any address (provided it has enough governance tokens) has such permissions. This can be considered a “default” role.

There are no restrictions on holding multiple roles.
In other words, for example, a `maintainer` may submit a proposal and an `admin` may also assign himself the `maintainer` role.

### Configuration

Several options are available to customize the behavior and life-cycle of proposals:

- `expire_time`, after how much time a proposal can no longer be voted on (in seconds)
- `vote_delay`, after how much time votes can be cast on a proposal (in seconds)
- `quorum_threshold` the minimum amount of votes to be cast on a proposal for it to be successful, expressed as a constant number to reach.
- `minimum_balance` the minimum amount of governance tokens needed to submit a new proposal

An initial value for all these options needs to be provided at contract origination and can be changed by `maintainer`s after that.

### Governance token

The snapshot contract requires the external governance token to be FA2 compliant.

The address and `token_id` of the governance token must be provided at origination and can never be changed.

Governance tokens ***will not*** be transferred from the accounts of the `participant`s for proposing, nor for voting.

### Proposals and voting

The life-cycle of a proposal, and interactions with it, are as follow:

1. A `participant` can submit a new proposal for a given IPFS URI, provided that it has at least a `minimum_balance` of governance tokens and that no other proposal exists for that URI.
The proposal has now the status of `submitted`, no votes can be cast on it yet.
2. After `vote_delay` the proposal status changes to `active`.
`participant`s can now submit a single vote, choosing one of the available options.
The weight of the vote **cannot** be specified and instead depends on the token held by the voter when the proposal was submitted.
3. After `expire_time` since the proposal submission, it's no longer possible to vote on it.
The proposal status is now `completed`, with an outcome depending on the votes that it received (see below).
4. From this point forward it's no longer possible to interact with the proposal nor change its status.

Technical note: the contract never stores a proposal's `status`, because it can always be calculated from its starting time and the configuration options used when it was created, which is data that is stored and doesn't change after its submission.

### Off-chain outcome calculation

To prevent vote duplication and other vote-related manipulations, in a way that supports convenient participation from the community, the contract doesn't transfer any governance token and isn't aware of the outcome of `completed` proposals.

Instead, the `level` and time in which a proposal was submitted is saved, and the outcome needs to be calculated off-chain based on the governance token's state at that level, as if a snapshot of the chain was taken at that point.

To obtain the outcome of a `completed` proposal, an external application will need to:

1. Acquire the `level`, time and configuration used when the proposal was created.
2. Get the list of all the voters for the proposal and the choice they picked
3. Calculate the governance token held by each voter at that `level`, which will be equal to the weight of their vote.
4. Sum up all vote weights, if their total number if bigger than the `quorum_threshold` then the proposal has the outcome of `passed` and the choice with the biggest cumulative weight has won. Otherwise the proposal outcome is `expired` and no choice has won.

Technical note: to acquire the data needed for these calculations, the off-chain application will need access to an indexer or perform several RPC calls.

# Entrypoints

## Administration

### `set_admin`

Called by the current `admin` to transfer its role to someone else.
Takes in input the address of the new `admin` candidate.
Note: for security reasons the transfer isn't complete until the new admin candidate calls `accept_admin`.

- fails when the sender is not the current `admin`
- if there is already a candidate, another call to this entrypoint will replace it
- if there is already a candidate, calling this with the `admin` address will invalidate the current candidate

### `accept_admin`

Called by an `admin` candidate (see `set_admin`) to complete the transfer of the role.
Takes no input.

- fails when the sender is not the current `admin` candidate

### `add_maintainers`

Takes as input a list of addresses that will become `maintainer`s

- fails when the sender is not the `admin`
- all the given addresses receive the `maintainer` role
- if a given address already has a `maintainer` role or is present in the list more than once, this call will not be affected (it's a no-op)

### `remove_maintainers`

Takes as input a list of addresses that will lose the `maintainer` role.

- fails when the sender is not the `admin`
- all the given addresses lose the `maintainer` role
- if a given addresses is not a `maintainer` this call will not be affected (it's a no-op)

### `configure`

Takes as input new values for all the **configuration options**, these new values will replace the current ones and will affect future proposals.

- fails when the sender does not have the `maintainer` role

## Proposals

### `propose`

Called by a `participant` to submit a new proposal.
Takes as input an IPFS URI and a list of strings for the `n`  available choices.

- fails if the sender does not have a governance token amount of `minimum_balance` or more
- fails if a proposal for the same IPFS URI was already submitted before
- fails if the number of choices `n` is `0`
- a new proposal for the given IPFS will be initialized (with a `submitted` status)

### `vote`

Called by a `participant` to cast a vote on a proposal.
Takes as input the proposal's IPFS URI and the choice made `x` .

- fails if there is no proposal for the given IPFS URI
- fails if there is no choice `x` available for the given proposal, aka if `x > n`
- fails if the sender already voted on this proposal before
- fails if less than `vote_delay` has passed since the proposal submission, aka the proposal is still in a `sumbitted` status
- fails if more than `expire_time` has passed since the proposal submission, aka the proposal has been completed (either as `expired` or `passed`)
