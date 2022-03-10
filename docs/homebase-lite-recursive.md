# Homebase Lite

**Code revision:** [6914305](https://github.com/tezos-commons/homebase-lite/commit/69143054a89a6b671f7d327d6085caf3b0af21db) *(Thu Mar 10 18:51:00 2022 +0100)*



Homebase Lite is an offchain, decentralized voting system. It aims to
provide a very simple interface for communities to propose and vote on
decisions that do not require binding actions, e.g. to request feedback,
run polls, etc.

It aims to be a very lightweight complement to Homebase.

<a name="section-Table-of-contents"></a>

## Table of contents

- [Haskell ⇄ Michelson conversion](#section-Haskell-c8644-Michelson-conversion)
- [Storage](#section-Storage)
  - [Storage](#storage-Storage)
- [Entrypoints](#section-Entrypoints)
  - [admin](#entrypoints-admin)
  - [voting](#entrypoints-voting)

**[Definitions](#definitions)**

- [Types](#section-Types)
  - [()](#types-lparenrparen)
  - [(a, b)](#types-lparenacomma-brparen)
  - [Address](#types-Address)
  - [AdminParameter](#types-AdminParameter)
  - [BalanceRequestItem](#types-BalanceRequestItem)
  - [BalanceResponseItem](#types-BalanceResponseItem)
  - [BigMap](#types-BigMap)
  - [ByteString](#types-ByteString)
  - [Configuration](#types-Configuration)
  - [FA2Config](#types-FA2Config)
  - [Integer](#types-Integer)
  - [List](#types-List)
  - [Named entry](#types-Named-entry)
  - [Natural](#types-Natural)
  - [ProposalInfo](#types-ProposalInfo)
  - [Seconds](#types-Seconds)
  - [Text](#types-Text)
  - [Timestamp](#types-Timestamp)
  - [TokenId](#types-TokenId)
  - [URI](#types-URI)
  - [VotingParameter](#types-VotingParameter)
- [Errors](#section-Errors)
  - [AlreadyVoted](#errors-AlreadyVoted)
  - [DuplicateProposal](#errors-DuplicateProposal)
  - [EmptyChoices](#errors-EmptyChoices)
  - [NoFA2Contract](#errors-NoFA2Contract)
  - [NoSuchChoice](#errors-NoSuchChoice)
  - [NoSuchProposal](#errors-NoSuchProposal)
  - [NotEnoughTokens](#errors-NotEnoughTokens)
  - [ProposalExpired](#errors-ProposalExpired)
  - [ProposalNotYetActive](#errors-ProposalNotYetActive)
  - [SenderIsNotAdminCandidate](#errors-SenderIsNotAdminCandidate)
  - [SenderIsNotAdmin_](#errors-SenderIsNotAdmin_)
  - [SenderIsNotMaintainer](#errors-SenderIsNotMaintainer)



<a name="section-Haskell-c8644-Michelson-conversion"></a>

## Haskell ⇄ Michelson conversion

This smart contract is developed in Haskell using the [Morley framework](https://gitlab.com/morley-framework/morley). Documentation mentions Haskell types that can be used for interaction with this contract from Haskell, but for each Haskell type we also mention its Michelson representation to make interactions outside of Haskell possible.

There are multiple ways to interact with this contract:

* Use this contract in your Haskell application, thus all operation submissions should be handled separately, e.g. via calling `tezos-client`, which will communicate with the `tezos-node`. In order to be able to call `tezos-client` you'll need to be able to construct Michelson values from Haskell.

  The easiest way to do that is to serialize Haskell value using `lPackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs) module, encode resulting bytestring to hexadecimal representation using `encodeHex` function. Resulting hexadecimal encoded bytes sequence can be decoded back to Michelson value via `tezos-client unpack michelson data`.

  Reverse conversion from Michelson value to the Haskell value can be done by serializing Michelson value using `tezos-client hash data` command, resulting `Raw packed data` should be decoded from the hexadecimal representation using `decodeHex` and deserialized to the Haskell value via `lUnpackValue` function from [`Lorentz.Pack`](https://gitlab.com/morley-framework/morley/-/blob/2441e26bebd22ac4b30948e8facbb698d3b25c6d/code/lorentz/src/Lorentz/Pack.hs).

* Construct values for this contract directly on Michelson level using types provided in the documentation.

<a name="section-Storage"></a>

## Storage

<a name="storage-Storage"></a>

---

### `Storage`

Contract storage.

**Structure:** 
  * ***admin*** :[`Address`](#types-Address)
  * ***adminCandidate*** :[`Address`](#types-Address)
  * ***maintainers*** :[`BigMap`](#types-BigMap) [`Address`](#types-Address) [`()`](#types-lparenrparen)
  * ***configuration*** :[`Configuration`](#types-Configuration)
  * ***FA2Config*** :[`FA2Config`](#types-FA2Config)
  * ***proposals*** :[`BigMap`](#types-BigMap) (***proposal_uri*** : [`URI`](#types-URI)) [`ProposalInfo`](#types-ProposalInfo)
  * ***votes*** :[`BigMap`](#types-BigMap) (***proposal_uri*** : [`URI`](#types-URI), ***voter_address*** : [`Address`](#types-Address)) (***vote_choice*** : [`Natural`](#types-Natural))
  * ***metadata*** :[`BigMap`](#types-BigMap) [`Text`](#types-Text) [`ByteString`](#types-ByteString)

**Final Michelson representation:** `pair (pair (pair address address) (big_map address unit) (pair nat nat) nat nat) (pair (pair address nat) (big_map string (pair (pair nat timestamp) timestamp nat (list string)))) (big_map (pair string address) nat) (big_map string bytes)`



<a name="section-Entrypoints"></a>

## Entrypoints

<a name="entrypoints-admin"></a>

---

### `admin`

**Argument:** 
  + **In Haskell:** [`AdminParameter`](#types-AdminParameter)
  + **In Michelson:** `(or (or address unit) (or (list address) (or (list address) (pair (pair nat nat) nat nat))))`
    + **Example:** <span id="example-id">`Left (Left "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB")`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Admin` constructor.
    + **In Haskell:** `Admin (·)`
    + **In Michelson:** `Left (·)`
1. Call the contract (default entrypoint) with the constructed argument.
</details>
<p>



<a name="section-Entrypoints"></a>

#### Entrypoints

<a name="entrypoints-set_admin"></a>

---

##### `set_admin`

Called by the current `admin` to transfer the role to someone else.
Takes the address of the new `admin` candidate as input.
Note: for security reasons the transfer isn't complete until the new admin
candidate calls `accept_admin`.
If there is already a candidate, another call to this entrypoint will
replace it.
If there is already a candidate, calling this with the `admin` address will
invalidate the current candidate.

**Argument:** 
  + **In Haskell:** [`Address`](#types-Address)
  + **In Michelson:** `address`
    + **Example:** <span id="example-id">`"KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB"`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `set_admin` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotAdmin_`](#errors-SenderIsNotAdmin_) — The sender is not the current admin



<a name="entrypoints-accept_admin"></a>

---

##### `accept_admin`

Called by the current `admin` candidate (see `set_admin`) to complete the transfer of
the role. Takes no input.

**Argument:** 
  + **In Haskell:** [`()`](#types-lparenrparen)
  + **In Michelson:** `unit`
    + **Example:** <span id="example-id">`Unit`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `accept_admin` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotAdminCandidate`](#errors-SenderIsNotAdminCandidate) — The sender is not the current admin candidate



<a name="entrypoints-add_maintainers"></a>

---

##### `add_maintainers`

Takes a list of addresses that will become `maintainer`s as input.
All these addresses receive the `maintainer` role.
For addresses that already have the `maintainer` role and for duplicate addresses in
the input list, this call is a no-op.

**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`Address`](#types-Address)
  + **In Michelson:** `(list address)`
    + **Example:** <span id="example-id">`{ "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `add_maintainers` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotAdmin_`](#errors-SenderIsNotAdmin_) — The sender is not the current admin



<a name="entrypoints-remove_maintainers"></a>

---

##### `remove_maintainers`

Takes a list of addresses that will lose the `maintainer` role as input.
All these addresses lose the `maintainer` role.
For addresses that don't have the `maintainer` role and for duplicate addresses in
the input list, this call is a no-op.

**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`Address`](#types-Address)
  + **In Michelson:** `(list address)`
    + **Example:** <span id="example-id">`{ "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `remove_maintainers` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotAdmin_`](#errors-SenderIsNotAdmin_) — The sender is not the current admin



<a name="entrypoints-configure"></a>

---

##### `configure`

Takes new values for all the **configuration options** as input.
These new values will replace the current ones and will affect future proposals.
The sender must have a `maintainer` role.

**Argument:** 
  + **In Haskell:** [`Configuration`](#types-Configuration)
  + **In Michelson:** `(pair (pair nat nat) nat nat)`
    + **Example:** <span id="example-id">`{ Pair 0 0; 0; 0 }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `configure` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`SenderIsNotMaintainer`](#errors-SenderIsNotMaintainer) — The sender does not have the `maintainer` role





<a name="entrypoints-voting"></a>

---

### `voting`

**Argument:** 
  + **In Haskell:** [`VotingParameter`](#types-VotingParameter)
  + **In Michelson:** `(or (pair string (list string)) (or (pair string nat) (list (pair (pair address nat) nat))))`
    + **Example:** <span id="example-id">`Left (Pair "hello" { "hello" })`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Wrap into `Voting` constructor.
    + **In Haskell:** `Voting (·)`
    + **In Michelson:** `Right (·)`
1. Call the contract (default entrypoint) with the constructed argument.
</details>
<p>



<a name="section-Entrypoints"></a>

#### Entrypoints

<a name="entrypoints-propose"></a>

---

##### `propose`

Called by a `participant` to submit a new proposal.
Takes an IPFS URI and a list of strings for the available choices as input.
A new proposal for the given IPFS URI will be initialized using the current
**configuration options** (see `configure`).

**Argument:** 
  + **In Haskell:** (***proposal_uri*** : [`URI`](#types-URI), ***choices*** : [`List`](#types-List) [`Text`](#types-Text))
  + **In Michelson:** `(pair string (list string))`
    + **Example:** <span id="example-id">`Pair "hello" { "hello" }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `propose` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`EmptyChoices`](#errors-EmptyChoices) — The proposal has no choices, i.e.

* [`DuplicateProposal`](#errors-DuplicateProposal) — A proposal for the same IPFS URI already exists



<a name="entrypoints-vote"></a>

---

##### `vote`

Called by a `participant` to cast a vote on a proposal.
Takes the proposal's IPFS URI and a choice index (zero-based).

**Argument:** 
  + **In Haskell:** (***proposal_uri*** : [`URI`](#types-URI), ***choice_index*** : [`Natural`](#types-Natural))
  + **In Michelson:** `(pair string nat)`
    + **Example:** <span id="example-id">`Pair "hello" 0`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `vote` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`NoSuchProposal`](#errors-NoSuchProposal) — No proposal for the given IPFS URI

* [`NoSuchChoice`](#errors-NoSuchChoice) — No choice with the given index available for the proposal

* [`ProposalNotYetActive`](#errors-ProposalNotYetActive) — The voting period for the proposal hasn't started yet

* [`ProposalExpired`](#errors-ProposalExpired) — The voting period for the proposal has ended

* [`AlreadyVoted`](#errors-AlreadyVoted) — The sender has already voted on this proposal



<a name="entrypoints-verify_min_balance"></a>

---

##### `verify_min_balance`

Callback for the `balance_of` CPS view of FA2,
used to check the governance token's balance of a `participant` that used the
`propose` entrypoint.

**Argument:** 
  + **In Haskell:** [`List`](#types-List) [`BalanceResponseItem`](#types-BalanceResponseItem)
  + **In Michelson:** `(list (pair (pair address nat) nat))`
    + **Example:** <span id="example-id">`{ Pair (Pair "KT1AEseqMV6fk2vtvQCVyA7ZCaxv7cpxtXdB" 0) 0 }`</span>

<details>
  <summary><b>How to call this entrypoint</b></summary>

0. Construct an argument for the entrypoint.
1. Call contract's `verify_min_balance` entrypoint passing the constructed argument.
</details>
<p>



**Possible errors:**
* [`NotEnoughTokens`](#errors-NotEnoughTokens) — Some of the `balance`s in the list are less than `minimum_balance`









# Definitions

<a name="section-Types"></a>

## Types

<a name="types-lparenrparen"></a>

---

### `()`

Unit primitive.

**Structure:** ()

**Final Michelson representation:** `unit`



<a name="types-lparenacomma-brparen"></a>

---

### `(a, b)`

Pair primitive.

**Final Michelson representation (example):** `(Integer,Natural)` = `pair int nat`



<a name="types-Address"></a>

---

### `Address`

Address primitive.

Unlike Michelson's `address`, it is assumed not to contain an entrypoint name,
even if it refers to a contract; this won't be checked, so passing an entrypoint
name may result in unexpected errors.


**Final Michelson representation:** `address`



<a name="types-AdminParameter"></a>

---

### `AdminParameter`

AdminParameter

**Structure:** *one of* 
+ **Set_admin**[`Address`](#types-Address)
+ **Accept_admin**[`()`](#types-lparenrparen)
+ **Add_maintainers**([`List`](#types-List) [`Address`](#types-Address))
+ **Remove_maintainers**([`List`](#types-List) [`Address`](#types-Address))
+ **Configure**[`Configuration`](#types-Configuration)


**Final Michelson representation:** `or (or address unit) (or (list address) (or (list address) (pair (pair nat nat) nat nat)))`



<a name="types-BalanceRequestItem"></a>

---

### `BalanceRequestItem`

Describes a request for an owner's balance

**Structure:** 
  * ***owner*** :[`Address`](#types-Address)
  * ***tokenId*** :[`TokenId`](#types-TokenId)

**Final Michelson representation:** `pair address nat`



<a name="types-BalanceResponseItem"></a>

---

### `BalanceResponseItem`

Describes a response to a request for an owner's balance

**Structure:** 
  * ***request*** :[`BalanceRequestItem`](#types-BalanceRequestItem)
  * ***balance*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair (pair address nat) nat`



<a name="types-BigMap"></a>

---

### `BigMap`

BigMap primitive.

**Final Michelson representation (example):** `BigMap Integer Natural` = `big_map int nat`



<a name="types-ByteString"></a>

---

### `ByteString`

Bytes primitive.

**Final Michelson representation:** `bytes`



<a name="types-Configuration"></a>

---

### `Configuration`

Options defining the behaviour and life-cycle of proposals.

**Structure:** 
  * ***expireTime*** :[`Seconds`](#types-Seconds)
  * ***voteDelay*** :[`Seconds`](#types-Seconds)
  * ***quorumThreshold*** :[`Natural`](#types-Natural)
  * ***minimumBalance*** :[`Natural`](#types-Natural)

**Final Michelson representation:** `pair (pair nat nat) nat nat`



<a name="types-FA2Config"></a>

---

### `FA2Config`

Parameters defining the governance token contract and type

**Structure:** 
  * ***fa2Addr*** :[`Address`](#types-Address)
  * ***fa2TokenId*** :[`TokenId`](#types-TokenId)

**Final Michelson representation:** `pair address nat`



<a name="types-Integer"></a>

---

### `Integer`

Signed number.

**Final Michelson representation:** `int`



<a name="types-List"></a>

---

### `List`

List primitive.

**Final Michelson representation (example):** `[Integer]` = `list int`



<a name="types-Named-entry"></a>

---

### `Named entry`

Some entries have names for clarity.

In resulting Michelson names may be mapped to annotations.

**Final Michelson representation (example):** `number: Integer` = `int`



<a name="types-Natural"></a>

---

### `Natural`

Unsigned number.

**Final Michelson representation:** `nat`



<a name="types-ProposalInfo"></a>

---

### `ProposalInfo`

Information defining a proposal.

**Structure:** 
  * ***level*** :[`Natural`](#types-Natural)
  * ***startsAt*** :[`Timestamp`](#types-Timestamp)
  * ***expiresAt*** :[`Timestamp`](#types-Timestamp)
  * ***quorumThreshold*** :[`Natural`](#types-Natural)
  * ***choices*** :[`List`](#types-List) [`Text`](#types-Text)

**Final Michelson representation:** `pair (pair nat timestamp) timestamp nat (list string)`



<a name="types-Seconds"></a>

---

### `Seconds`

A natural representing a number of seconds.

**Structure:** [`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-Text"></a>

---

### `Text`

Michelson string.

This has to contain only ASCII characters with codes from [32; 126] range; additionally, newline feed character is allowed.

**Final Michelson representation:** `string`



<a name="types-Timestamp"></a>

---

### `Timestamp`

Timestamp primitive.

**Final Michelson representation:** `timestamp`



<a name="types-TokenId"></a>

---

### `TokenId`

Token identifier as defined by [TZIP-12](https://gitlab.com/tzip/tzip/-/blob/1f83a3671cdff3ab4517bfa9ee5a57fc5276d4ff/proposals/tzip-12/tzip-12.md#general).

**Structure:** [`Natural`](#types-Natural)

**Final Michelson representation:** `nat`



<a name="types-URI"></a>

---

### `URI`

Text representing IPFS URI for a proposal.

**Structure:** [`Text`](#types-Text)

**Final Michelson representation:** `string`



<a name="types-VotingParameter"></a>

---

### `VotingParameter`

VotingParameter

**Structure:** *one of* 
+ **Propose**(***proposal_uri*** : [`URI`](#types-URI), ***choices*** : [`List`](#types-List) [`Text`](#types-Text))
+ **Vote**(***proposal_uri*** : [`URI`](#types-URI), ***choice_index*** : [`Natural`](#types-Natural))
+ **Verify_min_balance**([`List`](#types-List) [`BalanceResponseItem`](#types-BalanceResponseItem))


**Final Michelson representation:** `or (pair string (list string)) (or (pair string nat) (list (pair (pair address nat) nat)))`



<a name="section-Errors"></a>

## Errors

Our contract implies the possibility of error scenarios, this section enlists
all values which the contract can produce via calling `FAILWITH` instruction
on them. In case of error, no changes to contract state will be applied.

Each entrypoint also contains a list of errors which can be raised during its
execution; only for no-throw entrypoints this list will be omitted.
Errors in these lists are placed in the order in which the corresponding
properties are checked unless the opposite is specified. I.e., if for a
given entrypoint call two different errors may take place, the one which
appears in the list first will be thrown.

The errors are represented either as a string `error tag` or a pair `(error tag, error argument)`.
See the list of errors below for details.

We distinquish several error classes:
+ **Action exception**: given action cannot be performed with
  regard to the current contract state.

  Examples: "insufficient balance", "wallet does not exist".

  If you are implementing a middleware, such errors should be propagated to
  the client.

+ **Bad argument**: invalid argument supplied to the entrypoint.

  Examples: entrypoint accepts a natural number from `0-3` range, and you
  supply `5`.

  If you are implementing a middleware, you should care about not letting
  such errors happen.

+ **Internal**: contract-internal error.

  In ideal case, such errors should not take place, but still, make sure
  that you are ready to handle them. They can signal either invalid contract
  deployment or a bug in contract implementation.

  If an internal error is thrown, please report it to the author of this contract.


<a name="errors-AlreadyVoted"></a>

---

### `AlreadyVoted`

**Class:** Action exception

**Fires if:** The sender has already voted on this proposal

**Representation:** `AlreadyVoted`

<a name="errors-DuplicateProposal"></a>

---

### `DuplicateProposal`

**Class:** Action exception

**Fires if:** A proposal for the same IPFS URI already exists

**Representation:** `DuplicateProposal`

<a name="errors-EmptyChoices"></a>

---

### `EmptyChoices`

**Class:** Bad argument

**Fires if:** The proposal has no choices, i.e. the provided list of available choices is empty

**Representation:** `EmptyChoices`

<a name="errors-NoFA2Contract"></a>

---

### `NoFA2Contract`

**Class:** Internal

**Fires if:** Configured FA2 contract not found

**Representation:** `NoFA2Contract`

<a name="errors-NoSuchChoice"></a>

---

### `NoSuchChoice`

**Class:** Action exception

**Fires if:** No choice with the given index available for the proposal

**Representation:** `NoSuchChoice`

<a name="errors-NoSuchProposal"></a>

---

### `NoSuchProposal`

**Class:** Action exception

**Fires if:** No proposal for the given IPFS URI

**Representation:** `NoSuchProposal`

<a name="errors-NotEnoughTokens"></a>

---

### `NotEnoughTokens`

**Class:** Action exception

**Fires if:** Some of the `balance`s in the list are less than `minimum_balance`

**Representation:** `NotEnoughTokens`

<a name="errors-ProposalExpired"></a>

---

### `ProposalExpired`

**Class:** Action exception

**Fires if:** The voting period for the proposal has ended

**Representation:** `ProposalExpired`

<a name="errors-ProposalNotYetActive"></a>

---

### `ProposalNotYetActive`

**Class:** Action exception

**Fires if:** The voting period for the proposal hasn't started yet

**Representation:** `ProposalNotYetActive`

<a name="errors-SenderIsNotAdminCandidate"></a>

---

### `SenderIsNotAdminCandidate`

**Class:** Action exception

**Fires if:** The sender is not the current admin candidate

**Representation:** `SenderIsNotAdminCandidate`

<a name="errors-SenderIsNotAdmin_"></a>

---

### `SenderIsNotAdmin_`

**Class:** Action exception

**Fires if:** The sender is not the current admin

**Representation:** `SenderIsNotAdmin_`

<a name="errors-SenderIsNotMaintainer"></a>

---

### `SenderIsNotMaintainer`

**Class:** Action exception

**Fires if:** The sender does not have the `maintainer` role

**Representation:** `SenderIsNotMaintainer`
