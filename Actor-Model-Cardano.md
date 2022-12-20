Author: Las Safin
Date: 2022-12-12

# [DRAFT] Actor ledger model / Actor model on Cardano

## Problem

Cardano makes use of the UTXO ledger model. It has allowed several critical features of Cardano,
e.g. trivial sharding (through Ouroboros Leios) and determinism (you get only what you wanted,
otherwise nothing happens and you pay nothing).

However, there is a critical problem: _Writing_ protocols for Cardano truly necessitates a tremendous
effort. In addition, to date, seemingly _no active protocol_ does interoperability correctly.

What does interoperability mean? It means, anywhere where a user can interact with a protocol,
another protocol should be able to take the place of the user.
This fundamental property is violated by most protocols on Cardano, and heavily limits
more advanced use cases (such as funds controlled by a vote by thousands of stake holders).

How do we solve this? It helps to try to first identify _what_ a protocol on Cardano is.
Essentially, it is a system whereby you (some user) can lock your funds, which the protocol
can then use in some arbitrary way. The protocol can receive funds, send funds, and evolve
its internal state given certain constraints.

The current version of Plutus Specification Language attempted to solve this by having a restricted notion
of protocol, which is defined by how it consumes and produces UTXOs under certain constraints.
This proved to be insufficient to support true interoperability, and for the cases it supported,
it relied on being able to run many scripts in a single transaction, which is fundamentally not true
due to script limits.

To support true interoperability, it is a _must_ that it should work under the constraint that
the scripts included in a transaction are heavily restricted, i.e. only scripts from a single
protocol and generic helper scripts are allowed. Scripts from seperate protocols running
in the same transaction should never be a necessity, as it can never be proven that
two arbitrary protocols can fit in the same transaction.

To solve this issue, a protocol _standard_ (like ERC-20, but not for tokens) for Cardano is proposed.
A protocol is now modelled similarly to IOG's original idea of _state machines_, but with the following
crucial difference: Rather than _constraints_, _messages_ are used, i.e. state machines
can pass and receive messages from other state machines. The messages can carry _value_.

This simple idea can represent most protocols on Cardano, and natively allows protocol
interoperability. Implemented naively, it is _less efficient_, but using optimisations
(wrt. how they're represented on-chain), they can get quite close to the uninteroperable
protocols currently used.

A fundamental remaining issue is throughput:
This should be no different from before, as what was a UTXO before is now a single state machine.
However, it should be noted that integration with solutions like Seath can be streamlined in combination
with this.

## Abstract Definition

Free from the constraints of Cardano (or any other specific ledger model), we define
a _monetary distributed Actor model_.

Each _protocol_ defines the behaviour of its _instances_.
(The protocol is analogous to a class. The instance to its object.)
Instances have _globally unique identifiers_. An instance can hold a certain number of (asset) classes
of tokens, and in addition holds an infinite amount of a token, the asset class of which
is unique to the instance.
An instance holds state which evolves with each step.
Instances can receive/send messages from/to other instances in a step.
An instance can be created arbitrarily with a specific state defined by its protocol.
An instance can terminate itself.
Instances are inherently nondeterministic, and can choose what to do in a step.

This mostly mirrors traditional Actor Model programming.
You can additionally have OOP-like _interfaces_ which represent what messages
an instance accepts, however, this is informal, as there is no need to have
an explicit representation of interfaces.
Inheritance seemingly doesn't make much sense, albeit it's arguably that
inheritance is a bad idea in general.
Seemingly, you can always use interfaces in place of inheritance.
Inheritance can then be an informal concept like interfaces.

Semi-formalisation in Haskell:
```haskell
type Address :: Type -> Type

type AssetClass :: Type

data Act rmsg
  = Terminate
  -- ^ All remaining funds are released
  | forall smsg. Send (Address smsg) [(AssetClass, Natural)] smsg
  -- ^ Send a message to the specified address along with some funds
  | forall smsg. Receive (Address smsg) [(AssetClass, Natural)] rmsg
  -- ^ Receive a message from the specified address along with some funds
  | Take AssetClass Integer
  -- ^ Take funds (it doesn't matter where they come from / go to).
  -- Can be negative to signal dropping.

data Context p = Context
  { self :: Address (RMsg p)
  , ownAssetClass :: TokenType p -> AssetClass
  , balances :: [(AssetClass, Integer)]
  , state :: State p
  , redeemer :: Redeemer p
  }
  
class Protocol (p :: Type) where
  data Redeemer p :: Type
  data State p :: Type
  data RMsg p :: Type
  data TokenType p :: Type
  init :: State p
  step :: Context p -> (state, [Act (RMsg p)])


data Protocol redeemer state rmsg = Protocol
  { init :: state
  , step :: Context redeemer state rmsg -> (state, [Act rmsg])
  }

data SomeProtocol = forall redeemer state rmsg. SomeProtocol (Protocol redeemer state rmsg)

adaClass :: AssetClass
```

For example, a simplified Uniswap-style DEX can be modelled as such:
```haskell
data DexRedeemer
  = DexChooseTradingPair AssetClass AssetClass
  | DexAddLiquidity Natural Natural
  | DexRemoveLiquidity Natural Natural
  | DexTrade Natural Natural
data DexState = DexInit | DexTrading AssetClass AssetClass
data DexMsg = DexMsg

dexStep :: Context DexRedeemer DexState DexMsg -> (DexState, [Act rmsg])
dexStep c = case (c.state, c.redeemer) of
  (DexInit, DexChooseTradingPair t_x t_y) -> (DexTrading t_x t_y, [])
  (state@(DexTrading t_x t_y), DexAddLiquidity d_t_x d_t_y) -> (state, [Take t_x d_t_x, Take t_y d_t_y, Take ownAssetClass ])
  ()

-- Must not trade with ADA for simplicity
dex :: Protocol DexRedeemer DexState DexMsg
dex = Protocol
  { init = DexInit
  , step = dexStep
  }
```
