
# On-chain chess

We present a simple on-chain game of chess, that allows you to lock funds
that go to the winner.

There are two scripts in play:
- A validator script that controls the UTXO containing the instance token.
- A minting policy script that controls minting the instance token and color tokens.

The scripts will be parameterised by a specific `TxOutRef`.
The token names are all 1 byte long, and are either:
0) instance token
1) black token
2) white token

To mint these tokens, the following conditions must be upheld:
- There must be exactly one instance token minted, locked with the validator script
  with a correct initial datum.
- The specified `TxOutRef` must be consumed.

The datum for the instance UTXO will contain a representation of
the chess board along with information about who will play next.
