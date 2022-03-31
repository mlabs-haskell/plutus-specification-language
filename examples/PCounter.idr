module PCounter

import PSL
import Data.Nat
import Data.DPair

record CounterDatum where
  constructor MkCounterDatum
  counter : Nat
  pkh : PubKeyHash

data CounterProtocolPermissible : ProtocolName CounterDatum -> Type where
  MkCounterProtocolStep : DPair (UTXO self) (\u => LTE 1 u.datum.counter) -> CounterProtocolPermissible self
  MkCounterProtocolConsume : DPair (UTXO self) (\u => u.datum.counter === 0) -> CounterProtocolPermissible self

counterProtocolPermissible : (self : ProtocolName CounterDatum) -> (CounterProtocolPermissible self, TimeRange) -> TxDiagram {d = CounterDatum} self
counterProtocolPermissible self (MkCounterProtocolStep (MkDPair utxo _), validRange) = MkTxDiagram {
  inputs = [ MkTxIn { ref = Nothing, utxo = MkSomeUTXO utxo } ],
  outputs = [ mkOwnTxOut ({datum := {counter := minus utxo.datum.counter 1} utxo.datum} utxo) ],
  validRange = validRange,
  signatures = [],
  mint = NilMap
}
counterProtocolPermissible self (MkCounterProtocolConsume (MkDPair utxo _), validRange) = MkTxDiagram {
  inputs = [ MkTxIn { ref = Nothing, utxo = MkSomeUTXO utxo } ],
  outputs = [],
  validRange = validRange,
  signatures = [ utxo.datum.pkh ],
  mint = NilMap
}

counterProtocol : Protocol
counterProtocol = MkProtocol {
  datumType = CounterDatum,
  permissible' = \self => MkSet _ (counterProtocolPermissible self)
}
