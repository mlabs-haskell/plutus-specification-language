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
  inputs = fromList [ MkTxIn { ref = Nothing, utxo = MkSomeUTXO utxo } ],
  ownOutputs = [ {datum := {counter := minus utxo.datum.counter 1} utxo.datum} utxo ],
  otherOutputs = fromList [],
  validRange = validRange,
  signatures = fromList [],
  mint = Zero
}
counterProtocolPermissible self (MkCounterProtocolConsume (MkDPair utxo _), validRange) = MkTxDiagram {
  inputs = fromList [ MkTxIn { ref = Nothing, utxo = MkSomeUTXO utxo } ],
  ownOutputs = [],
  otherOutputs = fromList [],
  validRange = validRange,
  signatures = fromList [ utxo.datum.pkh ],
  mint = Zero
}

counterProtocol : Protocol
counterProtocol = MkProtocol {
  datumType = CounterDatum,
  permissible' = \self => MkSet _ (counterProtocolPermissible self)
}
