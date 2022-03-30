module PCounter

import PSL
import Data.Nat

record CounterDatum where
  constructor MkCounterDatum
  counter : Nat
  pkh : PubKeyHash

data CounterProtocolPermissible : ProtocolName -> Type where
  MkCounterProtocolStep : {0 self : ProtocolName} -> {0 p : PSL.datumType self === CounterDatum} -> DPair (UTXOFor self) (\u => LTE 1 (the (CounterDatum) $ replace {p = \x => x} p u.datum).counter) -> CounterProtocolPermissible self
  MkCounterProtocolConsume : {0 p : PSL.datumType self === CounterDatum} -> DPair (UTXOFor self) (\u => (replace {p = \x => x} p u.datum).counter === 0) -> CounterProtocolPermissible self

counterProtocolPermissible : (self : ProtocolName) -> (datumType self === CounterDatum) -> (CounterProtocolPermissible self, TimeRange) -> TxDiagram self
counterProtocolPermissible self Refl (MkCounterProtocolStep (MkDPair utxo _), validRange) = MkTxDiagram {
  inputs = [ MkTxIn { ref = Nothing, utxo = MkDPair self utxo } ],
  outputs = [ MkTxOut { unique = (), utxo = MkDPair self ({ datum.counter := utxo.datum.counter - 1 } utxo) } ],
  validRange = validRange,
  signatures = [],
  mint = NilMap
}

{-

counterProtocol : Protocol
counterProtocol = MkProtocol {
  datumType = CounterDatum
  permissibleType = CounterProtocolPermissible
  permissible = counterProtocolPermissible
}

counterProtocolSoundness : ProtocolSoundness PSL.counterProtocol
counterProtocolSoundness = MkProtocolSoundness {
  reducibleType = \_ => (),
  disjointnessProof = ?osff,
  coverageProof = ?noideaatallwrtwhatthisshouldbehelpme,
  reducibleProof = ?
}

-}
