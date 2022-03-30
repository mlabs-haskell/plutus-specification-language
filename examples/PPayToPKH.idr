module PPayToPKH

import PSL
import Data.Nat

pPayToPKHPermissible : (self : ProtocolName) -> (PubKeyHash, TimeRange) -> TxDiagram self
pPayToPKHPermissible self (pkh, validRange) = MkTxDiagram {
  inputs = Nil,
  outputs = Nil,
  mint = NilMap,
  signatures = [pkh],
  validRange = validRange
}

pPayToPKH : Protocol
pPayToPKH = MkProtocol {
  datumType = PubKeyHash,
  permissible' = \self => MkSet (the Type (PubKeyHash, TimeRange)) (pPayToPKHPermissible self)
}
