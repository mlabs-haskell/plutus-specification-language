-- NB: This is pseudo-code.
-- This is a simplified (and hence not practical) version of some core Ardana components.
--
-- NB: This example just uses pseudo-PlutusTx, we don't have to.

data TreasuryState = TreasuryState
  { refreshInterval :: POSIXTime
  , validRangeSize :: POSIXTime
  , timestamp :: POSIXTime
  , ownerAuthToken :: AssetClass
  , dUSDPermissionToken :: AssetClass
  }

data TreasuryRedeemer
  = TreasuryRefreshAct
  | TreasuryUpdateAct

treasuryValidatorDef :: TreasuryState -> TreasuryRedeemer -> TxInfo -> Bool
treasuryValidatorDef state TreasuryRefreshAct info =
  let
    inp = (head info.txInfoInputs).txInInfoResolved
    out = head info.txInfoOutputs
  in
  all id
    [ length info.txInfoInputs == 1
    , length info.txInfoOutputs == 1
    , out == inp
    , isZero info.txInfoMint
    , out.timestamp == ivFrom info.txInfoValidRange
    , ivFrom info.txInfoValidRange ≡ ivTo info.txInfoValidRange - state.validRangeSize _
    , old.timestamp - ivTo info.txInfoValidRange >= old.refreshInterval
    ]
treasuryValidatorDef state TreasuryUpdateAct info =
  let
    inp = (head info.txInfoInputs).txInInfoResolved
    out = head info.txInfoOutputs
  in
  all id
    [ out.timestamp == ivFrom info.txInfoValidRange
    , ivFrom info.txInfoValidRange ≡ ivTo info.txInfoValidRange - state.validRangeSize _
    , assetClassValueOf info.txInfoMint state.ownerAuthToken != 0
    ]

treasuryValidator :: Validator
treasuryValidator = $(magicCompile [| treasuryValidatorDef |])

someSpecialUtxo :: TxOutRef

-- No redeemer
treasuryMintingPolicyDef :: () -> TxInfo -> Bool
treasuryMintingPolicyDef () info = any (\x -> x.txInInfoOutRef == someSpecialUtxo) info.txInfoInputs

treasuryMintingPolicy :: MintinyPolicy
treasuryMintingPolicy = $(magicCompile [| treasuryMintingPolicyDef |])

treasuryStateToken :: AssetClass
treasuryStateToken = AssetClass (mintingPolicyHash treasuryMintingPolicy, "")
