
import Treasury

certMPHash :: ByteString

tokenPresent :: AssetClass -> TxInfo -> Bool
tokenPresent token info =
  assetClassValueOf info.txInfoMint token != 0
  || any (\x -> assetClassValueOf x.txInInfoResolved.txOutValue token != 0) info.txInfoInputs

dUSDMintingPolicyDef :: TokenName -> TxInfo -> Bool
dUSDMintingPolicyDef treasuryStateHash info =
  let
    treasuryState = fromJust $ Map.lookup (coerce treasuryStateHash) info.txInfoData
  in
  assetClassValueOf info.txInfoMint (AssetClass (ownCurrency, "")) < 0
  || all id
    [ tokenPresent (AssetClass (certMPHash, coerce treasuryStateHash)) info
    , tokenPresent treasuryState.dUSDPermissionToken info
    ]

dUSDMintingPolicy :: MintinyPolicy
dUSDMintingPolicy = $(magicCompile [| dUSDMintingPolicyDef |])

dUSD :: AssetClass
dUSD = AssetClass (mintingPolicyHash dUSDMintingPolicy, "")
