
import DUSD

data VaultState = VaultState
  { borrowPrincipal :: Natural
  , interest :: Natural
  , interestTimestamp :: POSIXTime
  , userAuthToken :: AssetClass
  , collateralCurrency :: AssetClass
  }

data VaultRedeemer = VaultAddCollateralAct

vaultValidatorDef :: VaultState -> VaultRedeemer -> TxInfo -> Bool
vaultValidatorDef state VaultAddCollateralAct info =
  let
    -- Might fail
    ownOutput = getOwnOutput info
    ownInput = getOwnInput info
    stateHash = (getOwnInput info).txInInfoResolved.txOutDatumHash
  in
  all id
    [ ownOutput == ownInput
    -- Fatal mistake!
    -- , assetClassValueOf txInfoMint dUSD == 0
    ]

vaultValidator :: Validator
vaultValidator = $(magicCompile [| vaultValidatorDef |])

-- No redeemer
vaultMintingPolicyDef :: () -> TxInfo -> Bool
vaultMintingPolicyDef () info =
  let
    [ownOutput] = filter (\x -> x.txOutAddress == scriptHashAddress vaultValidatorHash)
  in
  any (\x -> x.txInInfoOutRef == someSpecialUtxo) info.txInfoInputs

vaultMintingPolicy :: MintinyPolicy
vaultMintingPolicy = $(magicCompile [| vaultMintingPolicyDef |])

vaultStateToken :: AssetClass
vaultStateToken = AssetClass (mintingPolicyHash vaultMintingPolicy, "")
