
import Vault

v1MintingPolicyDef :: ()-> TxInfo -> Bool
v1MintingPolicyDef () info = assetClassValueOf info.txInfoMint vaultStateToken > 0

v1MintingPolicy :: MintinyPolicy
v1MintingPolicy = $(magicCompile [| v1MintingPolicyDef |])
