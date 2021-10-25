
import Vault
import Treasury
import DUSD
import V1

alwaysValidates :: Validator -> Property
alwaysValidates validator =
  Property
    [ HasInput
        Just validator
        Nothing
        Nothing
        Nothing
    ]
    $(magicCompile [| f |])
  where
    f :: TxInfo -> Bool -> Bool
    f info _ = checkValidator validator info -- Will fail if there is no input locked with this validator

alwaysValidates' :: Validator -> Property
alwaysValidates' validator =
  Property
    []
    $(magicCompile [| f |])
  where
    f :: TxInfo -> Bool -> Bool
    f info _ =
      -- implication
      not (any (\x -> x.txInInfoResolved.txOutAddress == scriptHashAddress validator) info.txInfoInputs)
      || checkValidator validator info

alwaysFails :: Validator -> Property
alwaysFails validator =
  Property
    [ HasInput
        Just validator
        Nothing
        Nothing
        Nothing
    ]
    $(magicCompile [| f |])
  where
    f :: TxInfo -> Bool -> Bool
    f _ validates = not validates

alwaysFails' :: Validator -> Property
alwaysFails' validator =
  Property
    []
    $(magicCompile [| f |])
  where
    f :: TxInfo -> Bool -> Bool
    f info validates =
      -- implication
      not (any (\x -> x.txInInfoResolved.txOutAddress == scriptHashAddress validator) info.txInfoInputs)
      || not validates

sanityCheck :: Validator -> Check
sanityCheck validator = shouldFail (alwaysValidates validator) <> shouldFail (alwaysFails validator)

can_only_mint_up_to_collateral_ratio :: Property
can_only_mint_up_to_collateral_ratio =
  Property
    [ HasInput
        (Just vaultValidator)
        Nothing
        (Just VaultAddCollateralAct)
        Nothing
    , HasNInputs 1
    , Mints (Just dUSD) Nothing
    ]
    $(magicCompile [| f |])
  where
    f :: TxInfo -> Bool -> Bool
    f info validates = fill out

