 -- This is a starter contract, based on the Game contract,
-- containing the bare minimum required scaffolding.
--
-- What you should change to something more suitable for
-- your use case:
--   * The MyDatum type
--   * The MyRedeemer type
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * publish
--   * redeem

import           Control.Monad        (void)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         (Value)
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Applicative (..))
import qualified Ledger.Ada            as Ada
import qualified Data.Map              as Map
import qualified Prelude               as Haskell
import           Ledger               

-- | These are the data script and redeemer types. We are using an integer
--   value for both, but you should define your own types.
newtype MyDatum = MyDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyDatum

newtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyRedeemer

-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: () -> () -> ScriptContext -> Bool
validateSpend _ _ _ = True -- Please provide an implementation.

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

contractValidator :: Validator
contractValidator = Scripts.validatorScript starterInstance

data Starter
instance Scripts.ValidatorTypes Starter where
    type instance RedeemerType Starter = ()
    type instance DatumType Starter = ()

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: Scripts.TypedValidator Starter
starterInstance = Scripts.mkTypedValidator @Starter
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @() @()

-- | The schema of the contract, with two endpoints.
type Schema =
        Endpoint "publish" ()
        .\/ Endpoint "redeem" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [publish, redeem]

-- | The "publish" contract endpoint.
publish :: AsContractError e => Promise () Schema e ()
publish = endpoint @"publish" $ \() -> do
    let tx         = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 10000000)
    void (submitTxConstraints starterInstance tx)    

-- | The "redeem" contract endpoint.
redeem :: AsContractError e => Promise () Schema e ()
redeem = endpoint @"redeem" $ \() -> do
    utxos <- fundsAtAddressGeq contractAddress (Ada.lovelaceValueOf 1)
    let tx       = collectFromScript utxos ()
        firstUtxo    = head (Map.toList utxos)
        customUtxo = (Map.fromList [(fst firstUtxo, snd firstUtxo)])
        myUtxo   = Constraints.otherScript (c) Haskell.<> 
                   Constraints.unspentOutputs customUtxo
        myTx     = Constraints.mustSpendScriptOutput (fst firstUtxo) (Redeemer $ PlutusTx.toBuiltinData ())
    logInfo (Haskell.show utxos)
    void (submitTxConstraintsWith @Starter myUtxo myTx)

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
