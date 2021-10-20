import           Control.Monad        (void)
import           Ledger               (Address, ScriptContext)
import qualified Ledger.Constraints   as Constraints
-- import for interval/posix related libraries
import           Ledger               hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         (Value)
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Applicative (..))
-- import for String
import           Prelude              (IO, Semigroup (..), Show (..), String)
-- import for printf
import           Text.Printf          (printf)

-- | These are the data script and redeemer types. We are using an integer
--   value for both, but you should define your own types.
newtype MyDatum = MyDatum POSIXTime deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyDatum

newtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyRedeemer

-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: MyDatum -> () -> ScriptContext -> Bool
validateSpend (MyDatum time) _ ctx = traceIfFalse "redemption period hasn't been reached yet" (not $ deadlineReached)
    where
        -- check if the date the datum provides contains the current time provided by the context. If so, that means
        -- the current time is passed the deadline and we would return true
        deadlineReached :: Bool
        deadlineReached = contains (from time) $ (txInfoValidRange $ scriptContextTxInfo ctx)

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

data Starter
instance Scripts.ValidatorTypes Starter where
    type instance RedeemerType Starter = ()
    type instance DatumType Starter = MyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: Scripts.TypedValidator Starter
starterInstance = Scripts.mkTypedValidator @Starter
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @MyDatum @()

-- | The schema of the contract, with two endpoints.
type Schema =
        Endpoint "publish" (POSIXTime, Value)
        .\/ Endpoint "redeem" ()

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [publish, redeem]

-- Fixed value to indicate when the contract can be used
openTime :: POSIXTime
openTime = POSIXTime 634421989000

-- | The "publish" contract endpoint. We can only publish ADA to give up after a certain
-- | time has been passed for the start of the contract
publish :: AsContractError e => Promise () Schema e ()
publish = endpoint @"publish" $ \(time, lockedFunds) -> do
    -- currentTime returns a Contract Monad which we extract the value out with the arrow function
    now   <- currentTime
    logInfo @String $ printf "now %s deadline %s" (show $ now) (show $ openTime)
    if now < openTime
        then logInfo @String "The script isn't open yet"
    else do
        logInfo @String "Making the transaction"
        let tx = Constraints.mustPayToTheScript (MyDatum time) lockedFunds
        void $ submitTxConstraints starterInstance tx

-- | The "redeem" contract endpoint.
redeem :: AsContractError e => Promise () Schema e ()
redeem = endpoint @"redeem" $ \_ -> do
    now   <- currentTime
    unspentOutputs <- utxosAt contractAddress
    -- Create a transaction to consume all outputs and also provide current time to make sure we collect
    -- before the redemption period ends
    let tx       = collectFromScript unspentOutputs () Prelude.<> Constraints.mustValidateIn (from 1534421999000)
    void $ submitTxConstraintsSpending starterInstance unspentOutputs tx

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
