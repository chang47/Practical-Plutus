-- A stock guessing game with two players. Player 1 locks up the ADA
-- they want to bet and we also send the current stock prize.
-- Player 2 guesses if the stock will be higher or lower at a specific time.
-- If the guess is correct, the validator script releases the funds to player 2.
-- If it isn't, the funds stay locked.
import           Control.Monad         (void)
import qualified Data.ByteString.Char8 as C
import           Data.Map              (Map)
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import           Ledger                (Address, Datum (Datum), ScriptContext, Validator, Value)
import qualified Ledger
import qualified Ledger.Ada            as Ada
import qualified Ledger.Constraints    as Constraints
import           Ledger.Tx             (ChainIndexTxOut (..))
import qualified Ledger.Typed.Scripts  as Scripts
import           Playground.Contract
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (pure, (<$>))
import qualified Prelude               as Haskell

------------------------------------------------------------

newtype LockDatum = LockDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''LockDatum

-- false is lower, true is higher
newtype GuessRedeemer = GuessRedeemer Bool deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''GuessRedeemer

-- | Parameters for the "lock" endpoint
data LockParams = LockParams
    { stockPrice :: Integer
    , amount     :: Value
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

--  | Parameters for the "guess" endpoint
--  | higher: false is lower, true is higher
data GuessParams = GuessParams
    { higher :: Bool
    }
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

type GameSchema =
    Endpoint "lock" LockParams
    .\/ Endpoint "guess" GuessParams

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = GuessRedeemer
    type instance DatumType Game = LockDatum

gameInstance :: Scripts.TypedValidator Game
gameInstance = Scripts.mkTypedValidator @Game
    $$(PlutusTx.compile [|| validateGuess ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @LockDatum @GuessRedeemer

currentStockValue :: Integer
currentStockValue = 1000

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
validateGuess :: LockDatum -> GuessRedeemer -> ScriptContext -> Bool
validateGuess (LockDatum stockPrice) (GuessRedeemer guess) _ = (stockPrice >= currentStockValue) == guess

-- | The validator script of the game.
gameValidator :: Validator
gameValidator = Scripts.validatorScript gameInstance

-- | The address of the game (the hash of its validator script)
gameAddress :: Address
gameAddress = Ledger.scriptAddress gameValidator

-- | The "lock" contract endpoint for starting the bet
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" @LockParams $ \(LockParams stockPrice amt) -> do
    let tx         = Constraints.mustPayToTheScript (LockDatum stockPrice) amt
    void (submitTxConstraints gameInstance tx)    

-- | The "guess" contract endpoint for guessing if a stock price is higher
guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" @GuessParams $ \(GuessParams higher) -> do
    utxos <- fundsAtAddressGeq gameAddress (Ada.lovelaceValueOf 1)
    let redeemer = GuessRedeemer higher
        tx       = collectFromScript utxos redeemer
    void (submitTxConstraintsSpending gameInstance utxos tx)

game :: AsContractError e => Contract () GameSchema e ()
game = selectList [lock, guess]

endpoints :: AsContractError e => Contract () GameSchema e ()
endpoints = game

mkSchemaDefinitions ''GameSchema

$(mkKnownCurrencies [])
