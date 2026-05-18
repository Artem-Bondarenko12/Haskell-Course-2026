-- Functor, Applicative, and Monad instances
newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader g) =
        Reader $ \r -> f (g r)

instance Applicative (Reader r) where
    pure x =
        Reader $ \_ -> x

    Reader f <*> Reader g =
        Reader $ \r -> f r (g r)

instance Monad (Reader r) where
    Reader g >>= f =
        Reader $ \r -> runReader (f (g r)) r

-- Primitive operations
ask :: Reader r r
ask =
    Reader $ \env -> env

asks :: (r -> a) -> Reader r a
asks f =
    Reader $ \env -> f env

local :: (r -> r) -> Reader r a -> Reader r a
local changeEnv reader =
    Reader $ \env ->
        runReader reader (changeEnv env)

-- A practical example — banking system
data BankConfig = BankConfig
    { interestRate :: Double
    , transactionFee :: Int
    , minimumBalance :: Int
    } deriving (Show)

data Account = Account
    { accountId :: String
    , balance :: Int
    } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest account = do
    rate <- asks interestRate
    return (round (fromIntegral (balance account) * rate))

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee account = do
    fee <- asks transactionFee
    return account { balance = balance account - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance account = do
    limit <- asks minimumBalance
    return (balance account >= limit)

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount account = do
    updatedAccount <- applyTransactionFee account
    interest <- calculateInterest account
    isValid <- checkMinimumBalance account
    return (updatedAccount, interest, isValid)
main :: IO ()
main = do
    print "Reader primitives compiled"

    let cfg = BankConfig { interestRate = 0.05, transactionFee = 2, minimumBalance = 100 }
    let acc = Account { accountId = "A-001", balance = 1000 }

    print (runReader (processAccount acc) cfg)