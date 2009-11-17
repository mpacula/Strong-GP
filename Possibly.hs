{--
  Simple monad for error handling
--}

module GP.Possibly
    (
      Possibly(Good, Error)
    , possibly
    ) where

data Possibly a = Good a
                | Error String
                  deriving (Show)

possibly :: (a -> b) -> (String -> b) -> Possibly a -> b
possibly good _     (Good x)    = good x
possibly _    error (Error msg) = error msg

instance Monad Possibly where
    (Good val)  >>= f   = f val
    (Error val) >>= f   = Error val
    return val          = Good val
