{--
  Simple monad for error handling
--}

module Possibly
    (
     Possibly(Good, Error)
    ) where

data Possibly a = Good a
                | Error String
                  deriving (Show)

instance Monad Possibly where
    (Good val)  >>= f   = f val
    (Error val) >>= f   = Error val
    return val          = Good val