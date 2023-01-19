-- |

module HelloEither where

a :: IO (Either Char (Either Char Int))
a = pure (Right $ Left 'a')
