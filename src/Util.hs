module Util where

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither b = maybe (Left b) Right

-- | A total read function
safeRead :: Read a => String -> Maybe a
safeRead s
  | [(x, "")] <- reads s = Just x
  | otherwise = Nothing
