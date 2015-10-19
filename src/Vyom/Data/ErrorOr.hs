module Vyom.Data.ErrorOr where

-- Handle String Errors
type Error = String
type ErrorOr a = Either Error a

