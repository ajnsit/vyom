{-# LANGUAGE RankNTypes, GADTs, TypeOperators #-}
module Vyom.Data.Typ
( Typ(..), typToExp, expToTyp
, Dynamic(..), asInt, asBool, asString, asUnit
, TypQ, unTypQ, tunit, tint, tbool, tstring, tarr, (~~>), ttuple, tlist
, eqT, cast, gcast, AsArrow(..), AsTuple(..), AsList(..)
, eqTrans1, eqTrans2, eqCast, eqCast2, eqCast3
, module Data.Type.Equality
) where

-- This module is a bit of a ragtag collection of typing stuff
-- TODO: Break this module into chunky bits

import Data.Type.Equality
import Vyom.Data.Exp
import Vyom.Data.ErrorOr

import Data.Binary

class TSym r where
  -- Our specific supported datatypes
  -- These *should* match those in various *Sym.hs
  ttint :: r Int
  ttbool :: r Bool
  ttstring :: r String
  ttunit :: r ()
  ttarr :: r a -> r b -> r (a -> b)
  tttuple :: r a -> r b -> r (a,b)
  ttlist :: r a -> r [a]

-- Abstract Type, constructor is not exported
newtype TypQ a = TypQ { unTypQ :: forall r . TSym r => r a }

instance TSym TypQ where
  ttunit = TypQ ttunit
  ttint = TypQ ttint
  ttbool = TypQ ttbool
  ttstring = TypQ ttstring
  ttarr a b = TypQ $ ttarr (unTypQ a) (unTypQ b)
  tttuple a b = TypQ $ tttuple (unTypQ a) (unTypQ b)
  ttlist a = TypQ $ ttlist (unTypQ a)

-- Specialised TypQ construction methods
tunit :: TypQ ()
tunit = ttunit
tint :: TypQ Int
tint = ttint
tbool :: TypQ Bool
tbool = ttbool
tstring :: TypQ String
tstring = ttstring
tarr, (~~>) :: TypQ a -> TypQ b -> TypQ (a->b)
tarr = ttarr
(~~>) = ttarr
ttuple :: TypQ a -> TypQ b -> TypQ (a,b)
ttuple = tttuple
tlist :: TypQ a -> TypQ [a]
tlist = ttlist

-- Analogous to Data.Typeable.TypeRep
-- NOTE: Figure out why:
--   Can't put forall inside the constructor (gives errors elsewhere)
--   Hence, also cannot make this a newtype
--   Because of the forall, we also can't use record syntax
data Typ = forall t. Typ (TypQ t)

-- A generalised Data.Dynamic
data Dynamic t = forall a. Dynamic (TypQ a) (t a)

-- Private
newtype AsInt a = AsInt (Maybe (a :~: Int))
newtype AsBool a = AsBool (Maybe (a :~: Bool))
newtype AsString a = AsString (Maybe (a :~: String))
newtype AsUnit a = AsUnit (Maybe (a :~: ()))

-- Don't want to export it, but have to
data AsArrow a = forall b1 b2. AsArrow (TypQ a) (Maybe (TypQ b1, TypQ b2, a :~: (b1->b2)))
data AsTuple a = forall b1 b2. AsTuple (TypQ a) (Maybe (TypQ b1, TypQ b2, a :~: (b1,b2)))
data AsList a = forall b. AsList (TypQ a) (Maybe (TypQ b, a :~: [b]))

-- Convert a dynamic to a () if possible
asUnit :: Dynamic t -> Maybe (t ())
asUnit (Dynamic (TypQ (AsUnit Nothing)) _) = Nothing
asUnit (Dynamic (TypQ (AsUnit (Just Refl))) c) = Just c

-- Convert a dynamic to an Int if possible
-- By matching Refl here, we prove that a :~: Int, hence it compiles
asInt :: Dynamic t -> Maybe (t Int)
asInt (Dynamic (TypQ (AsInt Nothing)) _) = Nothing
asInt (Dynamic (TypQ (AsInt (Just Refl))) c) = Just c

-- Convert a dynamic to a Bool if possible
asBool :: Dynamic t -> Maybe (t Bool)
asBool (Dynamic (TypQ (AsBool Nothing)) _) = Nothing
asBool (Dynamic (TypQ (AsBool (Just Refl))) c) = Just c

-- Convert a dynamic to a String if possible
asString :: Dynamic t -> Maybe (t String)
asString (Dynamic (TypQ (AsString Nothing)) _) = Nothing
asString (Dynamic (TypQ (AsString (Just Refl))) c) = Just c

instance TSym AsUnit where
  ttunit = AsUnit $ Just Refl
  ttint = AsUnit Nothing
  ttbool = AsUnit Nothing
  ttstring = AsUnit Nothing
  ttarr _ _ = AsUnit Nothing
  tttuple _ _ = AsUnit Nothing
  ttlist _ = AsUnit Nothing

instance TSym AsInt where
  ttunit = AsInt Nothing
  ttint = AsInt $ Just Refl
  ttbool = AsInt Nothing
  ttstring = AsInt Nothing
  ttarr _ _ = AsInt Nothing
  tttuple _ _ = AsInt Nothing
  ttlist _ = AsInt Nothing

instance TSym AsBool where
  ttunit = AsBool Nothing
  ttint = AsBool Nothing
  ttbool = AsBool $ Just Refl
  ttstring = AsBool Nothing
  ttarr _ _ = AsBool Nothing
  tttuple _ _ = AsBool Nothing
  ttlist _ = AsBool Nothing

instance TSym AsString where
  ttunit = AsString Nothing
  ttint = AsString Nothing
  ttbool = AsString Nothing
  ttstring = AsString $ Just Refl
  ttarr _ _ = AsString Nothing
  tttuple _ _ = AsString Nothing
  ttlist _ = AsString Nothing

instance TSym AsArrow where
  ttunit = AsArrow ttunit Nothing
  ttint = AsArrow ttint Nothing
  ttbool = AsArrow ttbool Nothing
  ttstring = AsArrow ttstring Nothing
  ttarr (AsArrow t1 _) (AsArrow t2 _) = AsArrow (tarr t1 t2) $ Just (t1,t2,Refl)
  tttuple (AsArrow t1 _) (AsArrow t2 _) = AsArrow (tttuple t1 t2) Nothing
  ttlist (AsArrow t _) = AsArrow (ttlist t) Nothing

instance TSym AsTuple where
  ttunit = AsTuple ttunit Nothing
  ttint = AsTuple ttint Nothing
  ttbool = AsTuple ttbool Nothing
  ttstring = AsTuple ttstring Nothing
  ttarr (AsTuple t1 _) (AsTuple t2 _) = AsTuple (tarr t1 t2) Nothing
  tttuple (AsTuple t1 _) (AsTuple t2 _) = AsTuple (tttuple t1 t2) $ Just (t1,t2,Refl)
  ttlist (AsTuple t _) = AsTuple (ttlist t) Nothing

instance TSym AsList where
  ttunit = AsList ttunit Nothing
  ttint = AsList ttint Nothing
  ttbool = AsList ttbool Nothing
  ttstring = AsList ttstring Nothing
  ttarr (AsList t1 _) (AsList t2 _) = AsList (tarr t1 t2) Nothing
  tttuple (AsList t1 _) (AsList t2 _) = AsList (tttuple t1 t2) Nothing
  ttlist (AsList t _) = AsList (ttlist t) $ Just (t, Refl)

-- Type checking using TypQs
-- SafeCast a allows you to potentially extract a witness of type equality with a
newtype SafeCast a = SafeCast (forall b. TypQ b -> Maybe (a :~: b))
instance TSym SafeCast where
    ttunit = SafeCast $ \(TypQ (AsUnit castf)) -> _runCast0 castf
    ttint = SafeCast $ \(TypQ (AsInt castf)) -> _runCast0 castf
    ttbool = SafeCast $ \(TypQ (AsBool castf)) -> _runCast0 castf
    ttstring = SafeCast $ \(TypQ (AsString castf)) -> _runCast0 castf
    ttlist (SafeCast t) =
      SafeCast $ \(TypQ (AsList _ castf)) -> _runCast1 castf t
    ttarr (SafeCast t1) (SafeCast t2) =
      SafeCast $ \(TypQ (AsArrow _ castf)) -> _runCast2 castf t1 t2
    tttuple (SafeCast t1) (SafeCast t2) =
      SafeCast $ \(TypQ (AsTuple _ castf)) -> _runCast2 castf t1 t2

_runCast0
  :: Functor m
  => m (a :~: b)
  -> m (b :~: a)
_runCast0 castf = sym <$> castf

_runCast1
  :: Monad m
  => m (t, c :~: t1 b)
  -> (t -> m (a :~: b))
  -> m (t1 a :~: c)
_runCast1 castf t = do
  (b, eqBb) <- castf
  eqTb <- t b
  return $ trans (eqTrans1 eqTb) (sym eqBb)

_runCast2
  :: Monad m
  => m (t1, t2, e :~: t3 b d)
  -> (t1 -> m (a :~: b))
  -> (t2 -> m (c :~: d))
  -> m (t3 a c :~: e)
_runCast2 castf t1 t2 = do
  (b1,b2,eqBb1b2) <- castf
  eqT1b1 <- t1 b1
  eqT2b2 <- t2 b2
  return $ trans (eqTrans2 eqT1b1 eqT2b2) (sym eqBb1b2)


-------------
-- UTILITY --
-------------

-- Casting equal constructs
eqCast :: a :~: b -> t a -> t b
eqCast Refl a = a
eqCast2 :: (a :~: c) -> (b :~: d) -> t a b -> t c d
eqCast2 Refl Refl a = a
eqCast3 :: (a :~: d) -> (b :~: e) -> (c :~: f) -> t a b c -> t d e f
eqCast3 Refl Refl Refl a = a

-- Prove equality over arbitrary "transformers"
eqTrans1 :: (a :~: b) -> forall t. t a :~: t b
eqTrans1 Refl = Refl
eqTrans2 :: (a :~: b) -> (c :~: d) -> forall t. t a c :~: t b d
eqTrans2 Refl Refl = Refl

------------------------------------
-- Data.Typeable Interface
------------------------------------

-- Extract a witness of type equality
eqT :: forall a b. TypQ a -> TypQ b -> Maybe (a :~: b)
eqT (TypQ (SafeCast f)) = f

-- Safe cast
cast :: TypQ a -> TypQ b -> a -> Maybe b
cast ta tb a = gen a <$> eqT ta tb
  where
    gen :: a -> a :~: b -> b
    gen c Refl = c

-- Safe construct cast
gcast :: TypQ a -> TypQ b -> c a -> Maybe (c b)
gcast ta tb ca = gen ca <$> eqT ta tb
 where
   gen :: c a -> a :~: b -> c b
   gen c Refl = c

-- TODO: Why do gcast1 and gcast2 give me kind errors?
-- gcast1 :: TypQ t -> TypQ t' -> c (t a) -> Maybe (c (t' a))
-- gcast1 ta tb ct = gen ct <$> eqT ta tb
--   where
--     gen :: c (t a) -> t :~: t' -> c (t' a)
--     gen c Refl = c

-- gcast2 :: TypQ t -> TypQ t' -> c (t a b) -> Maybe (c (t' a b))
-- gcast2 ta tb ct = gen ct <$> eqT ta tb
--   where
--     gen c Refl = c

-------------------------------------------------------------------------
-- SERIALISATION

-- HACK: To compare Typ, compare serial representations
instance Eq Typ where
  Typ (TypQ (ShowT s1)) == Typ (TypQ (ShowT s2)) = s1 == s2

-- Use conversion to/from ExprU to implement Binary instance
instance Binary Typ where
  put = put . typToExp
  get = do
    e <- get
    case expToTyp e of
      Left s -> error s
      Right t -> return t

-- Show instance for TypQ
newtype ShowT a = ShowT String
instance TSym ShowT where
  ttunit = ShowT "()"
  ttbool = ShowT "Bool"
  ttstring = ShowT "String"
  ttint = ShowT "Int"
  ttarr (ShowT a) (ShowT b) = ShowT $ "(" ++ a ++ " -> " ++ b ++ ")"
  tttuple (ShowT a) (ShowT b) = ShowT $ "(" ++ a ++ ", " ++ b ++ ")"
  ttlist (ShowT a) = ShowT $ "[" ++ a ++ "]"

instance Show (TypQ a) where
  show (TypQ (ShowT s)) = s

typToExp :: Typ -> ExprU
typToExp (Typ t) = _tqToExp t

_tqToExp :: TypQ a -> ExprU
_tqToExp = _unExpr . unTypQ

expToTyp :: ExprU -> ErrorOr Typ
-- Note: We use Nodes instead of Leafs for int/bool/str so that -
-- the representation for types is similar to the representation for values
-- it is not a hard requirement though (we can choose any representation)
expToTyp (Node "TUnit" [])    = return $ Typ ttunit
expToTyp (Node "TInt" [])     = return $ Typ ttint
expToTyp (Node "TBool" [])    = return $ Typ ttbool
expToTyp (Node "TString" [])  = return $ Typ ttstring
expToTyp (Node "TArr" [e1,e2]) = do
  Typ t1 <- expToTyp e1
  Typ t2 <- expToTyp e2
  return $ Typ $ ttarr t1 t2
expToTyp (Node "TTuple" [e1,e2]) = do
  Typ t1 <- expToTyp e1
  Typ t2 <- expToTyp e2
  return $ Typ $ tttuple t1 t2
expToTyp (Node "TList" [e]) = do
  Typ t <- expToTyp e
  return $ Typ $ ttlist t
expToTyp e = Left $ "Bad type expression: " ++ show e

-- Private
newtype TmpExpr a = TmpExpr { _unExpr :: ExprU }

instance TSym TmpExpr where
  ttunit = TmpExpr $ Node "TUnit" []
  ttint = TmpExpr $ Node "TInt" []
  ttbool = TmpExpr $ Node "TBool" []
  ttstring = TmpExpr $ Node "TString" []
  ttarr a b = TmpExpr $ Node "TArr" [_unExpr a, _unExpr b]
  tttuple a b = TmpExpr $ Node "TTuple" [_unExpr a, _unExpr b]
  ttlist a = TmpExpr $ Node "TList" [_unExpr a]
-- END Private

