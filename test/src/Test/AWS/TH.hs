{-# LANGUAGE TemplateHaskell, CPP, RankNTypes, KindSignatures, PolyKinds #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- |
-- Module      : Test.AWS.TH
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.TH where

import           Data.Time
import           GHC.Exts (RuntimeRep, TYPE)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.AWS.Data.Text
import           Network.AWS.Data.Time
import           Network.AWS.Lens           (view)

#if __GLASGOW_HASKELL__ > 900

liftTyped' :: forall (r :: RuntimeRep) (a :: TYPE r) m. Quote m => m Exp -> Code m a
liftTyped' = unsafeCodeCoerce

#else

liftTyped' :: forall (r :: RuntimeRep) (a :: TYPE r). Q Exp -> Q (TExp a)
liftTyped' = unsafeTExpCoerce

#endif

mkTime :: Text -> Q Exp
mkTime x =
    case fromText x :: Either String ISO8601 of
        Left  e -> error (show e)
        Right t -> [|view _Time t|]

instance Lift (Time a) where
    lift (Time x) = AppE (ConE (mkName "Time")) <$> lift x
    liftTyped x = liftTyped' (lift x)

instance Lift UTCTime where
    lift (UTCTime x y) = do
        x' <- lift x
        y' <- lift y
        return $ AppE (AppE (ConE (mkName "UTCTime")) x') y'
    liftTyped x = liftTyped' (lift x)

instance Lift DiffTime where
    lift x = [|toEnum $(lift (fromEnum x))|]
    liftTyped x = liftTyped' (lift x)

instance Lift Day where
    lift x = [|toEnum $(lift (fromEnum x))|]
    liftTyped x = liftTyped' (lift x)
