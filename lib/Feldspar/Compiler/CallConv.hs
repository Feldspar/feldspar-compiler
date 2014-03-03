{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Type rewriting for Feldspar programs
module Feldspar.Compiler.CallConv
  ( rewriteType
  , buildHaskellType
  , buildCType
  )
  where


import Language.Haskell.TH

import System.Plugins.MultiStage

import Foreign.Ptr (Ptr)

import Feldspar (Syntactic(..))

-- | Normalize the type (expand type synonyms and type families)
rewriteType :: Type -> Q Type
rewriteType t = applyTF ''Internal t >>= expandTF

haskellCC :: CallConv
haskellCC = CallConv { arg  = return
                     , res  = appT (conT ''IO) . return
                     }

feldsparCC :: CallConv
feldsparCC = CallConv { arg = conv
                      , res = toIO . appT (conT ''Ptr) . conv
                      }
  where
    conv   = appT (conT ''Ref) . appT (conT ''Rep) . return
    toIO t = appT (appT arrowT t) (appT (conT ''IO) (tupleT 0))

-- | Construct the corresponding Haskell type of a foreign Feldspar
-- function
--
-- > prog1 :: Data Index -> Vector1 Index
-- >
-- > sigD (mkName "h_prog1") $ loadFunType 'prog1 >>= rewriteType >>= buildHaskellType
--
-- becomes
--
-- > h_prog1 :: Index -> IO [Index]
--
buildHaskellType :: Type -> Q Type
buildHaskellType = buildType haskellCC

-- | Construct the corresponding C type of a compiled Feldspar function
--
-- > sigD (mkName "c_prog1_fun") $ loadFunType 'prog1 >>= rewriteType
--                                                    >>= buildCType
--
-- becomes
--
-- > c_prog1_fun :: Word32 -> Ptr (SA Word32) -> IO ()
--
buildCType :: Type -> Q Type
buildCType = buildType feldsparCC

