{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Feldspar.Compiler.Marshal
  ( SA(..)
  )
  where

import System.Plugins.MultiStage
import Feldspar.Core.Types (IntN(..), WordN(..))

import Data.Int (Int32)
import Data.Word (Word32)
import Data.Complex (Complex(..),realPart,imagPart)
import Control.Applicative

import Foreign.Ptr (Ptr)
import Foreign.Marshal (new, newArray, peekArray)
import Foreign.Storable (Storable(..))
import qualified Foreign.Storable.Record as Store

instance Reference IntN        where type Ref IntN        = IntN
instance Reference WordN       where type Ref WordN       = WordN
instance Reference (Complex a) where type Ref (Complex a) = Complex a

instance Marshal IntN        where type Rep IntN        = IntN
instance Marshal WordN       where type Rep WordN       = WordN
instance Marshal (Complex a) where type Rep (Complex a) = Complex a

instance (Storable (Rep a), Marshal a) => Marshal [a]
  where
    type Rep [a] = SA (Rep a)
    to xs = do
        let len  = fromIntegral $ length xs
        let size = fromIntegral $ sizeOf (undefined :: Rep a)
        ys <- mapM to xs
        buffer <- newArray ys
        return $ SA buffer len size (fromIntegral (len * size))
    from SA{..} = mapM from =<< peekArray (fromIntegral elems) buf


-- | Buffer descriptor for Feldspar arrays
data SA a = SA { buf   :: Ptr a
               , elems :: Int32
               , esize :: Int32
               , bytes :: Word32
               }
  deriving (Eq, Show)

storeSA :: Storable a => Store.Dictionary (SA a)
storeSA = Store.run $ SA
    <$> Store.element buf
    <*> Store.element elems
    <*> Store.element esize
    <*> Store.element bytes

instance Storable a => Storable (SA a)
  where
    sizeOf    = Store.sizeOf    storeSA
    alignment = Store.alignment storeSA
    peek      = Store.peek      storeSA
    poke      = Store.poke      storeSA

instance (Storable a) => Reference (SA a)
  where
    type Ref (SA a) = Ptr (SA a)
    ref   = new
    deref = peek

storeComplex :: (RealFloat a, Storable a)
             => Store.Dictionary (Complex a)
storeComplex = Store.run $ (:+)
    <$> Store.element realPart
    <*> Store.element imagPart

instance (RealFloat a, Storable a) => Storable (Complex a)
  where
    sizeOf    = Store.sizeOf    storeComplex
    alignment = Store.alignment storeComplex
    peek      = Store.peek      storeComplex
    poke      = Store.poke      storeComplex

