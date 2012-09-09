--
-- Copyright (c) 2009-2011, ERICSSON AB
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright notice, 
--       this list of conditions and the following disclaimer.
--     * Redistributions in binary form must reproduce the above copyright
--       notice, this list of conditions and the following disclaimer in the
--       documentation and/or other materials provided with the distribution.
--     * Neither the name of the ERICSSON AB nor the names of its contributors
--       may be used to endorse or promote products derived from this software
--       without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE 
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
-- SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
-- CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
-- OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
--

{-# LANGUAGE OverlappingInstances, UndecidableInstances, StandaloneDeriving #-}

module Feldspar.Transformation.Framework where

import Feldspar.Compiler.Error
import Feldspar.Compiler.Imperative.Representation

transformationError = handleError "PluginArch/TransformationFramework" InternalError

-- ===========
-- == Utils ==
-- ===========

class Default t where
    def :: t
    def = transformationError "Default value requested."

class Combine t where
    combine :: t -> t -> t
    combine = transformationError "Default combination function used."

class Convert a b where
    convert :: a -> b

instance Default () where
    def = ()

instance Default [a] where
    def = []

instance Default Int where
    def = 0

instance (Default a, Default b) => Default (a,b) where
    def = (def, def)

instance (Default a, Default b, Default c) => Default (a,b,c) where
    def = (def, def, def)

instance Combine () where
    combine _ _ = ()

instance Combine String where
    combine s1 s2 = s1 ++ s2

instance Combine Int where
    combine i1 i2 = i1 + i2

instance (Combine a, Combine b)
    => Combine (a,b) where
        combine (x,y) (v,w) = (combine x v, combine y w)

instance Default b => Convert a b where
    convert _ = def

-- =============================
-- == TransformationFramework ==
-- =============================

class (Default (Up t), Combine (Up t))
    => Transformation t where
        type From t
        type To t

        type State t
        type Down t
        type Up t

data Result t s
        = Result
        { result    :: s (To t)
        , state     :: State t
        , up        :: Up t
        }

deriving instance (Transformation t, Show (s (To t)), Show (State t), Show (Up t)) => Show (Result t s)

data Result1 t s a
        = Result1
        { result1   :: s (a (To t))
        , state1    :: State t
        , up1       :: Up t
        }

deriving instance (Transformation t, Show (s (b (To t))), Show (State t), Show (Up t)) => Show (Result1 t s b)

-- The following classes used to have `Transformation t` as super-class, but
-- this resulted in looping dictionaries (at run time) after switching to
-- GHC-7.4. This may or may not be related to the following (unconfirmed) bug:
--
--   http://hackage.haskell.org/trac/ghc/ticket/5913
--
-- The constraint `Transformation t` has currently been moved to the relevant
-- instances.

class Transformable t s where
        transform :: t -> State t -> Down t -> s (From t) -> Result t s

class Transformable1 t s a where
        transform1 :: t -> State t -> Down t -> s (a (From t)) -> Result1 t s a

class DefaultTransformable t s where
        defaultTransform :: t -> State t -> Down t -> s (From t) -> Result t s

class DefaultTransformable1 t s a where
        defaultTransform1 :: t -> State t -> Down t -> s (a (From t)) -> Result1 t s a

instance (DefaultTransformable t s)
    => Transformable t s where
        transform = defaultTransform

instance (DefaultTransformable1 t s a)
    => Transformable1 t s a where
        transform1 = defaultTransform1

