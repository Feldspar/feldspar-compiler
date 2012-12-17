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

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Backend.C.Plugin.BlockProgramHandler where

import Data.List (partition)

import Feldspar.Transformation

import Feldspar.Compiler.Imperative.Representation (isScalarType)
-- ===========================================================================
--  == Plugin for floating variable declarations.
-- ===========================================================================

data BlockProgramHandler = BlockProgramHandler

instance Default [Declaration ()] where
    def = []

instance Combine [Declaration ()] where
    combine a b = a ++ b

instance Transformation BlockProgramHandler where
    type From BlockProgramHandler = ()
    type To BlockProgramHandler = ()
    type Down BlockProgramHandler = ()
    type Up BlockProgramHandler = [Declaration ()]
    type State BlockProgramHandler = ()

instance Transformable BlockProgramHandler Block where
        -- The main work horse. up tr are definitions floating upwards.
        transform t s d b = tr
            { result = (result tr)
                { locals = locals (result tr) ++ up tr
                }
            , up = []
            }
            where
                tr = defaultTransform t s d b

instance Transformable BlockProgramHandler Program where
    transform t s d p =
      case result tr of -- Note [Floating initializations]
        e | Just b <- getBlock e
          , (flt, noFlt) <- splt b ->
               Result (newLocals e noFlt) () (flt ++ up tr)
        _ -> tr
      where
        tr = defaultTransform t s d p
        splt b = partition scalarValueOrUninitialized (locals b)

-- | Returns a block for constructs that support floating. Keep implementation
-- in sync with newLocals below.
getBlock :: Program t -> Maybe (Block t)
getBlock (SeqLoop _ _ b _ _) = Just b
getBlock (ParLoop _ _ _ b _ _) = Just b
getBlock (BlockProgram b _) = Just b
getBlock _ = Nothing


-- | Sets the locals of a construct. Keep implementation in sync with getBlock
-- above.
newLocals :: Program t -> [Declaration t] -> Program t
newLocals (SeqLoop c b' b l1 l2) d = SeqLoop c b' (b { locals = d }) l1 l2
newLocals (ParLoop c b' s b l1 l2) d = ParLoop c b' s (b { locals = d }) l1 l2
newLocals (BlockProgram b l) [] = blockBody b
newLocals (BlockProgram b l) d = BlockProgram (b { locals = d }) l
newLocals _ _ = error "newLocals and getBlock out of sync in BlockProgramHandler.hs"

-- | True if a declaration is a scalar value or a complex type that is
-- not initialized.
scalarValueOrUninitialized :: Declaration t -> Bool
scalarValueOrUninitialized (Declaration {..})
  | Just t <- initVal = isScalarType (varType declVar)
  | otherwise = True

instance Plugin BlockProgramHandler where
    type ExternalInfo BlockProgramHandler = ()
    executePlugin BlockProgramHandler _ procedure = 
        result $ transform BlockProgramHandler ({-state-}) () procedure

{-

Note [Floating initializations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Declarations and initializations should be floated out of loops for
reasons of efficiency. We need to be careful so that we preserve the
order of initialization. Consider the program:

((\v n -> sequential n ((0...1)::Vector1 WordN) (\i s -> (i, s))) :: Vector1 WordN -> Data Length -> Data [WordN])

The generated code should look like this:

    struct array x1 = {0};
    struct array v4 = {0};

    initArray(&x1, sizeof(uint32_t), 2);
    at(uint32_t,&x1,0) = 0;
    at(uint32_t,&x1,1) = 1;
    v4 = x1;
    for(...) {...}

Initializing v4 any sooner than that might give mysterious garbage as
input in the first loop iteration.

We tried to avoid the problem by not floating any declarations as soon
as we found one declaration that was not a scalar value and
immediately initialized, but that made the output hard to read for
humans.

-}
