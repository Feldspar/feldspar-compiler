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

{-# LANGUAGE EmptyDataDecls, TypeFamilies #-}

module Feldspar.Compiler.Backend.C.Plugin.BlockProgramHandler where

import Data.List
import Feldspar.Transformation
import Feldspar.Compiler.Backend.C.CodeGeneration
import Feldspar.Compiler.Backend.C.Options
import Feldspar.Compiler.Error

-- ===========================================================================
--  == Type definition generator plugin
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
        transform t s d b = tr
            { result = (result tr)
                { locals = (locals $ result tr) ++ up tr
                }
            , up = ([])
            }
            where
                tr = defaultTransform t s d b

instance Transformable BlockProgramHandler Program where
        transform t s d p =
            case result tr of
                BlockProgram b _ -> Result (blockBody b) () (locals b ++ up tr)
                _ -> tr
            where
                    tr = defaultTransform t s d p


instance Plugin BlockProgramHandler where
    type ExternalInfo BlockProgramHandler = ()
    executePlugin BlockProgramHandler externalInfo procedure = 
        result $ transform BlockProgramHandler ({-state-}) () procedure
