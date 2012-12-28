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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Feldspar.Compiler.Imperative.FromCore.Literal where



import Control.Monad.RWS
import Data.Complex
import GHC.Float (float2Double)

import Language.Syntactic

import Feldspar.Core.Types as Core
import Feldspar.Core.Interpretation
import Feldspar.Core.Constructs.Literal

import Feldspar.Range (upperBound)

import Feldspar.Compiler.Imperative.Frontend
import Feldspar.Compiler.Imperative.FromCore.Interpretation

instance Compile (Literal :|| Core.Type) dom
  where
    compileExprSym (C' (Literal a)) info Nil = literal (infoType info) (infoSize info) a

    compileProgSym (C' (Literal a)) info loc Nil = literalLoc loc (infoType info) (infoSize info) a

literal :: TypeRep a -> Size a -> a -> CodeWriter Expr
literal UnitType        _  ()     = return $ LitI I32 0
literal BoolType        _  a      = return $ litB a
literal trep@IntType{}  sz a      = return $ LitI (compileTypeRep trep sz) (toInteger a)
literal FloatType       _  a      = return $ litF $ float2Double a
literal (ComplexType t) _  (r:+i) = do re <- literal t (defaultSize t) r
                                       ie <- literal t (defaultSize t) i
                                       return $ LitC re ie
literal t s a = do loc <- freshVar "x" t s
                   literalLoc loc t s a
                   return loc

literalLoc :: Location -> TypeRep a -> Size a -> a -> CodeWriter ()
literalLoc loc (ArrayType t) (rs :> es) e
    = do
        tellProg [initArray loc $ LitI I32 $ toInteger $ upperBound rs]
        zipWithM_ (writeElement t es) (map (LitI I32) [0..]) e
  where writeElement :: TypeRep a -> Size a -> Expr -> a -> CodeWriter ()
        writeElement ty sz ix x = do
            literalLoc (loc :!: ix) ty sz x

literalLoc loc (Tup2Type ta tb) (sa,sb) (a,b) =
    do literalLoc (loc :.: "member1") ta sa a
       literalLoc (loc :.: "member2") tb sb b

literalLoc loc (Tup3Type ta tb tc) (sa,sb,sc) (a,b,c) =
    do literalLoc (loc :.: "member1") ta sa a
       literalLoc (loc :.: "member2") tb sb b
       literalLoc (loc :.: "member3") tc sc c
       
literalLoc loc (Tup4Type ta tb tc td) (sa,sb,sc,sd) (a,b,c,d) =
    do literalLoc (loc :.: "member1") ta sa a
       literalLoc (loc :.: "member2") tb sb b
       literalLoc (loc :.: "member3") tc sc c
       literalLoc (loc :.: "member4") td sd d
       
literalLoc loc (Tup5Type ta tb tc td te) (sa,sb,sc,sd,se) (a,b,c,d,e) =
    do literalLoc (loc :.: "member1") ta sa a
       literalLoc (loc :.: "member2") tb sb b
       literalLoc (loc :.: "member3") tc sc c
       literalLoc (loc :.: "member4") td sd d
       literalLoc (loc :.: "member5") te se e
       
literalLoc loc (Tup6Type ta tb tc td te tf) (sa,sb,sc,sd,se,sf) (a,b,c,d,e,f) =
    do literalLoc (loc :.: "member1") ta sa a
       literalLoc (loc :.: "member2") tb sb b
       literalLoc (loc :.: "member3") tc sc c
       literalLoc (loc :.: "member4") td sd d
       literalLoc (loc :.: "member5") te se e
       literalLoc (loc :.: "member6") tf sf f
       
literalLoc loc (Tup7Type ta tb tc td te tf tg) (sa,sb,sc,sd,se,sf,sg) (a,b,c,d,e,f,g) =
    do literalLoc (loc :.: "member1") ta sa a
       literalLoc (loc :.: "member2") tb sb b
       literalLoc (loc :.: "member3") tc sc c
       literalLoc (loc :.: "member4") td sd d
       literalLoc (loc :.: "member5") te se e
       literalLoc (loc :.: "member6") tf sf f
       literalLoc (loc :.: "member7") tg sg g

literalLoc loc t sz a =
    do rhs <- literal t sz a
       assign loc rhs

