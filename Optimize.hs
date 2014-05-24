{-
   Copyright 2014 Harold H. Lee

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
-}
module Optimize where

import AST

optimize exprs = map returnElim exprs

{- Remove return statements that are in a tail position -}
returnElim (FunDef n p body) = FunDef n p (returnElimForBlock body)
returnElim expr = expr

returnElimForBlock [] = []
returnElimForBlock block = blockBeginning ++ concatMap unpackReturn blockLast
  where (blockBeginning, blockLast) = splitAt (length block - 1) block

unpackReturn (ReturnExpr Nothing) = [Nil]
unpackReturn (ReturnExpr (Just vals)) = vals
unpackReturn (CondBlock condCases) = [CondBlock $ map handleCondCase condCases]
  where handleCondCase (test, block) = (test, returnElimForBlock block)
unpackReturn expr = [expr]
