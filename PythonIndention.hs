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
module PythonIndention where

import Token
import Lexer

-- Replace each Newline Whitespace pair with Newline (Indent|Dedent)+
addIndentation :: [Token] -> [Token]
addIndentation = addIndention [0]

-- Use a stack of indent levels per http://docs.python.org/2/reference/lexical_analysis.html
addIndention :: [Int] -> [Token] -> [Token]
addIndention _ [] = []
addIndention stack@(s0:_) (Newline n : Whitespace w : tokens) =
  let l = whitespaceLength w in Newline n :
    if l == s0 then addIndention stack tokens
    else if l > s0 then Indent : addIndention (l : stack) tokens
    else let (dedents, newStack) = handleDedent stack l in dedents ++ addIndention newStack tokens
-- New line starting in column 0
addIndention stack@(s0:_) (Newline n : tokens) = Newline n :
  if 0 == s0 then addIndention stack tokens
  else let (dedents, newStack) = handleDedent stack 0 in dedents ++ addIndention newStack tokens
addIndention stack (token : tokens) = token : addIndention stack tokens

handleDedent stack newIndentColumn = let (stackToPop, stackAfter) = break (<= newIndentColumn) stack in
  if newIndentColumn /= head stackAfter then
    error $ "Inappropriate dedent! " ++ (show stackToPop) ++ (show stackAfter) ++ (show newIndentColumn)
  else (take (length stackToPop) (repeat Dedent), stackAfter)

whitespaceLength :: String -> Int
whitespaceLength = sum . map charLen
  where charLen '\t' = 4
        charLen _ = 1

removeWhitespace = filter (\t -> case t of Whitespace _ -> False ; _ -> True)

removeComments = filter (\t -> case t of Comment _ -> False ; _ -> True)

removeTrailingNewlines  = reverse . collapse . reverse
  where collapse (a@(Newline _) : b@(Newline _) : rest) = collapse (b : rest)
        collapse rest = rest
