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
module Token where

data Token = Newline Int
           | Comment String
           | Whitespace String
           | Indent
           | Dedent
           | Identifier String
           | NumLiteral Integer
           | StringLiteral String
           | BooleanLiteral Bool
           | At
           | Colon
           | SemiColon
           | Comma
           | Period
           | OpenParen
           | CloseParen
           | OpenBracket
           | CloseBracket
           | OpenBrace
           | CloseBrace
           | BackTick
           | And
           | As
           | Assert
           | Break
           | Class
           | Continue
           | Def
           | Del
           | Elif
           | Else
           | Except
           | Exec
           | Finally
           | For
           | From
           | Global
           | If
           | Import
           | In
           | Is
           | Lambda
           | Not
           | Or
           | Pass
           | Print
           | Raise
           | Return
           | Try
           | While
           | With
           | Yield
           | OpPlus
           | OpMinus
           | OpStar
           | OpStarStar
           | OpSlash
           | OpSlashSlash
           | OpPercent
           | OpLessLess
           | OpGreaterGreater
           | OpAmpersand
           | OpPipe
           | OpCarat
           | OpTilde
           | OpLess
           | OpGreater
           | OpLessEq
           | OpGreaterEq
           | OpEquals
           | OpNotEquals
           | OpAssign
           | OpPlusAssign
           | OpMinusAssign
           | OpMultAssign
           | OpDivAssign
           | OpModAssign
           | OpBitAndAssign
           | OpBitOrAssign
           | OpBitXorAssign
           | OpLeftShiftAssign
           | OpRightShiftAssign
           | OpExptAssign
           | OpFloorDivAssign
  deriving (Eq, Show)
