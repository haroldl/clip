{
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
module Lexer where

import Token
}

%wrapper "posn"

$digit = [0-9]
$nonzerodigit = [1-9]
$lowercase = [a-z]
$uppercase = [A-Z]
$letter = [ $lowercase $uppercase ]
$whitespace = $white # [\n\r]
$nodoublequote = $printable # \"
$nosinglequote = $printable # \'

tokens :-
  \+    { \t -> \s -> OpPlus }
  \-    { \t -> \s -> OpMinus }
  \*    { \t -> \s -> OpStar }
  \* \* { \t -> \s -> OpStarStar }
  \/    { \t -> \s -> OpSlash }
  \/ \/ { \t -> \s -> OpSlashSlash }
  \%    { \t -> \s -> OpPercent }
  \< \< { \t -> \s -> OpLessLess }
  \> \> { \t -> \s -> OpGreaterGreater }
  \&    { \t -> \s -> OpAmpersand }
  \|    { \t -> \s -> OpPipe }
  \^    { \t -> \s -> OpCarat }
  \~    { \t -> \s -> OpTilde }
  \<    { \t -> \s -> OpLess }
  \>    { \t -> \s -> OpGreater }
  \< \= { \t -> \s -> OpLessEq }
  \> \= { \t -> \s -> OpGreaterEq }
  \= \= { \t -> \s -> OpEquals }
  \=    { \t -> \s -> OpAssign }
  \! \= { \t -> \s -> OpNotEquals }
  \< \> { \t -> \s -> OpNotEquals }
  \+ \= { \t -> \s -> OpPlusAssign }
  \- \= { \t -> \s -> OpMinusAssign }
  \* \= { \t -> \s -> OpMultAssign }
  \/ \= { \t -> \s -> OpDivAssign }
  \% \= { \t -> \s -> OpModAssign }
  \& \= { \t -> \s -> OpBitAndAssign }
  \| \= { \t -> \s -> OpBitOrAssign }
  \^ \= { \t -> \s -> OpBitXorAssign }
  \< \< \= { \t -> \s -> OpLeftShiftAssign }
  \> \> \= { \t -> \s -> OpRightShiftAssign }
  \* \* \= { \t -> \s -> OpExptAssign }
  \/ \/ \= { \t -> \s -> OpFloorDivAssign }
  \@    { \t -> \s -> At }
  \:    { \t -> \s -> Colon }
  \;    { \t -> \s -> SemiColon }
  \,    { \t -> \s -> Comma }
  \.    { \t -> \s -> Period }
  \(    { \t -> \s -> OpenParen }
  \)    { \t -> \s -> CloseParen }
  \[    { \t -> \s -> OpenBracket }
  \]    { \t -> \s -> CloseBracket }
  \{    { \t -> \s -> OpenBrace }
  \}    { \t -> \s -> CloseBrace }
  \`    { \t -> \s -> BackTick }
  \n                                      { buildNewline }
  \\ \n                                   ;
  $whitespace+                            { \t -> \s -> Whitespace s }
  \# ([^\n]*)                             { \t -> \s -> Comment s }
  \" (\\ $printable | $printable # [\"\\])* \"                       { buildSingleQuoted }
  \' (\\ $printable | $printable # [\'\\])* \'                       { buildSingleQuoted }
  \" \" \" ($nodoublequote | \n | \"{1,2} $nodoublequote)* \" \" \"  { buildTripleQuoted }
  \' \' \' ($nosinglequote | \n | \'{1,2} $nosinglequote)* \' \' \'  { buildTripleQuoted }
  and                                     { \t -> \s -> And }
  as                                      { \t -> \s -> As }
  assert                                  { \t -> \s -> Assert }
  break                                   { \t -> \s -> Break }
  class                                   { \t -> \s -> Class }
  continue                                { \t -> \s -> Continue }
  def                                     { \t -> \s -> Def }
  del                                     { \t -> \s -> Del }
  elif                                    { \t -> \s -> Elif }
  else                                    { \t -> \s -> Else }
  except                                  { \t -> \s -> Except }
  exec                                    { \t -> \s -> Exec }
  finally                                 { \t -> \s -> Finally }
  for                                     { \t -> \s -> For }
  from                                    { \t -> \s -> From }
  global                                  { \t -> \s -> Global }
  if                                      { \t -> \s -> If }
  import                                  { \t -> \s -> Import }
  in                                      { \t -> \s -> In }
  is                                      { \t -> \s -> Is }
  lambda                                  { \t -> \s -> Lambda }
  not                                     { \t -> \s -> Not }
  or                                      { \t -> \s -> Or }
  pass                                    { \t -> \s -> Pass }
  print                                   { \t -> \s -> Print }
  raise                                   { \t -> \s -> Raise }
  return                                  { \t -> \s -> Return }
  try                                     { \t -> \s -> Try }
  while                                   { \t -> \s -> While }
  with                                    { \t -> \s -> With }
  yield                                   { \t -> \s -> Yield }
  True                                    { \t -> \s -> BooleanLiteral True }
  False                                   { \t -> \s -> BooleanLiteral False }
  ($letter | \_) ($letter | $digit | \_)* { \t -> \s -> Identifier s }
  0 | ($nonzerodigit $digit*)             { \t -> \s -> NumLiteral (read s) }

{

unescape ('\\' : c : rest) = c : unescape rest
unescape (c : rest) = c : unescape rest
unescape [] = []

buildSingleQuoted t s = StringLiteral $ unescape $ take (length s - 2) $ drop 1 s

buildTripleQuoted t s = StringLiteral $ take (length s - 6) $ drop 3 s

buildNewline (AlexPn offset line col) s = Newline line

}
