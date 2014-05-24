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
module UnitTests where

import Test.HUnit

import Token
import PythonIndention

main = do result <- runTestTT tests
          putStrLn $ show result

tests = test [
    "testTabSize" ~: do
      assertEqual "whitespace length of tabs should be 4" 6 (whitespaceLength " \t "),
    "testIndentDedent" ~: do
      let input = [Newline 1,
                   Whitespace "  ", Newline 2,
                   Whitespace "        ", Newline 3,
                   Whitespace "        ", Newline 4,
                   Whitespace "        ", Newline 5,
                   Whitespace "  "]
          result = addIndentation input in
        assertEqual "" [Newline 1,Indent,Newline 2,Indent,Newline 3,Newline 4,Newline 5,Dedent] result
  ]
