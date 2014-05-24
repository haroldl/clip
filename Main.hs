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
module Main where

import Lexer
import PythonIndention
import Parser
import CodeGen
import Optimize

main = do
  putStrLn ""
  s <- getContents
  let tokens = alexScanTokens s
  let cleanedTokens = removeTrailingNewlines $ removeComments $ removeWhitespace $ addIndentation tokens
  printSection "Raw Tokens" $ map show tokens
  printSection "Final Tokens" $ map show cleanedTokens
  let ast = clip cleanedTokens
  printSection "AST" $ map show ast
  printSection "Code" $ map (codegen []) ast
  let optimized = optimize ast
  printSection "Optimized AST" $ map show optimized
  printSection "Optimized Code" $ map (codegen []) optimized

printSection title lines = do
  putStrLn title
  putStrLn $ take (length title) (repeat '=')
  putStrLn ""
  sequence_ $ map putStrLn lines
  putStrLn ""
