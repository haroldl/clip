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
module AST where

data Expr = Literal String
          | NoOp
          | ToDo
          | Assign { destinations :: [Expr], values :: [Expr] }
          | FunDef { name :: String, params :: Params, body :: [Expr] }
          | Format { destination :: Expr, values :: [Expr], newline :: Bool }
          | T
          | Nil
          | VariableRef { identifier :: String }
          | NotExpr Expr
          | ReturnExpr (Maybe [Expr])
          | ApplyFun { function :: Expr, args :: Params }
          | Block [Expr]
          | CondBlock [(Expr, [Expr])]
          | GetElem { container :: Expr, subscriptRefs :: [Expr] }
          | GetField { object :: Expr, field :: String }
          | Param { paramName :: Maybe Expr, paramVal :: Maybe Expr }
  deriving (Eq, Show)

data Params = Params { positional :: [Expr],
                       varargsName :: Maybe String,
                       kwargsName :: Maybe String }
  deriving (Eq, Show)

emptyParams = Params [] Nothing Nothing
