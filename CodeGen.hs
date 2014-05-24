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
module CodeGen where

import Data.List
import AST

data Context = Function String
             | Class String

contextName (Function name) = name
contextName (Class name) = name

codegen :: [Context] -> Expr -> String
codegen ctx T = "t"
codegen ctx Nil = "nil"
codegen ctx (Assign dests vals) = "(psetf " ++ (codegenSeveral ctx dests) ++ " " ++ (codegenSeveral ctx vals) ++ ")"
codegen ctx (VariableRef name) = name
codegen ctx (Literal value) = value
codegen ctx (Format dest vals newline) = "(format " ++ codegen ctx dest ++ " " ++ show formatString ++ " " ++ codegenSeveral ctx vals ++ ")"
  where formatString = (unwords $ take (length vals) (repeat "~a")) ++ (if newline then "~%" else "")
codegen ctx (FunDef name params body) = "(defun " ++ name ++ " (" ++ genParams ctx params ++ ") " ++ codegenSeveral (Function name : ctx) body ++ ")"
codegen (c:ctx) (ReturnExpr Nothing) = "(return-from " ++ contextName c ++ ")"
codegen (c:ctx) (ReturnExpr (Just exprs)) = "(return-from " ++ contextName c ++ " " ++ codegenSeveral (c:ctx) exprs ++ ")"
codegen ctx (ApplyFun name args) = "(" ++ codegen ctx name ++ " " ++ genArgs ctx args ++ ")"
codegen ctx (CondBlock cases) = "(cond " ++ unwords (map (genCondCase ctx) cases) ++ ")"
codegen ctx ToDo = error "ack! There is a ToDo left in the parse tree!"
codegen ctx expr = error $ "ack! Unimplemented AST structure, need to fix CodeGen for " ++ (show expr)

codegenSeveral ctx exprs = unwords $ map (codegen ctx) exprs

genArgs ctx (Params positional varargs kwargs) = unwords (map (genParam ctx) positional) ++ var ++ kw
  where var = case varargs of
                Nothing -> ""
                Just name -> " name"
        kw = "" -- TODO

genParams ctx (Params positional varargs kwargs) = unwords (map (genParam ctx) positional) ++ genVarargs varargs ++ genKwargs kwargs

genParam ctx (Param (Just (Literal name)) _) = name
genParam ctx (Param Nothing (Just (Literal value))) = value
genParam ctx (Param Nothing (Just expr)) = codegen ctx expr
genParam ctx (VariableRef name) = name
genParam ctx (Literal value) = value
genParam ctx expr = codegen ctx expr
--genParam ctx expr = error $ "Can't treat as a parameter: " ++ show expr

genVarargs Nothing = ""
genVarargs (Just name) = " &rest " ++ name

genKwargs Nothing = ""
genKwargs _ = undefined

genCondCase ctx (test, exprs) = "(" ++ codegen ctx test ++ " " ++ codegenSeveral ctx exprs ++ ")"
