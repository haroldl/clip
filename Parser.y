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
module Parser where

import Token
import AST
}

%name clip
%tokentype { Token }
%error { parseError }

%token
    indent { Indent }
    dedent { Dedent }
    and { And }
    as { As }
    assert { Assert }
    break { Break }
    class { Class }
    continue { Continue }
    def { Def }
    del { Del }
    elif { Elif }
    else { Else }
    except { Except }
    exec { Exec }
    finally { Finally }
    for { For }
    from { From }
    global { Global }
    if { If }
    import { Import }
    in { In }
    is { Is }
    lambda { Lambda }
    not { Not }
    or { Or }
    pass { Pass }
    print { Print }
    raise { Raise }
    return { Return }
    try { Try }
    while { While }
    with { With }
    yield { Yield }
    name    { Identifier $$ }
    number  { NumLiteral $$ }
    boolean { BooleanLiteral $$ }
    string  { StringLiteral $$ }
    newline { Newline $$ }
    '@'     { At }
    ':'     { Colon }
    ';'     { SemiColon }
    ','     { Comma }
    '.'     { Period }
    '('     { OpenParen }
    ')'     { CloseParen }
    '['     { OpenBracket }
    ']'     { CloseBracket }
    '{'     { OpenBrace }
    '}'     { CloseBrace }
    '`'     { BackTick }
    '+'     { OpPlus }
    '-'     { OpMinus }
    '*'     { OpStar }
    '**'    { OpStarStar }
    '/'     { OpSlash }
    '//'    { OpSlashSlash }
    '%'     { OpPercent }
    '<<'    { OpLessLess }
    '>>'    { OpGreaterGreater }
    '&'     { OpAmpersand }
    '|'     { OpPipe }
    '^'     { OpCarat }
    '~'     { OpTilde }
    '<'     { OpLess }
    '>'     { OpGreater }
    '<='    { OpLessEq }
    '>='    { OpGreaterEq }
    '=='    { OpEquals }
    '!='    { OpNotEquals }
    '='     { OpAssign }
    '+='    { OpPlusAssign }
    '-='    { OpMinusAssign }
    '*='    { OpMultAssign }
    '/='    { OpDivAssign }
    '%='    { OpModAssign }
    '&='    { OpBitAndAssign }
    '|='    { OpBitOrAssign }
    '^='    { OpBitXorAssign }
    '<<='    { OpLeftShiftAssign }
    '>>='    { OpRightShiftAssign }
    '**='    { OpExptAssign }
    '//='    { OpFloorDivAssign }

%%

Exp : StmtOrNewlines { reverse $1 }

StmtOrNewlines: {- empty -}  { [] }
              | StmtOrNewlines Stmt { $2 : $1 }
              | StmtOrNewlines newline { $1 }

Decorator: '@' DottedName newline { ToDo }
         | '@' DottedName '(' ')' newline { ToDo }
         | '@' DottedName '(' ArgList ')' newline { ToDo }

Decorators: {- empty -} { [] }
          | Decorator Decorators { $1 : $2 }

Decorated: Decorators ClassDef { ToDo }
         | Decorators FuncDef { $2 } {- TODO: apply the decorator(s) -}

FuncDef: def name Parameters ':' Suite { FunDef $2 (Params $3 Nothing Nothing) $5 }

Parameters: '(' ')' { [] }
          | '(' VarArgsList ')' { $2 }

VarArgsList: {- empty -} { [] }
           | FPDef OptionalDefaultVal { map (\e -> Param e $2) $1 }
           | VarArgsList ',' FPDef OptionalDefaultVal { $1 ++ (map (\e -> Param e $4) $3) }
{- TODO: varargs, kwargs -}

OptionalDefaultVal: {- empty -} { Nothing }
                  | '=' Test { Just $2 }

FPDef: name { [Just $ Literal $1] }
     | '(' FPList ')' { $2 }

FPList: FPList ',' FPDef { $1 ++ $3 }
      | FPDef { $1 }
      | FPList ',' { $1 }

Stmt: SimpleStmt { $1 }
    | CompoundStmt { $1 }

Stmts: {- empty -} { [] }
     | Stmts Stmt { $2 : $1 }

SimpleStmt: SmallStmt newline { $1 }
          | SmallStmt ';' newline { $1 }
          | SmallStmt ';' SimpleStmt { ToDo }

SmallStmt: ExprStmt { $1 }
         | PrintStmt { $1 }
         | DelStmt { $1 }
         | PassStmt { $1 }
         | FlowStmt { $1 }
         | ImportStmt { $1 }
         | GlobalStmt { $1 }
         | ExecStmt { $1 }
         | AssertStmt { $1 }

ExprStmt: TestList { buildBlock $1 }
        | TestList '=' TestList { Assign $1 $3 }
        | TestList '=' YieldExpr { ToDo }
        | TestList AugAssign TestList { ToDo }
        | TestList AugAssign YieldExpr { ToDo }

AugAssign: '+=' { ToDo }
         | '-=' { ToDo }
         | '*=' { ToDo }
         | '/=' { ToDo }
         | '%=' { ToDo }
         | '&=' { ToDo }
         | '|=' { ToDo }
         | '^=' { ToDo }
         | '<<=' { ToDo }
         | '>>=' { ToDo }
         | '**=' { ToDo }
         | '//=' { ToDo }

PrintStmt: print '>>' Test ',' PrintTests { Format $3 (reverse $5) True }
         | print '>>' Test ',' PrintTests ',' { Format $3 (reverse $5) False }
         | print PrintTests { Format T (reverse $2) True }
         | print PrintTests ',' { Format T (reverse $2) False }
         | print { Format T [] True }

PrintTests: Test { [$1] }
          | PrintTests ',' Test { $3 : $1 }

DelStmt: del ExprList { ToDo }

PassStmt: pass { NoOp }

FlowStmt: BreakStmt { ToDo }
        | ContinueStmt { ToDo }
        | ReturnStmt { $1 }
        | RaiseStmt { ToDo }
        | YieldStmt { ToDo }

BreakStmt: break { ToDo }

ContinueStmt: continue { ToDo }

ReturnStmt: return { ReturnExpr Nothing }
          | return TestList { ReturnExpr (Just $2) }

YieldStmt: YieldExpr { ToDo }

RaiseStmt: raise { ToDo }
         | raise Test { ToDo }
         | raise Test ',' Test { ToDo }
         | raise Test ',' Test ',' Test { ToDo }

ImportStmt: ImportName { ToDo }
          | ImportFrom { ToDo }

ImportName: import DottedAsNames { ToDo }

ImportFrom: from Module import '*' { ToDo }
          | from Module import '(' ImportAsNames ')' { ToDo }
          | from Module import ImportAsNames { ToDo }

Module: '.' Module { ToDo }
      | '.' { ToDo }
      | DottedName { ToDo }

ImportAsName: name { ToDo }
            | name as name { ToDo }

DottedAsName: DottedName { ToDo }
            | DottedName as name { ToDo }

ImportAsNames: ImportAsName { ToDo }
             | ImportAsName ',' { ToDo }
             | ImportAsName ',' ImportAsNames { ToDo }

DottedAsNames: DottedAsName { ToDo }
             | DottedAsNames ',' DottedAsName { ToDo }

DottedName: name { ToDo }
          | DottedName '.' name { ToDo }

GlobalStmt: global Names { ToDo }

Names: name { ToDo }
     | Names ',' name { ToDo }

ExecStmt: exec Expr { ToDo }
        | exec Expr in Test { ToDo }
        | exec Expr in Test ',' Test { ToDo }

AssertStmt: assert Test { ToDo }
          | assert Test ',' Test { ToDo }

CompoundStmt: IfStmt { $1 }
            | WhileStmt { $1 }
            | ForStmt { $1 }
            | TryStmt { $1 }
            | WithStmt { $1 }
            | FuncDef { $1 }
            | ClassDef { $1 }
            | Decorated { $1 }

{- ElifBlocks is already in reverse order, so put the 0/1 element OptionalElseBlock on the front, then reverse the list -}
IfStmt: if Test ':' Suite ElifBlocks OptionalElseSuite { CondBlock (($2, $4) : reverse ($6 ++ $5)) }

ElifBlocks: {- empty -} { [] }
          | ElifBlocks ElifBlock { $2 : $1 }

ElifBlock: elif Test ':' Suite { ($2, $4) }

WhileStmt: while Test ':' Suite OptionalElseSuite { ToDo }

ForStmt: for ExprList in TestList ':' Suite OptionalElseSuite { ToDo }

TryStmt: try ':' Suite ExceptClauses finally ':' Suite { ToDo }

ExceptClauses: ExceptClause ':' Suite OptionalElseSuite { ToDo }
             | ExceptClause ':' Suite ExceptClauses { ToDo }

ExceptClause: except Test as Test { ToDo }
            | except Test ',' Test { ToDo }
            | except Test { ToDo }
            | except { ToDo }

WithStmt: with WithItems ':' Suite { ToDo }

WithItems: Test as Expr WithItems { ToDo }
         | Test WithItems { ToDo }
         | {- empty -} { ToDo }

OptionalElseSuite: else ':' Suite { [(T, $3)] }
                 | {- empty -} { [] }

Suite: SimpleStmt { [$1] }
     | newline indent Stmts dedent { reverse $3 }

TestListSafe: OldTests ',' { ToDo }
            | OldTests { ToDo }

OldTests: OldTest OldTests { ToDo }
        | {- empty -} { ToDo }

OldTest: OrTest { ToDo }
       | OldLambdef { ToDo }

OldLambdef: lambda VarArgsList ':' OldTest { ToDo }
          | lambda ':' OldTest { ToDo }

Tests: {- empty -} { [] }
     | Test { [$1] }
     | Tests ',' Test { $1 ++ [$3] }

Test: OrTest { $1 }
    | OrTest if OrTest else Test { ToDo }
    | Lambdef { ToDo }

OrTest: AndTest { $1 }
      | AndTest or OrTest { simpleCall "or" [$1, $3] }

AndTest: NotTest { $1 }
       | NotTest and AndTest { simpleCall "and" [$1, $3] }

NotTest: not NotTest { NotExpr $2 }
       | Comparison { $1 }

Comparison: Expr { $1 }
          | Expr CompOp Comparison { $2 $ [$1, $3] }

CompOp: '<' { \es -> simpleCall "<" es }
      | '>' { \es -> simpleCall ">" es }
      | '==' { \es -> simpleCall "equal" es }
      | '>=' { \es -> simpleCall ">=" es }
      | '<=' { \es -> simpleCall "<=" es }
      | '!=' { \es -> simpleCall "not" [simpleCall "equal" es] }
      | in { \es -> ToDo }
      | not in { \es -> ToDo }
      | is { \es -> ToDo }
      | is not { \es -> ToDo }

Expr: XorExpr { $1 }
    | XorExpr '|' Expr { simpleCall "logior" [$1, $3] }

XorExpr: AndExpr { $1 }
       | AndExpr '^' XorExpr { simpleCall "logxor" [$1, $3] }

AndExpr: ShiftExpr { $1 }
       | ShiftExpr '&' AndExpr { simpleCall "logand" [$1, $3] }

ShiftExpr: ArithExpr { $1 }
         | ArithExpr '<<' ShiftExpr { simpleCall "ash" [$1, $3]  }
         | ArithExpr '>>' ShiftExpr { simpleCall "ash" [$1, negateNumber $3] }

ArithExpr: Term { $1 }
         | Term '+' ArithExpr { simpleCall "+" [$1, $3] }
         | Term '-' ArithExpr { simpleCall "-" [$1, $3] }

Term: Factor { $1 }
    | Factor '*' Term { ToDo }
    | Factor '/' Term { ToDo }
    | Factor '%' Term { ToDo }
    | Factor '//' Term { ToDo }

Factor: '+' Factor { ToDo }
      | '-' Factor { ToDo }
      | '~' Factor { ToDo }
      | Power { $1 }

{- Apply the function returned from parsing Trailers to the Atom expression. -}
Power: Atom Trailers { $2 $1 }
     | Atom Trailers '**' Factor { ToDo }

Atom: name { VariableRef $1 }
    | number { Literal $ show $1 }
    | boolean { if $1 then T else Nil }
    | string { Literal $ show $1 }
    | '(' ')' { ToDo }
    | '(' YieldExpr ')' { ToDo }
    | '(' TestListComp ')' { ToDo }
    | '[' ']' { ToDo }
    | '[' ListMaker ']' { ToDo }
    | '{' '}' { ToDo }
    | '{' DictOrSetMaker '}' { ToDo }
    | '`' TestList1 '`' { ToDo }

ListMaker: Test ListFor { ToDo }
         | Tests ',' { ToDo }
         | Tests { ToDo }

TestListComp: Test CompFor { ToDo }
            | Tests ',' { ToDo }
            | Tests { ToDo }

Lambdef: lambda VarArgsList ':' Test { ToDo }
       | lambda ':' Test { ToDo }

{- Parses a function that modifies an expression, :: Expr -> Expr -}
Trailers: {- empty -} { \e -> e }
        | Trailers Trailer { $2 . $1 }

{-
    Parse a trailer into a function that wraps the appropriate expression around
    the previous one. For example, in "f(x)" the trailer is "(x)" and the previous
    expression e is "f", so the parse result for "(x)" is a function that will
    take "f" and wrap it in an ApplyFun with the params already filled in as "x".
 -}
Trailer: '(' ')' { \e -> ApplyFun e emptyParams }
       | '(' ArgList ')' { \e -> ApplyFun e $2 }
       | '[' SubscriptList ']' { \e -> GetElem e (reverse $2) }
       | '.' name { \e -> GetField e $2 }

SubscriptList: SubscriptList ',' Subscript { $3 : $1 }
             | ',' { [] }
             | {- empty -} { [] }

Subscript: '.' '.' '.' { ToDo }
         | Test { ToDo }
         | Test ':' Test SliceOp { ToDo }
         | Test ':' Test { ToDo }
         | Test ':' SliceOp { ToDo }
         | Test ':' { ToDo }
         | ':' Test SliceOp { ToDo }
         | ':' Test { ToDo }
         | ':' SliceOp { ToDo }
         | ':' { ToDo }

SliceOp: ':' Test { ToDo }

ExprList: Expr { ToDo }
        | ExprList ',' Expr { ToDo }
        | ExprList ',' Expr ',' { ToDo }

TestList: Test { [$1] }
        | TestList ',' Test { $1 ++ [$3] }
        | TestList ',' Test ',' { $1 ++ [$3] }

DictOrSetMaker: Test ':' Test CompFor { ToDo }
              | KeyValuePairs { ToDo } 
              | KeyValuePairs ',' { ToDo }
              | Test CompFor { ToDo }
              | Test1 { ToDo }
              | Test1 ',' { ToDo }

KeyValuePairs: Test ':' Test { ToDo }
             | KeyValuePairs ',' Test ':' Test { ToDo }

Test1: Test1 ',' Test { ToDo }
     | Test { ToDo }

ClassDef: class name ':' Suite { ToDo }
        | class name '(' ')' ':' Suite { ToDo }
        | class name '(' TestList ')' ':' Suite { ToDo }

{- TODO: add varargs/kwargs support -}
ArgList: Argument1 { Params (reverse $1)  Nothing Nothing }
       | Argument1 ',' '*' VarArgs1 { Params (reverse $1)  Nothing Nothing }
       | Argument1 ',' '*' VarArgs1 ',' '**' Test { Params (reverse $1)  Nothing Nothing }
       | Argument1 ',' '**' Test { Params (reverse $1)  Nothing Nothing }

VarArgs1: Test { ToDo }
        | Test ',' Argument1 { ToDo }

Argument1: Argument1 ',' Argument { $3 : $1 }
         | Argument { [$1] }

Argument: Test { Param Nothing (Just $1) }
        | Test CompFor { Param Nothing (Just $1) } {- TODO: what does a param followed by a for loop do? -}
        | Test '=' Test { Param (Just $1) (Just $3) }

ListIter: ListFor { ToDo }
        | ListIf { ToDo }

ListFor: for ExprList in TestListSafe { ToDo }
       | for ExprList in TestListSafe ListIter { ToDo }

ListIf: if OldTest { ToDo }
      | if OldTest ListIter { ToDo }

CompIter: CompFor { ToDo }
        | CompIf { ToDo }

CompFor: for ExprList in OrTest { ToDo }
       | for ExprList in OrTest CompIter { ToDo }

CompIf: if OldTest { ToDo }
      | if OldTest CompIter { ToDo }

TestList1: TestList1 ',' Test { ToDo }
         | Test { ToDo }

EncodingDecl: name { ToDo }

YieldExpr: yield { ToDo }
         | yield TestList { ToDo }

{

simpleCall :: String -> [Expr] -> Expr
simpleCall name args = ApplyFun (Literal name) (Params args Nothing Nothing)

negateNumber :: Expr -> Expr
negateNumber n = case n of
  Literal ('-' : rest) -> Literal rest
  Literal n -> Literal $ '-' : n
  _ -> simpleCall "-" [n]

buildBlock [] = NoOp
buildBlock (e:[]) = e
buildBlock es = Block es

parseError :: [Token] -> a
parseError tokens = error $ "Parse error: " ++ show tokens

}
