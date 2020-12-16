module Shrimp.SyntaxTree where

-- | Program definition
type Program = (Header, Block)

-- | Command block definition
type Block = [Command]

-- | Variable block definition
type Header = [Variable]

data ArithmeticExpr
  = -- | Constant integer
    Constant Int
  | -- | Integer variable identifier
    IntegerVar String
  | -- | Array variable access
    ArrayVar String ArithmeticExpr
  | -- | Addition between sub-expressions
    Add ArithmeticExpr ArithmeticExpr
  | -- | Subtraction between sub-expressions
    Sub ArithmeticExpr ArithmeticExpr
  | -- | Multiplication between sub-expressions
    Mul ArithmeticExpr ArithmeticExpr
  | -- | (Integer) division between sub-expression
    Div ArithmeticExpr ArithmeticExpr
  | -- | Modulus between sub-expression
    Mod ArithmeticExpr ArithmeticExpr
  | -- | Unary negation
    Neg ArithmeticExpr
  deriving (Eq, Show)

data BooleanExpr
  = -- | Ground True and False
    Truth Bool
  | -- | Boolean variable identifier
    BooleanVar String
  | -- | Not binary operator
    Not BooleanExpr
  | -- | Or binary operator
    Or BooleanExpr BooleanExpr
  | -- | And binary operator
    And BooleanExpr BooleanExpr
  | -- | Equal arithmetic operator
    Equal ArithmeticExpr ArithmeticExpr
  | -- | Not equal arithmetic operator
    NotEqual ArithmeticExpr ArithmeticExpr
  | -- | Less than arithmetic operator
    Less ArithmeticExpr ArithmeticExpr
  | -- | Greater than arithmetic operator
    Greater ArithmeticExpr ArithmeticExpr
  | -- | Less than or equal arithmetic operator
    LessEqual ArithmeticExpr ArithmeticExpr
  | -- | Less than or equal arithmetic operator
    GreaterEqual ArithmeticExpr ArithmeticExpr
  deriving (Eq, Show)

data Command
  = -- | Skip
    Skip
  | -- | Arithmetic assignment
    ArithmeticAssignment String ArithmeticExpr
  | -- | Boolean assignment
    BooleanAssignment String BooleanExpr
  | -- | Array element assignment
    ArrayAssignment String ArithmeticExpr ArithmeticExpr
  | -- | Branch command
    Branch BooleanExpr Block Block
  | -- | Loop command
    Loop BooleanExpr Block
  deriving (Eq, Show)

data Variable
  = -- | Integer variable declaration
    IntegerDecl String
  | -- | Boolean variable declaration
    BooleanDecl String
  | -- | Array variable of fixed size declaration
    ArrayDecl String Int
  deriving (Eq, Show)
