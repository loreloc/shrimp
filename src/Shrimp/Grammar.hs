module Shrimp.Grammar where

data ArithmeticExpr
  = -- | Addition between sub-expressions
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
  | -- | Constant integer
    Constant Int
  | -- | Identifier string
    Identifier String
  deriving (Eq, Show)

data BooleanExpr
  = -- | Ground True and False
    Boolean Bool
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

-- | Commands declaration
data Command
  = -- | Skip
    Skip
  | -- | Assignment
    Assignment String ArithmeticExpr
  | -- | Branch command
    Branch BooleanExpr Block Block
  | -- | Loop command
    Loop BooleanExpr Block
  deriving (Eq, Show)

-- | Block declaration
type Block = [Command]
