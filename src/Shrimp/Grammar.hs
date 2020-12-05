module Shrimp.Grammar where

data ArithmeticExpr
  = -- | Addition between sub-expressions
    Add ArithmeticExpr ArithmeticExpr
  | -- | Subtraction between sub-expressions
    Sub ArithmeticExpr ArithmeticExpr
  | -- | Multiplication between sub-expressions
    Mul ArithmeticExpr ArithmeticExpr
  | -- | Constant integer
    Constant Int
  | -- | Identifier string
    Identifier String
  deriving (Show)

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
  | -- | Less than or equal arithmetic operator
    LessEqual ArithmeticExpr ArithmeticExpr
  deriving (Show)

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
  deriving (Show)

-- | Block declaration
type Block = [Command]
