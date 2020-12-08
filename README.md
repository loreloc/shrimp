<img src="logo.svg" width=50%>


## Language Syntax

The syntax for the programs written in `shrimp` is expressed using EBNF as following:
```ebnf
Type ::= "int"
Integer ::= [0-9]+
Identifier ::= [a-zA-Z_]+

Program ::= "shrimp" Block
Block ::= [Command]*
Command ::= {Assignment | Branch | Loop}
Assignment ::= Identifier "=" ArithmeticExpr ";"
Branch ::= "if" "(" BooleanExpr ")" "then" Block {"else" Block} "end if" ";"
Loop ::= "while" "(" BooleanExpr ")" "do" Block "end while" ";"

ArithmeticExpr ::=
      ArithmeticTerm "+" ArithmeticExpr
    | ArithmeticTerm "-" ArithmeticExpr
ArithmeticTerm ::=
      ArithmeticFactor "*" ArithmeticTerm
    | ArithmeticFactor "/" ArithmeticTerm
    | ArithmeticFactor "%" ArithmeticTerm
arithmeticFactor ::=
      Integer
    | Identifier
    | "-" ArithmeticExpr
    | "(" ArithmeticExpr ")"

BooleanExpr ::=
      BooleanTerm "or" BooleanExpr
BooleanTerm ::=
      BooleanFactor "and" BooleanTerm
BooleanTerm ::=
      "true"
    | "false"
    | "not" BooleanExpr
    | ArithmetciExpr "eq" ArithmeticExpr
    | ArithmetciExpr "neq" ArithmeticExpr
    | ArithmetciExpr "lt" ArithmeticExpr
    | ArithmetciExpr "gt" ArithmeticExpr
    | ArithmetciExpr "leq" ArithmeticExpr
    | ArithmetciExpr "geq" ArithmeticExpr
    | "(" BooleanExpr ")"
```
