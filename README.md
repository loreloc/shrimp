<img src="logo.svg" width=50%>


## Language Syntax

The syntax for the programs written in `shrimp` is expressed using EBNF as following:
```ebnf
Type ::= "int"
Integer ::= [0-9]+
Identifier ::= [a-zA-Z_]+
ArithmeticExpr ::= Integer
                 | Identifier
                 | ArithmeticExpr "+" ArithmeticExpr
                 | ArithmeticExpr "-" ArithmeticExpr
                 | ArithmeticExpr "*" ArithmeticExpr
BooleanExpr ::= "true" | "false"
              | "not" BooleanExpr
              | BooleanExpr "or" BooleanExpr
              | BooleanExpr "and" BooleanExpr
              | ArithmeticExpr "eq" ArithmeticExpr
              | ArithmeticExpr "leq" ArithmeticExpr
Command ::= {Assignment | Branch | Loop} ";"
Block ::= [Command]*
Assignment ::= Identifier "=" ArithmeticExpr
Branch ::= "if" "(" BooleanExpr ")" "then" Block "else" v "end if"
Loop ::= "while" "(" BooleanExpr ")" "do" Block "end while"
Program ::= "shrimp" Block
```
