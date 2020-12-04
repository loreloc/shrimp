<img src="logo.svg" width=50%>


## Language Syntax

The syntax for the programs written in `shrimp` is expressed using EBNF as following:
```ebnf
Type ::= "int"
Integer ::= [0-9]+
Identifier ::= [a-zA-Z_][a-zA-Z_0-9]*
ArithmeticExpr ::= Integer
                 | Identifier
                 | ArithmeticExpr "+" ArithmeticExpr
                 | ArithmeticExpr "-" ArithmeticExpr
                 | ArithmeticExpr "*" ArithmeticExpr
BooleanExpr ::= True | False
              | "not" BooleanExpr
              | BooleanExpr "or" BooleanExpr
              | BooleanExpr "and" BooleanExpr
              | ArithmeticExpr "eq" ArithmeticExpr
              | ArithmeticExpr "leq" ArithmeticExpr
VariableDecl ::= "let" Identifier "of" Type ";"
Command ::= {Assignment | Branch | Loop} ";"
Assignment ::= Identifier "=" ArithmeticExpr
Branch ::= "if" "(" BooleanExpr ")" "then" [Command]* "else" [Command]* "end if"
Loop ::= "while" "(" BooleanExpr ")" "do" [Command]* "end loop"
Program ::= "shrimp" [VariableDecl]* "begin" [Command]* "end"
```
