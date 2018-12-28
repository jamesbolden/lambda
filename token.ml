type token =
    | PACKAGE
    | IMPORT
    | WHERE
    | DATA
    | STRUCT
    | TYPE
    | CLASS
    | INSTANCE
    | TRY
    | CATCH
    | WHILE
    | DO
    | AS
    | CASE
    | OF
    | LET
    | IN
    | IF
    | THEN
    | ELSE
    | LAMBDA
    | NIL
    | UNIT
    | ANNOT
    | LPAREN
    | RPAREN
    | LBRACK
    | RBRACK
    | RIGHTARROW
    | FATRIGHTARROW
    | FORALL
    | INFIXL
    | INFIXR
    | INFIX
    | DOT
    | COMMA
    | WILDCARD
    | BACKTICK
    | DEF
    | BAR
    | TOPLEVEL
    | ELLIPSES
    | INDENT
    | DEDENT
    | SEPARATOR
    | SEMICOLON
    | EOF
    | UPPERID of string
    | LOWERID of string
    | OP of string
    | PRIMINTEGER of int
    | PRIMFLOATING of float
    | PRIMCHAR of char
    | INTEGER of int
    | FLOATING of float
    | CHAR of char
    | STRING of string
