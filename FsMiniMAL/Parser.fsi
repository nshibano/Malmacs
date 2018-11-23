// Signature file for parser generated by fsyacc
module FsMiniMAL.Parser
type token = 
  | WITH
  | WHILE
  | WHEN
  | VAR
  | VAL
  | TYPE
  | TRY
  | TO
  | THEN
  | REMOVE
  | OF
  | MUTABLE
  | LEX
  | IF
  | HIDE
  | FUNCT
  | FUN
  | FN
  | FOR
  | EXCEPTION
  | END
  | ELSE
  | DOWNTO
  | DO
  | CATCH
  | CASE
  | BEGIN
  | AS
  | AND
  | QMARK
  | EXCLAMATION
  | RBRACE
  | BARRBRACKET
  | BARBAR
  | BAR
  | LBRACE
  | UNDERSCORE
  | RBRACKET
  | LESSLESS
  | COLONEQUAL
  | LESSMINUS
  | LBRACKETBAR
  | LBRACKET
  | SEMI
  | COLONCOLON
  | COLON
  | DOT
  | MINUSGREATER
  | COMMA
  | STARSTAR
  | STAR
  | RPAREN
  | LPAREN
  | AMPAMP
  | EQUALEQUAL
  | EQUAL
  | EOF
  | STRING of (string)
  | FLOAT of (float)
  | CHAR of (char)
  | INT of (string)
  | UNARY of (string)
  | MULTDIV of (string)
  | SUBTRACTIVE of (string)
  | ADDITIVE of (string)
  | CONCAT of (string)
  | COMPARE of (string)
  | QUOTED of (string)
  | IDENT of (string)
type tokenId = 
    | TOKEN_WITH
    | TOKEN_WHILE
    | TOKEN_WHEN
    | TOKEN_VAR
    | TOKEN_VAL
    | TOKEN_TYPE
    | TOKEN_TRY
    | TOKEN_TO
    | TOKEN_THEN
    | TOKEN_REMOVE
    | TOKEN_OF
    | TOKEN_MUTABLE
    | TOKEN_LEX
    | TOKEN_IF
    | TOKEN_HIDE
    | TOKEN_FUNCT
    | TOKEN_FUN
    | TOKEN_FN
    | TOKEN_FOR
    | TOKEN_EXCEPTION
    | TOKEN_END
    | TOKEN_ELSE
    | TOKEN_DOWNTO
    | TOKEN_DO
    | TOKEN_CATCH
    | TOKEN_CASE
    | TOKEN_BEGIN
    | TOKEN_AS
    | TOKEN_AND
    | TOKEN_QMARK
    | TOKEN_EXCLAMATION
    | TOKEN_RBRACE
    | TOKEN_BARRBRACKET
    | TOKEN_BARBAR
    | TOKEN_BAR
    | TOKEN_LBRACE
    | TOKEN_UNDERSCORE
    | TOKEN_RBRACKET
    | TOKEN_LESSLESS
    | TOKEN_COLONEQUAL
    | TOKEN_LESSMINUS
    | TOKEN_LBRACKETBAR
    | TOKEN_LBRACKET
    | TOKEN_SEMI
    | TOKEN_COLONCOLON
    | TOKEN_COLON
    | TOKEN_DOT
    | TOKEN_MINUSGREATER
    | TOKEN_COMMA
    | TOKEN_STARSTAR
    | TOKEN_STAR
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_AMPAMP
    | TOKEN_EQUALEQUAL
    | TOKEN_EQUAL
    | TOKEN_EOF
    | TOKEN_STRING
    | TOKEN_FLOAT
    | TOKEN_CHAR
    | TOKEN_INT
    | TOKEN_UNARY
    | TOKEN_MULTDIV
    | TOKEN_SUBTRACTIVE
    | TOKEN_ADDITIVE
    | TOKEN_CONCAT
    | TOKEN_COMPARE
    | TOKEN_QUOTED
    | TOKEN_IDENT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startProgram
    | NONTERM_Program
    | NONTERM_Command_list_emptiable
    | NONTERM_Command_list
    | NONTERM_Opt_semi
    | NONTERM_Command
    | NONTERM_Value_def
    | NONTERM_Function_def
    | NONTERM_Var_def
    | NONTERM_Equation
    | NONTERM_Expression_def
    | NONTERM_Expression_5
    | NONTERM_Expression_4
    | NONTERM_Expression_3_comma_list
    | NONTERM_Expression_3
    | NONTERM_Expression_2
    | NONTERM_Expression_1_list
    | NONTERM_Expression_1
    | NONTERM_Expression_3_emptiable_comma_list
    | NONTERM_Pattern_1_list
    | NONTERM_Opt_bar
    | NONTERM_Operator
    | NONTERM_Matching
    | NONTERM_Opt_when_clause
    | NONTERM_Matching_list
    | NONTERM_Label_expr_list
    | NONTERM_Dir_flag
    | NONTERM_Pattern_6
    | NONTERM_Pattern_5_comma_list
    | NONTERM_Pattern_5
    | NONTERM_Pattern_4
    | NONTERM_Pattern_3
    | NONTERM_Pattern_2
    | NONTERM_Pattern_1
    | NONTERM_Pattern_5_emptiable_comma_list
    | NONTERM_Label_pattern_list
    | NONTERM_Type_expr_2
    | NONTERM_Type_expr_1_star_list
    | NONTERM_Type_expr_1
    | NONTERM_Type_expr_2_comma_list
    | NONTERM_Typedef_list
    | NONTERM_Typedef
    | NONTERM_Typedef_decl
    | NONTERM_Constr_decl
    | NONTERM_Constr1_decl
    | NONTERM_Label_decl
    | NONTERM_Label1_decl
    | NONTERM_Opt_mutable
    | NONTERM_Type_params
    | NONTERM_Type_var_list
    | NONTERM_Lexer_def_list
    | NONTERM_Lexer_def
    | NONTERM_Lexer_regexp_3
    | NONTERM_Lexer_regexp_2
    | NONTERM_Lexer_regexp_1
    | NONTERM_Lexer_charset_list
    | NONTERM_Lexer_charset
    | NONTERM_Lexer_rule_list
    | NONTERM_Lexer_rule
    | NONTERM_Lexer_rule_args
    | NONTERM_Lexer_rule_case_list
    | NONTERM_Lexer_rule_case
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Program : (FsMiniMAL.Lexing.LexBuffer -> token) -> FsMiniMAL.Lexing.LexBuffer -> (Syntax.command list * System.Collections.Generic.IDictionary<string, obj>) 
