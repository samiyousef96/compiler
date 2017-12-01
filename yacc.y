%output ="yacc.cpp"
	%{
		#include <iostream>
		#include "ErrorRecovery.h"
		using namespace std;
		#include <FlexLexer.h>
        #define _CRT_SECURE_NO_WARNINGS
		int yylex(void);
		int yyparse();
		void yyerror(char *);
		FlexLexer* lexer = new yyFlexLexer();
		ErrorRecovery* err = new ErrorRecovery();
		class Parser
		{
			public:
			int parse()
				{
					return yyparse();
				}
		};
	%}
		%union{
				struct R{
				int i;
				float f;
				double d;
				char c;
				int brackets;
				char* str;
				int myLineNo;
				int myColno;
		}r;
}

/***
 *** C# parser/scanner
 *** Copyright 2002 James Power, NUI Maynooth, Ireland <james.power@may.ie>
 *** This version: 19 Feb 2002
 ***
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY <copyright holder> ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
***/

/* Based on Appendix C of the C# Language Specification,
 *  version 0.28 of 5/7/2001
 */

/* Special tokens to help disambiguate rank_specifiers */
%token RANK_SPECIFIER

/* C.1.4 Tokens */
%token IDENTIFIER 
%token INTEGER_LITERAL FLOAT_LITERAL DOUBLE_LITERAL CHARACTER_LITERAL STRING_LITERAL 



/* C.1.7 KEYWORDS */ 
%token  ABSTRACT AS BASE BOOL BREAK
%token  BYTE CASE CATCH CHAR CHECKED
%token  CLASS CONST CONTINUE DECIMAL DEFAULT
%token  DELEGATE DO DOUBLE ELSE ENUM
%token  EVENT EXPLICIT EXTERN FALSE FINALLY
%token  FIXED FLOAT FOR FOREACH GOTO
%token  IF IMPLICIT IN INT INTERFACE
%token  INTERNAL IS LOCK LONG NAMESPACE
%token  NEW NULL_LITERAL OBJECT OPERATOR OUT
%token  OVERRIDE PARAMS PRIVATE PROTECTED PUBLIC
%token  READONLY REF RETURN SBYTE SEALED
%token  SHORT SIZEOF STACKALLOC STATIC STRING
%token  STRUCT SWITCH THIS THROW TRUE
%token  TRY TYPEOF UINT ULONG UNCHECKED
%token  UNSAFE USHORT USING VIRTUAL VOID
%token  VOLATILE WHILE
%token EQUAL EXTENDS FA9LE FROM GLOBAL ASYNC VAR YIELD ID STAR
%token LEFTB1 RIGHTB1 LEFTB2 RIGHTB2 LEFTB3 RIGHTB3 DOT AND T3GOB PLUS MINUS DIV OR MORE_THAN LESS_THAN QUESTION_MARK
%token THAL TWO_DOT PERCENT CHAPO BADID 

%nonassoc "then"
%nonassoc ELSE WHILE
%nonassoc IDENTIFIER
%nonassoc FA9LE TWO_DOT
%nonassoc "now"
%nonassoc LEFTB2


/* The ones that seem to be context sensitive */
/* Attribute Targets */
%token ASSEMBLY FIELD METHOD MODULE PARAM PROPERTY TYPE
/* Accessor types */
%token GET SET 
/* Event accessor declarations */
%token ADD REMOVE

/*** PUNCTUATION AND SINGLE CHARACTER OPERATORS ***/
%token COMMA  ","
%token LEFT_BRACKET  "["
%token RIGHT_BRACKET "]"

/*** MULTI-CHARACTER OPERATORS ***/
%token PLUSEQ MINUSEQ STAREQ DIVEQ MODEQ
%token XOREQ  ANDEQ   OREQ LTLT GTGT GTGTEQ LTLTEQ EQEQ NOTEQ
%token LEQ GEQ ANDAND OROR PLUSPLUS MINUSMINUS ARROW

%start compilation_unit  /* I think */

%%

/***** C.1.8 Literals *****/
literal
  : boolean_literal {cout<<"literal \n";}
  | INTEGER_LITERAL {cout<<"literal \n";}
  | FLOAT_LITERAL  {cout<<"literal \n";}
  | DOUBLE_LITERAL {cout<<"literal \n";}
  | CHARACTER_LITERAL {cout<<"literal \n"<<$<r.d>1<<"\n";}
  | STRING_LITERAL {cout<<"literal \n";}
  | NULL_LITERAL {cout<<"literal \n";}
  ;
boolean_literal
  : TRUE  {cout<<"boolean_literal \n";}
  | FALSE {cout<<"boolean_literal \n";}
  ;
/********** C.2 Syntactic grammar **********/

/***** C.2.1 Basic concepts *****/
namespace_name
  :  qualifier IDENTIFIER  {cout<<"namespace_name \n";}
  | IDENTIFIER {cout<<"namespace_name \n";}
  ;
/***** C.2.2 Types *****/
type
  : non_array_type  {cout<<"type \n";}
  | array_type {cout<<"type \n";}
  ;
non_array_type
  : simple_type {cout<<"non_array_type \n";}
  ;
simple_type
  : primitive_type {cout<<"simple_type \n";}
  | class_type {cout<<"simple_type \n";}
  | pointer_type {cout<<"simple_type \n";}
  ;
primitive_type
  : numeric_type {cout<<"primitive_type \n";}
  | BOOL {cout<<"primitive_type \n";}
  ;
numeric_type
  : integral_type {cout<<"numeric_type \n";}
  | floating_point_type {cout<<"numeric_type \n";}
  | DECIMAL {cout<<"numeric_type \n";}
  ;
integral_type
  : SBYTE  {cout<<"integral_type \n";}
  | BYTE  {cout<<"integral_type \n";}
  | SHORT  {cout<<"integral_type \n";}
  | USHORT  {cout<<"integral_type \n";}
  | INT  {cout<<"integral_type \n";}
  | UINT  {cout<<"integral_type \n";}
  | LONG  {cout<<"integral_type \n";}
  | ULONG  {cout<<"integral_type \n";}
  | CHAR {cout<<"integral_type \n";}
  ;
floating_point_type
  : FLOAT  {cout<<"floating_point_type \n";}
  | DOUBLE {cout<<"floating_point_type \n";}
  ;
class_type
  : OBJECT  {cout<<"class_type \n";}
  | STRING {cout<<"class_type \n";}
  ;
pointer_type
  : type STAR {cout<<"pointer_type\n";}
  | qualifier IDENTIFIER STAR {cout<<"pointer_type\n";}
  | IDENTIFIER STAR {cout<<"pointer_type\n";}
  | VOID STAR {cout<<"pointer_type\n";}
  ;
array_type
  : array_type RANK_SPECIFIER {cout<<"array_type\n";}	
  | simple_type RANK_SPECIFIER	{cout<<"array_type\n";}	
  |  qualifier IDENTIFIER  RANK_SPECIFIER	{cout<<"array_type\n";}	
  | IDENTIFIER RANK_SPECIFIER	{cout<<"array_type\n";}	
  ;
rank_specifiers_opt
  : /* Nothing */ {cout<<"rank_specifiers_opt nothing \n";}
  | RANK_SPECIFIER rank_specifiers_opt  {cout<<"rank_specifiers_opt \n";}
  ;
/***** C.2.3 Variables *****/
variable_reference
  : expression {cout<<"variable_reference \n";}
  ;
/***** C.2.4 Expressions *****/
argument_list 
  : argument  {cout<<"argument_list     \n";}
  | argument_list COMMA argument  {cout<<"argument_list     \n";}
  ;
argument
  : expression  {cout<<"argument     \n";}
  | REF variable_reference  {cout<<"argument     \n";}
  | OUT variable_reference  {cout<<"argument     \n";}
  ;
primary_expression
  : parenthesized_expression  {cout<<"primary_expression     \n";}
  | primary_expression_no_parenthesis  {cout<<"primary_expression     \n";}
  ;
primary_expression_no_parenthesis
  : literal  {cout<<"primary_expression_no_parenthesis     \n";}
  | array_creation_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  | member_access  {cout<<"primary_expression_no_parenthesis     \n";}
  | invocation_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  | element_access  {cout<<"primary_expression_no_parenthesis     \n";}
  | this_access  {cout<<"primary_expression_no_parenthesis     \n";}
  | base_access  {cout<<"primary_expression_no_parenthesis     \n";}
  | new_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  | typeof_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  | sizeof_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  | checked_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  | unchecked_expression  {cout<<"primary_expression_no_parenthesis     \n";}
  ;
parenthesized_expression
  : LEFTB2  expression RIGHTB2 {cout<<"parenthesized_expression \n )";}
  ;
member_access
  : primary_expression DOT IDENTIFIER  {cout<<"member_access     \n";}
  | primitive_type DOT IDENTIFIER  {cout<<"member_access     \n";}
  | class_type DOT IDENTIFIER  {cout<<"member_access     \n";}
  ;
invocation_expression
  : primary_expression_no_parenthesis LEFTB2 argument_list_opt RIGHTB2  {cout<<"invocation_expression     \n";}
  |  qualifier IDENTIFIER  LEFTB2 argument_list_opt RIGHTB2  {cout<<"invocation_expression     \n";}
  | IDENTIFIER LEFTB2 argument_list_opt RIGHTB2  {cout<<"invocation_expression     \n";}
  ;
argument_list_opt
  : /* Nothing */  {cout<<"argument_list_opt nothing     \n";}
  | argument_list  {cout<<"argument_list_opt     \n";}
  ;
element_access
  : primary_expression LEFT_BRACKET expression_list RIGHT_BRACKET  {cout<<"element_access     \n";}
  |  qualifier IDENTIFIER  LEFT_BRACKET expression_list RIGHT_BRACKET  {cout<<"element_access     \n";}
  | IDENTIFIER LEFT_BRACKET expression_list RIGHT_BRACKET  {cout<<"element_access     \n";}
  ;
expression_list_opt
  : /* Nothing */  {cout<<"expression_list_opt    nothing \n";}
  | expression_list  {cout<<"expression_list_opt     \n";}
  ;
expression_list
  : expression  {cout<<"expression_list     \n";}
  | expression_list COMMA expression  {cout<<"expression_list     \n";}
  ;
this_access
  : THIS  {cout<<"this_access     \n";}
  ;
base_access
  : BASE DOT IDENTIFIER  {cout<<"base_access     \n";}
  | BASE LEFT_BRACKET expression_list RIGHT_BRACKET  {cout<<"base_access     \n";}
  ;
post_increment_expression
  : postfix_expression PLUSPLUS  {cout<<"post_increment_expression     \n";}
  |  qualifier IDENTIFIER  PLUSPLUS  {cout<<"post_increment_expression     \n";}
  | IDENTIFIER PLUSPLUS  {cout<<"post_increment_expression     \n";}
  ;
post_decrement_expression
  : postfix_expression MINUSMINUS  {cout<<"post_decrement_expression     \n";}
  |  qualifier IDENTIFIER  MINUSMINUS  {cout<<"post_decrement_expression     \n";}
  | IDENTIFIER MINUSMINUS  {cout<<"post_decrement_expression     \n";}
  ;
new_expression
  : object_creation_expression {cout<<"new_expression  \n";}
  ;
object_creation_expression
  : NEW type  LEFTB2  argument_list_opt  RIGHTB2 {cout<<"object_creation_expression  \n";}
  | NEW qualifier IDENTIFIER  LEFTB2  argument_list_opt  RIGHTB2 {cout<<"object_creation_expression  \n";} 
  | NEW IDENTIFIER  LEFTB2  argument_list_opt  RIGHTB2 {cout<<"object_creation_expression  \n";}
  ;
array_creation_expression
  : NEW non_array_type LEFT_BRACKET expression_list RIGHT_BRACKET rank_specifiers_opt array_initializer_opt {cout<<"array_creation_expression  \n";}
  | NEW qualifier IDENTIFIER LEFT_BRACKET expression_list RIGHT_BRACKET rank_specifiers_opt array_initializer_opt {cout<<"array_creation_expression  \n";}
  | NEW IDENTIFIER LEFT_BRACKET expression_list RIGHT_BRACKET rank_specifiers_opt array_initializer_opt {cout<<"array_creation_expression  \n";}
  | NEW array_type array_initializer {cout<<"array_creation_expression  \n";}
  ;
array_initializer_opt
  : /* Nothing */ {cout<<"array_initializer_opt  nothing\n";}
  | array_initializer {cout<<"array_initializer_opt  \n";}
  ;
typeof_expression
  : TYPEOF  LEFTB2  type  RIGHTB2 {cout<<"typeof_expression  \n";} 
  | TYPEOF  LEFTB2  qualifier IDENTIFIER  RIGHTB2 {cout<<"typeof_expression  \n";} 
  | TYPEOF  LEFTB2  IDENTIFIER  RIGHTB2  {cout<<"typeof_expression  \n";}
  | TYPEOF  LEFTB2  VOID  RIGHTB2  {cout<<"typeof_expression  \n";}
  ;
checked_expression
  : CHECKED  LEFTB2  expression  RIGHTB2  {cout<<"checked_expression  \n";}
  ;
unchecked_expression
  : UNCHECKED  LEFTB2  expression  RIGHTB2  {cout<<"unchecked_expression  \n";}
  ;
pointer_member_access
  : postfix_expression ARROW IDENTIFIER {cout<<"pointer_member_access  \n";}
  |  qualifier IDENTIFIER  ARROW IDENTIFIER {cout<<"pointer_member_access  \n";}
  | IDENTIFIER ARROW IDENTIFIER {cout<<"pointer_member_access  \n";}
  ;
addressof_expression
  : AND unary_expression {cout<<"addressof_expression  \n";}
  ;
sizeof_expression
  : SIZEOF  LEFTB2  type  RIGHTB2  {cout<<"sizeof_expression  \n";}
  | SIZEOF  LEFTB2  qualifier IDENTIFIER  RIGHTB2  {cout<<"sizeof_expression  \n";}
  | SIZEOF  LEFTB2  IDENTIFIER  RIGHTB2  {cout<<"sizeof_expression  \n";}
  ;
postfix_expression
  : primary_expression {cout<<"postfix_expression  \n";}
  | post_increment_expression {cout<<"postfix_expression  \n";}
  | post_decrement_expression {cout<<"postfix_expression  \n";}
  | pointer_member_access {cout<<"postfix_expression  \n";}
  ;
unary_expression_not_plusminus
  : postfix_expression {cout<<"unary_expression_not_plusminus  \n";}
  |  qualifier IDENTIFIER  {cout<<"unary_expression_not_plusminus  \n";}
  | IDENTIFIER %prec "now" {cout<<"unary_expression_not_plusminus  \n";}
  |  T3GOB  unary_expression {cout<<"unary_expression_not_plusminus  \n";}
  |  THAL  unary_expression {cout<<"unary_expression_not_plusminus  \n";}
  | cast_expression {cout<<"unary_expression_not_plusminus  \n";}
  ;
pre_increment_expression
  : PLUSPLUS unary_expression {cout<<"pre_increment_expression  \n";}
  ;
pre_decrement_expression
  : MINUSMINUS unary_expression {cout<<"pre_decrement_expression  \n";}
  ;
unary_expression
  : unary_expression_not_plusminus {cout<<"unary_expression  \n";}
  |  PLUS unary_expression {cout<<"unary_expression  \n";}
  |  MINUS  unary_expression {cout<<"unary_expression  \n";}
  |  STAR  unary_expression {cout<<"unary_expression  \n";}
  | pre_increment_expression {cout<<"unary_expression  \n";}
  | pre_decrement_expression {cout<<"unary_expression  \n";}
  | addressof_expression {cout<<"unary_expression  \n";}
  ;
/* For cast_expression we really just want a (type) in the brackets,
 * but have to do some factoring to get rid of conflict with expressions.
 * The paremtnesised expression in the first three cases below should be 
 * semantically restricted to an identifier, optionally follwed by qualifiers
 */
cast_expression
  :  LEFTB2  expression  RIGHTB2  unary_expression_not_plusminus {cout<<"cast_expression  \n";}
  |  LEFTB2  multiplicative_expression  STAR   RIGHTB2  unary_expression  {cout<<"cast_expression  \n";}
  |  LEFTB2   qualifier IDENTIFIER  RANK_SPECIFIER type_quals_opt  RIGHTB2  unary_expression {cout<<"cast_expression  \n";}  
  |  LEFTB2  IDENTIFIER RANK_SPECIFIER type_quals_opt  RIGHTB2  unary_expression   {cout<<"cast_expression  \n";}
  |  LEFTB2  primitive_type type_quals_opt  RIGHTB2  unary_expression {cout<<"cast_expression  \n";}
  |  LEFTB2  class_type type_quals_opt  RIGHTB2  unary_expression {cout<<"cast_expression  \n";}
  |  LEFTB2  VOID type_quals_opt  RIGHTB2  unary_expression {cout<<"cast_expression  \n";}
  ;
type_quals_opt
  : /* Nothing */   {cout<<"type_quals_opt nothing\n";}
  | type_quals{cout<<"type_quals_opt \n";}
  ;
type_quals
  : type_qual  {cout<<"type_quals \n";}
  | type_quals type_qual {cout<<"type_quals \n";}
  ;
type_qual 
  : RANK_SPECIFIER   {cout<<"type_qual \n";}
  |  STAR   {cout<<"type_qual \n";}
  ;
multiplicative_expression
  : unary_expression  {cout<<"multiplicative_expression \n";}
  | multiplicative_expression  STAR  unary_expression  {cout<<"multiplicative_expression \n";}  
  | multiplicative_expression  DIV  unary_expression  {cout<<"multiplicative_expression \n";}
  | multiplicative_expression  PERCENT  unary_expression  {cout<<"multiplicative_expression \n";}
  ;
additive_expression
  : multiplicative_expression  {cout<<"additive_expression \n";}
  | additive_expression  PLUS multiplicative_expression  {cout<<"additive_expression \n";}
  | additive_expression  MINUS  multiplicative_expression  {cout<<"additive_expression \n";}
  ;
shift_expression
  : additive_expression   {cout<<"shift_expression \n";}
  | shift_expression LTLT additive_expression  {cout<<"shift_expression \n";}
  | shift_expression GTGT additive_expression  {cout<<"shift_expression \n";}
  ;
relational_expression
  : shift_expression  {cout<<"relational_expression \n";}
  | relational_expression  LESS_THAN shift_expression  {cout<<"relational_expression \n";}
  | relational_expression MORE_THAN shift_expression  {cout<<"relational_expression \n";}
  | relational_expression LEQ shift_expression  {cout<<"relational_expression \n";}
  | relational_expression GEQ shift_expression  {cout<<"relational_expression \n";}
  | relational_expression IS type  {cout<<"relational_expression \n";}
  | relational_expression AS type  {cout<<"relational_expression \n";}
  ;
equality_expression
  : relational_expression  {cout<<"equality_expression \n";}
  | equality_expression EQEQ relational_expression  {cout<<"equality_expression \n";}
  | equality_expression NOTEQ relational_expression  {cout<<"equality_expression \n";}
  ;
and_expression
  : equality_expression  {cout<<"and_expression \n";}
  | and_expression AND equality_expression  {cout<<"and_expression \n";}
  ;
exclusive_or_expression
  : and_expression  {cout<<"exclusive_or_expression \n";}
  | exclusive_or_expression  CHAPO  and_expression  {cout<<"exclusive_or_expression \n";}
  ;
inclusive_or_expression
  : exclusive_or_expression  {cout<<"inclusive_or_expression \n";}
  | inclusive_or_expression  OR  exclusive_or_expression  {cout<<"inclusive_or_expression \n";}
  ;
conditional_and_expression
  : inclusive_or_expression  {cout<<"conditional_and_expression \n";}
  | conditional_and_expression ANDAND inclusive_or_expression  {cout<<"conditional_and_expression \n";}
  ;
conditional_or_expression
  : conditional_and_expression  {cout<<"conditional_or_expression \n";}
  | conditional_or_expression OROR conditional_and_expression  {cout<<"conditional_or_expression \n";}
  ;
conditional_expression
  : conditional_or_expression  {cout<<"conditional_expression \n";}
  | conditional_or_expression  QUESTION_MARK  expression  TWO_DOT expression  {cout<<"conditional_expression \n";}
  ;
assignment
: unary_expression assignment_operator expression  {cout<<"assignment \n";}
  ;
assignment_operator
  :  EQUAL    {cout<<"assignment_operator \n";}
  | PLUSEQ   {cout<<"assignment_operator \n";}
  | MINUSEQ   {cout<<"assignment_operator \n";}
  | STAREQ   {cout<<"assignment_operator \n";}
  | DIVEQ   {cout<<"assignment_operator \n";}
  | MODEQ   {cout<<"assignment_operator \n";}
  | XOREQ   {cout<<"assignment_operator \n";}
  | ANDEQ   {cout<<"assignment_operator \n";}
  | OREQ   {cout<<"assignment_operator \n";}
  | GTGTEQ   {cout<<"assignment_operator \n";}
  | LTLTEQ   {cout<<"assignment_operator \n";}
  ;
expression
  : conditional_expression  {cout<<"expression \n";}
  | assignment  {cout<<"expression \n";}
  ;
constant_expression
  : expression  {cout<<"constant_expression \n";}
  ;
boolean_expression
  : expression  {cout<<"boolean_expression \n";}
  ;
/***** C.2.5 Statements *****/
statement
  : labeled_statement  {cout<<"statement \n";}
  | declaration_statement  {cout<<"statement \n";}
  | embedded_statement  {cout<<"statement \n";}
  ;
embedded_statement
  : block  {cout<<"embedded_statement \n";}
  | empty_statement  {cout<<"embedded_statement \n";}
  | expression_statement  {cout<<"embedded_statement \n";}
  | selection_statement  {cout<<"embedded_statement \n";}
  | iteration_statement  {cout<<"embedded_statement \n";}
  | jump_statement  {cout<<"embedded_statement \n";}
  | try_statement  {cout<<"embedded_statement \n";}
  | checked_statement  {cout<<"embedded_statement \n";}
  | unchecked_statement  {cout<<"embedded_statement \n";}
  | lock_statement  {cout<<"embedded_statement \n";}
  | using_statement  {cout<<"embedded_statement \n";}
  | unsafe_statement  {cout<<"embedded_statement \n";}
  | fixed_statement  {cout<<"embedded_statement \n";}
  ;
block
  :  LEFTB1 statement_list_opt  RIGHTB1  {cout<<"block \n";}
  ;
statement_list_opt
  : /* Nothing */  {cout<<"statement_list_opt \n";}
  | statement_list  {cout<<"statement_list_opt \n";}
  ;

statement_list
  : statement  {cout<<"statement_list \n";}
  | statement_list statement  {cout<<"statement_list \n";}
  ;
empty_statement
  :  FA9LE   {cout<<"empty_statement \n";}
  ;
labeled_statement
  : IDENTIFIER  TWO_DOT statement  {cout<<"labeled_statement \n";}
  ;
declaration_statement
  : local_variable_declaration  FA9LE   {cout<<"declaration_statement \n";}
  | local_constant_declaration  FA9LE   {cout<<"declaration_statement \n";}
  ;
local_variable_declaration
  : type variable_declarators  {cout<<"local_variable_declaration \n";}
  | qualifier IDENTIFIER variable_declarators  {cout<<"local_variable_declaration \n";}
  | IDENTIFIER variable_declarators  {cout<<"local_variable_declaration \n";}
  ;
variable_declarators
  : variable_declarator  {cout<<"variable_declarators \n";}
  | variable_declarators COMMA variable_declarator  {cout<<"variable_declarators \n";}
  ;
variable_declarator
  : IDENTIFIER  {cout<<"variable_declarator \n";}
  | IDENTIFIER  EQUAL  variable_initializer  {cout<<"variable_declarator \n";}
  | badid {cout<<"variable_declarator \n";}
  | badid EQUAL  variable_initializer  {cout<<"variable_declarator \n";}
  ;
  badid
  : BADID {int x  = strlen($<r.str>1)  ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno-x>1,"BAD ID",$<r.str>1);}
  ;
variable_initializer
  : expression  {cout<<"variable_initializer \n";}
  | array_initializer  {cout<<"variable_initializer \n";}
  | stackalloc_initializer  {cout<<"variable_initializer \n";}
  ;
stackalloc_initializer
: STACKALLOC type  LEFT_BRACKET expression RIGHT_BRACKET  {cout<<"stackalloc_initializer \n";} 
| STACKALLOC qualifier IDENTIFIER  LEFT_BRACKET expression RIGHT_BRACKET  {cout<<"stackalloc_initializer \n";} 
| STACKALLOC IDENTIFIER  LEFT_BRACKET expression RIGHT_BRACKET   {cout<<"stackalloc_initializer \n";}
  ; 
local_constant_declaration
  : CONST type constant_declarators  {cout<<"local_constant_declaration \n";}
  | CONST qualifier IDENTIFIER constant_declarators  {cout<<"local_constant_declaration \n";}
  | CONST IDENTIFIER constant_declarators  {cout<<"local_constant_declaration \n";}
  ;
constant_declarators
  : constant_declarator  {cout<<"constant_declarators \n";}
  | constant_declarators COMMA constant_declarator  {cout<<"constant_declarators \n";}
  ;
constant_declarator
  : IDENTIFIER  {int x  = strlen($<r.str>1)  ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno>1,"A const field requires a value to be provided","");}
  | IDENTIFIER  EQUAL  constant_expression {cout<<"constant_declarator \n";}
  | badid  EQUAL  constant_expression {cout<<"constant_declarator \n";}
  | badid {int x  = strlen($<r.str>1)  ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno>1,"A const field requires a value to be provided",$<r.str>1);}
  ;    
expression_statement
  : statement_expression  FA9LE   {cout<<"expression_statement \n";}
  ;
statement_expression
  : invocation_expression  {cout<<"statement_expression \n";}
  | object_creation_expression  {cout<<"statement_expression \n";}
  | assignment  {cout<<"statement_expression \n";}
  | post_increment_expression  {cout<<"statement_expression \n";}
  | post_decrement_expression  {cout<<"statement_expression \n";}
  | pre_increment_expression  {cout<<"statement_expression \n";}
  | pre_decrement_expression  {cout<<"statement_expression \n";}
  ;
selection_statement
  : if_statement  {cout<<"selection_statement \n";}
  | switch_statement  {cout<<"selection_statement \n";}
  ;
if_statement
  : IF  LEFTB2  boolean_expression  RIGHTB2   embedded_statement  %prec "then" {cout<<"if_statement \n";}  
  | IF  LEFTB2  boolean_expression  RIGHTB2   embedded_statement  ELSE  embedded_statement {cout<<"if_statement \n";}
  | ELSE {int x  = strlen($<r.str>1)  ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno-x>1,"Unexpected Symbol",$<r.str>1);} embedded_statement {cout<<"if_statement \n";} 
  | IF LEFTB2 RIGHTB2 {err->errQ->enqueue($<r.myLineNo>3,$<r.myColno>3,"Unexpected Symbol",$<r.str>3);}  embedded_statement  {cout<<"if_statement \n";} 
  ;
switch_statement
  : SWITCH  LEFTB2  expression  RIGHTB2  switch_block  {cout<<"switch_statement \n";}
  | SWITCH  switch_block  {cout<<"switch_statement \n";err->errQ->enqueue($<r.myLineNo>2,$<r.myColno-1>2,"Unexpected Symbol { expecting \" ( \"","");}
  ;
switch_block
  :  LEFTB1 switch_sections_opt  RIGHTB1  {cout<<"switch_block \n";}
  ;
switch_sections_opt
  : /* Nothing */  {cout<<"switch_sections_opt nothing \n";}
  | switch_sections  {cout<<"switch_sections_opt \n";}
  ;
switch_sections
  : switch_section  {cout<<"switch_sections \n";}
  | switch_sections switch_section  {cout<<"switch_sections \n";}
  ;
switch_section
  : switch_labels statement_list  {cout<<"switch_section \n";}
  ;
switch_labels
  : switch_label  {cout<<"switch_labels \n";}
  | switch_labels switch_label  {cout<<"switch_labels \n";}
  ;
switch_label
  : CASE constant_expression  TWO_DOT  {cout<<"switch_label \n";}
  | DEFAULT  TWO_DOT  {cout<<"switch_label \n";}
  ;
iteration_statement
  : while_statement  {cout<<"iteration_statement \n";}
  | do_statement  {cout<<"iteration_statement \n";}
  | for_statement  {cout<<"iteration_statement \n";}
  | foreach_statement  {cout<<"iteration_statement \n";}
  ;
unsafe_statement
  : UNSAFE block  {cout<<"unsafe_statement \n";}
  ;
while_statement
  : WHILE  LEFTB2  boolean_expression  RIGHTB2  embedded_statement  {cout<<"while_statement \n";}
  ;
do_statement
  : DO embedded_statement %prec "then" {cout<<"do_statement \n";err->errQ->enqueue($<r.myLineNo>2,$<r.myColno>2,"Unexpected Symbol , expecting \"while\"","");}
  | DO embedded_statement WHILE  LEFTB2  boolean_expression  RIGHTB2   FA9LE   {cout<<"do_statement \n";}
  ;
for_statement
  : FOR  LEFTB2  for_initializer_opt  FA9LE  for_condition_opt  FA9LE  for_iterator_opt  RIGHTB2  embedded_statement {cout<<"for_statement \n";}
  ;
for_initializer_opt
  : /* Nothing */  {cout<<"for_initializer_opt nothing \n";}
  | for_initializer {cout<<"for_initializer_opt \n";}
  ;
for_condition_opt
  : /* Nothing */{cout<<"for_condition_opt nothing\n"; }
  | for_condition {cout<<"for_condition_opt \n"; }
  ;
for_iterator_opt
  : /* Nothing */  {cout<<"for_iterator_opt nothing \n";}
  | for_iterator {cout<<"for_iterator_opt \n"; }
  ;
for_initializer
  : local_variable_declaration {cout<<"for_initializer \n"; }
  | statement_expression_list  {cout<<"for_initializer \n"; }
  ;
for_condition
  : boolean_expression {cout<<"for_condition\n";}
  ;
for_iterator
  : statement_expression_list {cout<<"for_iterator\n";}

  ;
statement_expression_list
  : statement_expression  {cout<<"statement_expression_list \n";}
  | statement_expression_list COMMA statement_expression  {cout<<"statement_expression_list \n";}
  ;
foreach_statement
  : FOREACH  LEFTB2  type IDENTIFIER IN expression  RIGHTB2  embedded_statement {cout<<"foreach_statement \n";}
  | FOREACH  LEFTB2  qualifier IDENTIFIER IDENTIFIER IN expression  RIGHTB2  embedded_statement {cout<<"foreach_statement \n";}
  | FOREACH  LEFTB2  IDENTIFIER IDENTIFIER IN expression  RIGHTB2  embedded_statement {cout<<"foreach_statement \n";}
  ;
jump_statement
  : break_statement  {cout<<"jump_statement \n";}
  | continue_statement  {cout<<"jump_statement \n";}
  | goto_statement  {cout<<"jump_statement \n";}
  | return_statement  {cout<<"jump_statement \n";}
  | throw_statement  {cout<<"jump_statement \n";}
  ;
break_statement
  : BREAK  %prec "then" {cout<<"break_statement \n";err->errQ->enqueue($<r.myLineNo>1,$<r.myColno>1,"Missing",";");}
  | BREAK  FA9LE {cout<<"break_statement \n";}
  ;
  
continue_statement
  : CONTINUE  FA9LE   {cout<<"continue_statement \n";}
  ;
goto_statement
  : GOTO IDENTIFIER  %prec "now" FA9LE {cout<<"goto_statement \n";}
  | GOTO IDENTIFIER  %prec "then" {cout<<"goto_statement \n";err->errQ->enqueue($<r.myLineNo>2,$<r.myColno>2,"Missing",";");}
  | GOTO CASE constant_expression  FA9LE {cout<<"goto_statement \n";}
  | GOTO DEFAULT  FA9LE {cout<<"goto_statement \n";}
  ;
return_statement
  : RETURN expression_opt   FA9LE   {cout<<"return_statement \n";}
  ;
expression_opt
  : /* Nothing */   {cout<<"expression_opt nothing \n";}
  |  expression  {cout<<"expression_opt \n";}
  ;
throw_statement
  : THROW expression_opt  FA9LE   {cout<<"throw_statement \n";}
  ;
try_statement
  : TRY block catch_clauses  {cout<<"try_statement \n";}
  | TRY block finally_clause  {cout<<"try_statement \n";}
  | TRY block catch_clauses finally_clause  {cout<<"try_statement \n";}
  ;
catch_clauses
  : catch_clause  {cout<<"catch_clauses \n";}
  | catch_clauses catch_clause  {cout<<"catch_clauses \n";}
  ;
catch_clause
  : CATCH  LEFTB2  class_type identifier_opt  RIGHTB2  block  {cout<<"catch_clause \n";}
  | CATCH  LEFTB2   qualifier IDENTIFIER  identifier_opt  RIGHTB2  block  {cout<<"catch_clause \n";}
  | CATCH  LEFTB2  IDENTIFIER identifier_opt  RIGHTB2  block  {cout<<"catch_clause \n";}
  | CATCH block  {cout<<"catch_clause \n";}
  ;
identifier_opt
  : /* Nothing */  {cout<<"identifier_opt  nothing\n";}
  | IDENTIFIER  {cout<<"identifier_opt \n";}
  ;
finally_clause
  : FINALLY block  {cout<<"finally_clause \n";}
  ;
checked_statement
  : CHECKED block  {cout<<"checked_statement \n";}
  ;
unchecked_statement
  : UNCHECKED block  {cout<<"unchecked_statement \n";}
  ;
lock_statement
  : LOCK  LEFTB2  expression  RIGHTB2  embedded_statement   {cout<<"lock_statement \n";}
  ;
using_statement
  : USING  LEFTB2  resource_acquisition  RIGHTB2  embedded_statement  {cout<<"using_statement \n";}
  ;
resource_acquisition
  : local_variable_declaration  {cout<<"resource_acquisition \n";}
  | expression  {cout<<"resource_acquisition \n";}
  ;
fixed_statement
/*! : FIXED  LEFTB2  pointer_type fixed_pointer_declarators  RIGHTB2  embedded_statement */
  : FIXED  LEFTB2   type fixed_pointer_declarators  RIGHTB2  embedded_statement  {cout<<"fixed_statement \n";}
  | FIXED  LEFTB2   qualifier IDENTIFIER fixed_pointer_declarators  RIGHTB2  embedded_statement  {cout<<"fixed_statement \n";}
  | FIXED  LEFTB2   IDENTIFIER fixed_pointer_declarators  RIGHTB2  embedded_statement  {cout<<"fixed_statement \n";}
  ;
fixed_pointer_declarators
  : fixed_pointer_declarator  {cout<<"fixed_pointer_declarators \n";}
  | fixed_pointer_declarators COMMA fixed_pointer_declarator  {cout<<"fixed_pointer_declarators \n";}
  ;
fixed_pointer_declarator
  : IDENTIFIER  EQUAL  expression  {cout<<"fixed_pointer_declarator \n";}
  ;
compilation_unit
  : using_directives_opt attributes_opt   {cout<<"compilation_unit \n";}
  | using_directives_opt namespace_member_declarations  {cout<<"compilation_unit \n";}
  ;
using_directives_opt
  : /* Nothing */  {cout<<"using_directives_opt nothing \n";}
  | using_directives  {cout<<"using_directives_opt \n";}
  ;
attributes_opt
  : /* Nothing */  {cout<<"attributes_opt nothing \n";}
  | attributes  {cout<<"attributes_opt \n";}
  ;
namespace_member_declarations_opt
  : /* Nothing */  {cout<<"namespace_member_declarations_opt  nothing \n";}
  | namespace_member_declarations  {cout<<"namespace_member_declarations_opt \n";}
  ;
namespace_declaration
  : attributes_opt NAMESPACE  qualifier IDENTIFIER  namespace_body comma_opt  {cout<<"namespace_declaration \n";}
  | attributes_opt NAMESPACE IDENTIFIER namespace_body comma_opt  {cout<<"namespace_declaration \n";}
  ;
comma_opt
  : /* Nothing */  {cout<<"comma_opt nothing \n";}
  |  FA9LE   {cout<<"comma_opt \n";}
  ;
qualifier
  : IDENTIFIER  DOT    {cout<<"qualifier \n";}
  | qualifier IDENTIFIER  DOT    {cout<<"qualifier \n";}
  ;
namespace_body
  :  LEFTB1 using_directives_opt namespace_member_declarations_opt  RIGHTB1  {cout<<"namespace_body \n";}
  ;
using_directives
  : using_directive  {cout<<"using_directives \n";}
  | using_directives using_directive  {cout<<"using_directives \n";}
  ;
using_directive
  : using_alias_directive  {cout<<"using_directive \n";}
  | using_namespace_directive  {cout<<"using_directive \n";}
  ;
using_alias_directive
  : USING IDENTIFIER  EQUAL   qualifier IDENTIFIER   FA9LE {cout<<"using_alias_directive \n";}
  | USING IDENTIFIER  EQUAL  IDENTIFIER  FA9LE {cout<<"using_alias_directive \n";}
  ;
using_namespace_directive
  : USING namespace_name   {cout<<"using_namespace_directive \n";}{int x  = strlen($<r.str>2) ; err->errQ->enqueue($<r.myLineNo>2,$<r.myColno>2,"MISSING",";");}
  | USING namespace_name  FA9LE {cout<<"using_namespace_directive \n";}
  ;
namespace_member_declarations
  : namespace_member_declaration  {cout<<"namespace_member_declarations \n";}
  | namespace_member_declarations namespace_member_declaration  {cout<<" \n";}
  ;
namespace_member_declaration
  : namespace_declaration  {cout<<"namespace_member_declaration \n";}
  | type_declaration  {cout<<"namespace_member_declaration \n";}
  ;
type_declaration
  : class_first  {cout<<"type_declaration \n";}
  | struct_declaration  {cout<<"type_declaration \n";}
  | interface_declaration  {cout<<"type_declaration \n";}
  | enum_declaration  {cout<<"type_declaration \n";}
  | delegate_declaration  {cout<<"type_declaration \n";}
  ;

/***** Modifiers *****/
/* This now replaces:
 * class_modifier, constant_modifier, field_modifier, method_modifier, 
 * property_modifier, event_modifier, indexer_modifier, operator_modifier, 
 * constructor_modifier, struct_modifier, interface_modifier, 
 * enum_modifier, delegate_modifier
 */
modifiers_opt
  : /* Nothing */  {cout<<"modifiers_opt nothing \n";}
  | modifiers  {cout<<"modifiers_opt \n";}
  ;
modifiers
  : modifier  {cout<<"modifiers \n";}
  | modifiers modifier  {cout<<"modifiers \n";}
  ;
modifier
  : ABSTRACT  {cout<<"modifier \n";}
  | EXTERN  {cout<<"modifier \n";}
  | INTERNAL  {cout<<"modifier \n";}
  | NEW  {cout<<"modifier \n";}
  | OVERRIDE  {cout<<"modifier \n";}
  | PRIVATE  {cout<<"modifier \n";}
  | PROTECTED  {cout<<"modifier \n";}
  | PUBLIC  {cout<<"modifier \n";}
  | READONLY  {cout<<"modifier \n";}
  | SEALED  {cout<<"modifier \n";}
  | STATIC  {cout<<"modifier \n";}
  | UNSAFE  {cout<<"modifier \n";}
  | VIRTUAL  {cout<<"modifier \n";}
  | VOLATILE  {cout<<"modifier \n";}
  ;
/***** C.2.6 Classes *****/
class_first
 :  attributes_opt modifiers_opt class_declaration  {cout<<"class_first \n";}
 |  attributes_opt modifiers_opt id bad_class_declaration  {cout<<"class_first \n";}
 ;
class_declaration
  :   CLASS IDENTIFIER class_base_opt class_body comma_opt  {cout<<"class_declaration \n";}
  |   CLASS BADID class_base_opt class_body comma_opt  {int x  = strlen($<r.str>2)  ; err->errQ->enqueue($<r.myLineNo>2,$<r.myColno-x>2,"BAD ID",$<r.str>2);}  {cout<<"class_declaration \n";}
  |   CLASS INTEGER_LITERAL class_base_opt class_body comma_opt  {int x  = strlen($<r.str>2) ; err->errQ->enqueue($<r.myLineNo>2,$<r.myColno-x>2,"BAD ID",$<r.str>2);}{cout<<"class_declaration \n";}  
  ;
bad_class_declaration
  :  IDENTIFIER  class_base_opt class_body comma_opt     {cout<<"bad_class_declaration \n";}
  |  BADID class_base_opt class_body comma_opt   {int x  = strlen($<r.str>1) ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno-x>1,"BAD ID",$<r.str>1);}   {cout<<"bad_class_declaration \n";}
  |  INTEGER_LITERAL class_base_opt class_body comma_opt    {int x  = strlen($<r.str>1) ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno-x>1,"BAD ID",$<r.str>1);}  {cout<<"bad_class_declaration \n";}
  ;
  id 
  : IDENTIFIER {int x  = strlen($<r.str>1)  ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno-x>1,"BAD CLASS",$<r.str>1);}
  ;
class_base_opt
  : /* Nothing */  {cout<<"class_base_opt nothing \n";}
  |   class_base {cout<<"class_base_opt \n";} 
  ;
class_base
  :  TWO_DOT class_type {cout<<"class_base \n";} 
  |  TWO_DOT interface_type_list {cout<<"class_base \n";} 
  |  TWO_DOT class_type COMMA interface_type_list {cout<<"class_base \n";} 
  ;
interface_type_list
  :  qualifier IDENTIFIER   {cout<<"interface_type_list \n";}
  |  qualifier badid {cout<<"interface_type_list \n";}
  | IDENTIFIER  {cout<<"interface_type_list \n";}
  | badid
  | interface_type_list COMMA  qualifier IDENTIFIER  {cout<<"interface_type_list \n";} 
  | interface_type_list COMMA IDENTIFIER  {cout<<"interface_type_list \n";}
  | interface_type_list COMMA  qualifier BADID {int x  = strlen($<r.str>4) ; err->errQ->enqueue($<r.myLineNo>4,$<r.myColno-x>4,"BAD ID",$<r.str>4);} {cout<<"interface_type_list \n";} 
  | interface_type_list COMMA badid{cout<<"interface_type_list \n";}
  ;
class_body
  :  LEFTB1 class_member_declarations_opt  RIGHTB1 {cout<<"class_body \n";}
  ;
class_member_declarations_opt
  : /* Nothing */  {cout<<"class_member_declarations_opt  nothing \n";}
  | class_member_declarations {cout<<"class_member_declarations_opt \n";}
  ;
class_member_declarations
  : class_member_declaration {cout<<"class_member_declarations \n";}
  | class_member_declarations class_member_declaration {cout<<"class_member_declarations \n";}
  ;
class_member_declaration
  : constant_declaration  {cout<<"class_member_declaration \n";}
  | field_declaration  {cout<<"class_member_declaration \n";}
  | method_declaration  {cout<<"class_member_declaration \n";}
  | property_declaration  {cout<<"class_member_declaration \n";}
  | event_declaration  {cout<<"class_member_declaration \n";}
  | indexer_declaration  {cout<<"class_member_declaration \n";}
  | operator_declaration  {cout<<"class_member_declaration \n";}
  | constructor_declaration  {cout<<"class_member_declaration \n";}
  | destructor_declaration  {cout<<"class_member_declaration \n";}
/*  | static_constructor_declaration */
  | type_declaration  {cout<<"class_member_declaration \n";}
  | if_statement {cout<<"class_member_declaration \n";}{ err->errQ->enqueue($<r.myLineNo>1,$<r.myColno-2>1,"Unexpected Symbol \"if\"or\"else\"in class,struct or interface member declaration","");}
  ;
constant_declaration
  : attributes_opt modifiers_opt CONST type constant_declarators  FA9LE  {cout<<"constant_declaration \n"} 
  | attributes_opt modifiers_opt CONST qualifier IDENTIFIER constant_declarators  FA9LE  {cout<<"constant_declaration \n"} 
  | attributes_opt modifiers_opt CONST IDENTIFIER constant_declarators  FA9LE  {cout<<"constant_declaration \n"} 
  | attributes_opt modifiers_opt CONST qualifier badid constant_declarators  FA9LE  {cout<<"constant_declaration \n"} 
  | attributes_opt modifiers_opt CONST badid constant_declarators  FA9LE  {cout<<"constant_declaration \n"} 
  ;
field_declaration
  : attributes_opt modifiers_opt type variable_declarators  FA9LE   {cout<<"field_declaration \n";}
  | attributes_opt modifiers_opt qualifier IDENTIFIER variable_declarators  FA9LE   {cout<<"field_declaration \n";}
  | attributes_opt modifiers_opt IDENTIFIER variable_declarators  FA9LE   {cout<<"field_declaration \n";}
  | attributes_opt modifiers_opt qualifier badid variable_declarators  FA9LE   {cout<<"field_declaration \n";}
  | attributes_opt modifiers_opt badid variable_declarators  FA9LE  {cout<<"field_declaration \n";}
  ;
method_declaration   
  : method_header method_body  {cout<<"method_declaration \n";}
  ;
/* Inline return_type to avoid conflict with field_declaration */
method_header
  : attributes_opt modifiers_opt type  qualifier IDENTIFIER   LEFTB2  formal_parameter_list_opt  RIGHTB2   {cout<<"method_header \n";}
  | attributes_opt modifiers_opt qualifier IDENTIFIER  qualifier IDENTIFIER   LEFTB2  formal_parameter_list_opt  RIGHTB2   {cout<<"method_header \n";}
  | attributes_opt modifiers_opt IDENTIFIER  qualifier IDENTIFIER   LEFTB2  formal_parameter_list_opt  RIGHTB2   {cout<<"method_header \n";}
  | attributes_opt modifiers_opt VOID  qualifier IDENTIFIER   LEFTB2  formal_parameter_list_opt  RIGHTB2   {cout<<"method_header \n";}
  | attributes_opt modifiers_opt type IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2    {cout<<"method_header \n";}
  | attributes_opt modifiers_opt qualifier IDENTIFIER IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2    {cout<<"method_header \n";}
  | attributes_opt modifiers_opt IDENTIFIER IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2   {cout<<"method_header \n";}
  | attributes_opt modifiers_opt VOID IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2   {cout<<"method_header \n";}
  ;
formal_parameter_list_opt
  : /* Nothing */  {cout<<"formal_parameter_list_opt nothing\n";}
  | formal_parameter_list  {cout<<"formal_parameter_list_opt \n";}
  ;
return_type
  : type  {cout<<"return_type \n";}
  | VOID  {cout<<"return_type \n";}
  ;
method_body
  : block  {cout<<"method_body \n";}
  |  FA9LE   {cout<<"method_body \n";}
  ;
formal_parameter_list
  : formal_parameter  {cout<<"formal_parameter_list \n";}
  | formal_parameter_list  COMMA formal_parameter  {cout<<"formal_parameter_list \n";}
  ;
formal_parameter
  : fixed_parameter  {cout<<"formal_parameter \n";}
  | parameter_array  {cout<<"formal_parameter \n";}
  ;
fixed_parameter
  : attributes_opt parameter_modifier_opt type IDENTIFIER  {cout<<"fixed_parameter \n";}
  | attributes_opt parameter_modifier_opt qualifier IDENTIFIER IDENTIFIER  {cout<<"fixed_parameter \n";}
  | attributes_opt parameter_modifier_opt IDENTIFIER IDENTIFIER  {cout<<"fixed_parameter \n";}
  ;
parameter_modifier_opt
  : /* Nothing */  {cout<<"parameter_modifier_opt nothing \n";}
  | REF  {cout<<"parameter_modifier_opt \n";}
  | OUT  {cout<<"parameter_modifier_opt \n";}
  ;
parameter_array
/*!  : attributes_opt PARAMS array_type IDENTIFIER */
  : attributes_opt PARAMS type IDENTIFIER  {cout<<"parameter_array \n";}
  | attributes_opt PARAMS qualifier IDENTIFIER IDENTIFIER  {cout<<"parameter_array \n";}
  | attributes_opt PARAMS IDENTIFIER IDENTIFIER  {cout<<"parameter_array \n";}
  ;
property_declaration
  : attributes_opt modifiers_opt type  qualifier IDENTIFIER  
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"property_declaration \n";}
  | attributes_opt modifiers_opt type IDENTIFIER 
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"property_declaration \n";}
  | attributes_opt modifiers_opt qualifier IDENTIFIER  qualifier IDENTIFIER  
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"property_declaration \n";}
  | attributes_opt modifiers_opt qualifier IDENTIFIER IDENTIFIER 
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"property_declaration \n";}
  | attributes_opt modifiers_opt IDENTIFIER  qualifier IDENTIFIER  
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"property_declaration \n";}
  | attributes_opt modifiers_opt IDENTIFIER IDENTIFIER 
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"property_declaration \n";}
  ;
accessor_declarations
  : get_accessor_declaration set_accessor_declaration_opt  {cout<<"accessor_declarations \n";}
  | set_accessor_declaration get_accessor_declaration_opt  {cout<<"accessor_declarations \n";}
  ;
set_accessor_declaration_opt
  : /* Nothing */  {cout<<"set_accessor_declaration_opt nothing \n";}
  | set_accessor_declaration  {cout<<"set_accessor_declaration_opt \n";}
  ;
get_accessor_declaration_opt
  : /* Nothing */  {cout<<"get_accessor_declaration_opt nothing  \n";}
  | get_accessor_declaration  {cout<<"get_accessor_declaration_opt \n";}
  ;
get_accessor_declaration
  : attributes_opt GET 
      EXIT_getset
    accessor_body
      ENTER_getset  {cout<<"get_accessor_declaration \n";}
  ;
set_accessor_declaration
  : attributes_opt SET 
      EXIT_getset
    accessor_body
      ENTER_getset  {cout<<"set_accessor_declaration \n";}
  ;
accessor_body
  : block  {cout<<"accessor_body \n";}
  |  FA9LE   {cout<<"accessor_body \n";}
  ;
event_declaration
  : attributes_opt modifiers_opt EVENT type variable_declarators  FA9LE 
  | attributes_opt modifiers_opt EVENT type  qualifier IDENTIFIER  
      ENTER_accessor_decl 
     LEFTB1 event_accessor_declarations  RIGHTB1
      EXIT_accessor_decl  {cout<<"event_declaration \n";}
  | attributes_opt modifiers_opt EVENT type IDENTIFIER 
      ENTER_accessor_decl 
     LEFTB1 event_accessor_declarations  RIGHTB1
      EXIT_accessor_decl  {cout<<"event_declaration \n";}
  | attributes_opt modifiers_opt EVENT qualifier IDENTIFIER variable_declarators  FA9LE  {cout<<" \n";} 
  | attributes_opt modifiers_opt EVENT qualifier IDENTIFIER  qualifier IDENTIFIER  
      ENTER_accessor_decl 
     LEFTB1 event_accessor_declarations  RIGHTB1
      EXIT_accessor_decl  {cout<<"event_declaration \n";}
  | attributes_opt modifiers_opt EVENT qualifier IDENTIFIER IDENTIFIER 
      ENTER_accessor_decl 
     LEFTB1 event_accessor_declarations  RIGHTB1
      EXIT_accessor_decl  {cout<<"event_declaration \n";}
  | attributes_opt modifiers_opt EVENT IDENTIFIER variable_declarators  FA9LE  {cout<<"event_declaration \n";} 
  | attributes_opt modifiers_opt EVENT IDENTIFIER  qualifier IDENTIFIER  
      ENTER_accessor_decl 
     LEFTB1 event_accessor_declarations  RIGHTB1
      EXIT_accessor_decl  {cout<<"event_declaration \n";}
  | attributes_opt modifiers_opt EVENT IDENTIFIER IDENTIFIER 
      ENTER_accessor_decl 
     LEFTB1 event_accessor_declarations  RIGHTB1
      EXIT_accessor_decl  {cout<<"event_declaration \n";}
  ;
event_accessor_declarations
  : add_accessor_declaration remove_accessor_declaration  {cout<<"event_accessor_declarations \n";}
  | remove_accessor_declaration add_accessor_declaration  {cout<<"event_accessor_declarations \n";}
  ;
add_accessor_declaration
  : attributes_opt ADD 
      EXIT_accessor_decl 
    block 
      ENTER_accessor_decl  {cout<<"add_accessor_declaration \n";}
  ;
remove_accessor_declaration
  : attributes_opt REMOVE 
      EXIT_accessor_decl 
    block 
      ENTER_accessor_decl  {cout<<"remove_accessor_declaration \n";}
  ;
indexer_declaration
  : attributes_opt modifiers_opt indexer_declarator 
      ENTER_getset
     LEFTB1 accessor_declarations  RIGHTB1
      EXIT_getset  {cout<<"indexer_declaration \n";}
  ;
indexer_declarator
  : type THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET  {cout<<"indexer_declarator \n";}
/* | type  qualifier IDENTIFIER   DOT  THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET */
/* | type IDENTIFIER  DOT  THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET */
  | type qualified_this LEFT_BRACKET formal_parameter_list RIGHT_BRACKET  {cout<<"indexer_declarator \n";}
  | qualifier IDENTIFIER THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET  {cout<<"indexer_declarator \n";}
/* | qualifier IDENTIFIER  qualifier IDENTIFIER   DOT  THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET */
/* | qualifier IDENTIFIER IDENTIFIER  DOT  THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET */
  | qualifier IDENTIFIER qualified_this LEFT_BRACKET formal_parameter_list RIGHT_BRACKET  {cout<<"indexer_declarator \n";}
  | IDENTIFIER THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET  {cout<<"indexer_declarator \n";}
/* | IDENTIFIER  qualifier IDENTIFIER   DOT  THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET */
/* | IDENTIFIER IDENTIFIER  DOT  THIS LEFT_BRACKET formal_parameter_list RIGHT_BRACKET */
  | IDENTIFIER qualified_this LEFT_BRACKET formal_parameter_list RIGHT_BRACKET  {cout<<"indexer_declarator \n";}
  ;
qualified_this
  : qualifier THIS  {cout<<"qualified_this \n";}
  ;
/* Widen operator_declaration to make modifiers optional */
operator_declaration
  : attributes_opt modifiers_opt operator_declarator operator_body  {cout<<"operator_declaration \n";}
  ;
operator_declarator
  : overloadable_operator_declarator  {cout<<"operator_declarator \n";}
  | conversion_operator_declarator  {cout<<"operator_declarator \n";}
  ; 
overloadable_operator_declarator
  : type OPERATOR overloadable_operator  LEFTB2  type IDENTIFIER  RIGHTB2   {cout<<"overloadable_operator_declarator \n";}
  | type OPERATOR overloadable_operator  LEFTB2  type IDENTIFIER COMMA type IDENTIFIER  RIGHTB2  {cout<<"overloadable_operator_declarator \n";} 
  | qualifier IDENTIFIER OPERATOR overloadable_operator  LEFTB2  qualifier IDENTIFIER IDENTIFIER  RIGHTB2   {cout<<"overloadable_operator_declarator \n";}
  | qualifier IDENTIFIER OPERATOR overloadable_operator  LEFTB2  qualifier IDENTIFIER IDENTIFIER COMMA qualifier IDENTIFIER IDENTIFIER  RIGHTB2  {cout<<"overloadable_operator_declarator \n";} 
  | IDENTIFIER OPERATOR overloadable_operator  LEFTB2  IDENTIFIER IDENTIFIER  RIGHTB2   {cout<<"overloadable_operator_declarator \n";}
  | IDENTIFIER OPERATOR overloadable_operator  LEFTB2  IDENTIFIER IDENTIFIER COMMA IDENTIFIER IDENTIFIER  RIGHTB2  {cout<<"overloadable_operator_declarator \n";} 
  ;
overloadable_operator
  :  PLUS   {cout<<"overloadable_operator \n";}
  |  MINUS   {cout<<"overloadable_operator \n";} 
  |  T3GOB    {cout<<"overloadable_operator \n";}
  |  THAL    {cout<<"overloadable_operator \n";}
  | PLUSPLUS   {cout<<"overloadable_operator \n";}
  | MINUSMINUS   {cout<<"overloadable_operator \n";}
  | TRUE   {cout<<"overloadable_operator \n";}
  | FALSE  {cout<<"overloadable_operator \n";}
  |  STAR    {cout<<"overloadable_operator \n";}
  |  DIV    {cout<<"overloadable_operator \n";}
  |  PERCENT    {cout<<"overloadable_operator \n";}
  | AND   {cout<<"overloadable_operator \n";}
  |  OR    {cout<<"overloadable_operator \n";}
  |  CHAPO   {cout<<"overloadable_operator \n";} 
  | LTLT   {cout<<"overloadable_operator \n";}
  | GTGT   {cout<<"overloadable_operator \n";}
  | EQEQ   {cout<<"overloadable_operator \n";}
  | NOTEQ   {cout<<"overloadable_operator \n";}
  | MORE_THAN   {cout<<"overloadable_operator \n";}
  |  LESS_THAN   {cout<<"overloadable_operator \n";}
  | GEQ   {cout<<"overloadable_operator \n";}
  | LEQ  {cout<<"overloadable_operator \n";}
  ; 
conversion_operator_declarator
  : IMPLICIT OPERATOR type  LEFTB2  type IDENTIFIER  RIGHTB2   {cout<<"conversion_operator_declarator \n";}
  | EXPLICIT OPERATOR type  LEFTB2  type IDENTIFIER  RIGHTB2   {cout<<"conversion_operator_declarator \n";}
  | IMPLICIT OPERATOR qualifier IDENTIFIER  LEFTB2  qualifier IDENTIFIER IDENTIFIER  RIGHTB2   {cout<<"conversion_operator_declarator \n";}
  | EXPLICIT OPERATOR qualifier IDENTIFIER  LEFTB2  qualifier IDENTIFIER IDENTIFIER  RIGHTB2   {cout<<"conversion_operator_declarator \n";}
  | IMPLICIT OPERATOR IDENTIFIER  LEFTB2  IDENTIFIER IDENTIFIER  RIGHTB2   {cout<<"conversion_operator_declarator \n";}
  | EXPLICIT OPERATOR IDENTIFIER  LEFTB2  IDENTIFIER IDENTIFIER  RIGHTB2   {cout<<"conversion_operator_declarator \n";}
  ;
constructor_declaration
  : attributes_opt modifiers_opt constructor_declarator constructor_body  {cout<<"constructor_declaration \n";}
  ;
constructor_declarator
  : IDENTIFIER  %prec"then"  {cout<<"constructor_declarator \n";int x  = strlen($<r.str>1) ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno>1,"missing \( \)","");}
  | IDENTIFIER LEFTB2  formal_parameter_list_opt  RIGHTB2  constructor_initializer_opt  {cout<<"constructor_declarator1 \n";}
  | badid  LEFTB2  formal_parameter_list_opt  RIGHTB2  constructor_initializer_opt  {cout<<"constructor_declarator \n";}
  | badid    {int x  = strlen($<r.str>1) ; err->errQ->enqueue($<r.myLineNo>1,$<r.myColno>1,"missing \( \)","");} {cout<<"constructor_declarator \n";}
  ;
constructor_initializer_opt
  : /* Nothing */  {cout<<"constructor_initializer_opt nothing \n";}
  | constructor_initializer  {cout<<"constructor_initializer_opt \n";}
  ;
constructor_initializer
  :  TWO_DOT BASE  LEFTB2  argument_list_opt  RIGHTB2   {cout<<"constructor_initializer \n";}
  |  TWO_DOT THIS  LEFTB2  argument_list_opt  RIGHTB2   {cout<<"constructor_initializer \n";}
  ;
/* Widen from unsafe_opt STATIC to modifiers_opt */
/* This is now subsumed by constructor_declaration - delete
 * static_constructor_declaration
 *  : attributes_opt modifiers_opt IDENTIFIER  LEFTB2   RIGHTB2  block
 *  ;
 */
/* No longer needed after modification of static_constructor_declaration
 * unsafe_opt
 * : 
 * | UNSAFE
 * ;
 */
/* Widen from unsafe_opt to modifiers_opt */
destructor_declaration
  : attributes_opt modifiers_opt  THAL  IDENTIFIER  LEFTB2   RIGHTB2  block  {cout<<"destructor_declaration \n";}
  ;
operator_body
  : block  {cout<<"operator_body \n";}
  |  FA9LE   {cout<<"operator_body \n";}
  ;
constructor_body /*** Added by JP - same as method_body ***/
  : block  {cout<<"constructor_body \n";}
  |  FA9LE   {cout<<"constructor_body \n";}
  ;

/***** C.2.7 Structs *****/
struct_declaration
  : attributes_opt modifiers_opt STRUCT IDENTIFIER struct_interfaces_opt struct_body comma_opt  {cout<<"struct_declaration \n";}
  ;
struct_interfaces_opt
  : /* Nothing */  {cout<<"struct_interfaces_opt nothing \n";}
  | struct_interfaces  {cout<<"struct_interfaces_opt \n";}
  ;
struct_interfaces
  :  TWO_DOT interface_type_list  {cout<<"struct_interfaces \n";}
  ;
struct_body
  :  LEFTB1 struct_member_declarations_opt  RIGHTB1  {cout<<"struct_body \n";}
  ;
struct_member_declarations_opt
  : /* Nothing */  {cout<<"struct_member_declarations_opt nothing  \n";}
  | struct_member_declarations  {cout<<"struct_member_declarations_opt \n";}
  ;
struct_member_declarations
  : struct_member_declaration  {cout<<"struct_member_declarations \n";}
  | struct_member_declarations struct_member_declaration  {cout<<"struct_member_declarations \n";}
  ;
struct_member_declaration
  : constant_declaration  {cout<<"struct_member_declaration \n";}
  | field_declaration  {cout<<"struct_member_declaration \n";}
  | method_declaration  {cout<<"struct_member_declaration \n";}
  | property_declaration  {cout<<"struct_member_declaration \n";}
  | event_declaration  {cout<<"struct_member_declaration \n";}
  | indexer_declaration  {cout<<"struct_member_declaration \n";}
  | operator_declaration  {cout<<"struct_member_declaration \n";}
  | constructor_declaration  {cout<<"struct_member_declaration \n";}
/*  | static_constructor_declaration */
  | type_declaration  {cout<<"struct_member_declaration \n";}
  ;

/***** C.2.8 Arrays *****/
array_initializer
  :  LEFTB1 variable_initializer_list_opt  RIGHTB1  {cout<<"array_initializer \n";}
  |  LEFTB1 variable_initializer_list COMMA  RIGHTB1  {cout<<"array_initializer \n";}
  ;
variable_initializer_list_opt
  : /* Nothing */  {cout<<"variable_initializer_list_opt nothing  \n";}
  | variable_initializer_list  {cout<<"variable_initializer_list_opt \n";}
  ;
variable_initializer_list
  : variable_initializer  {cout<<"variable_initializer_list \n";}
  | variable_initializer_list COMMA variable_initializer  {cout<<"variable_initializer_list \n";}
  ;

/***** C.2.9 Interfaces *****/
interface_declaration
  : attributes_opt modifiers_opt INTERFACE IDENTIFIER interface_base_opt interface_body comma_opt  {cout<<"interface_declaration \n";}
  ;
interface_base_opt
  : /* Nothing */  {cout<<"interface_base_opt nothing \n";}
  | interface_base  {cout<<"interface_base_opt \n";}
  ;
interface_base
  :  TWO_DOT interface_type_list  {cout<<"interface_base \n";}
  ;
interface_body
  :  LEFTB1 interface_member_declarations_opt  RIGHTB1  {cout<<"interface_body \n";}
  ;
interface_member_declarations_opt
  : /* Nothing */  {cout<<"interface_member_declarations_opt nothing \n";}
  | interface_member_declarations  {cout<<"interface_member_declarations_opt \n";}
  ;
interface_member_declarations
  : interface_member_declaration  {cout<<"interface_member_declarations \n";}
  | interface_member_declarations interface_member_declaration  {cout<<"interface_member_declarations \n";}
  ;
interface_member_declaration
  : interface_method_declaration  {cout<<"interface_member_declaration \n";}
  | interface_property_declaration  {cout<<"interface_member_declaration \n";}
  | interface_event_declaration  {cout<<"interface_member_declaration \n";}
  | interface_indexer_declaration  {cout<<"interface_member_declaration \n";}
  ;
/* inline return_type to avoid conflict with interface_property_declaration */
interface_method_declaration 
  : attributes_opt new_opt type IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2  interface_empty_body  {cout<<"interface_method_declaration \n";}
  | attributes_opt new_opt qualifier IDENTIFIER IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2  interface_empty_body  {cout<<"interface_method_declaration \n";}
  | attributes_opt new_opt IDENTIFIER IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2  interface_empty_body  {cout<<"interface_method_declaration \n";}
  | attributes_opt new_opt VOID IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2  interface_empty_body  {cout<<"interface_method_declaration \n";}
  ;
new_opt
  : /* Nothing */  {cout<<"new_opt nothing \n";}
  | NEW  {cout<<"new_opt \n";}
  ;
interface_property_declaration  
  : attributes_opt new_opt type IDENTIFIER 
      ENTER_getset
     LEFTB1 interface_accessors  RIGHTB1
      EXIT_getset  {cout<<"interface_property_declaration \n";}
  | attributes_opt new_opt qualifier IDENTIFIER IDENTIFIER 
      ENTER_getset
     LEFTB1 interface_accessors  RIGHTB1
      EXIT_getset  {cout<<"interface_property_declaration \n";}
  | attributes_opt new_opt IDENTIFIER IDENTIFIER 
      ENTER_getset
     LEFTB1 interface_accessors  RIGHTB1
      EXIT_getset  {cout<<"interface_property_declaration \n";}
  ;
interface_indexer_declaration 
  : attributes_opt new_opt type THIS 
    LEFT_BRACKET formal_parameter_list RIGHT_BRACKET 
      ENTER_getset
     LEFTB1 interface_accessors  RIGHTB1
      EXIT_getset  {cout<<"interface_indexer_declaration \n";}
  | attributes_opt new_opt qualifier IDENTIFIER THIS 
    LEFT_BRACKET formal_parameter_list RIGHT_BRACKET 
      ENTER_getset
     LEFTB1 interface_accessors  RIGHTB1
      EXIT_getset  {cout<<"interface_indexer_declaration \n";}
  | attributes_opt new_opt IDENTIFIER THIS 
    LEFT_BRACKET formal_parameter_list RIGHT_BRACKET 
      ENTER_getset
     LEFTB1 interface_accessors  RIGHTB1
      EXIT_getset  {cout<<"interface_indexer_declaration \n";}
  ;

interface_accessors
  : attributes_opt GET interface_empty_body  {cout<<"interface_accessors \n";}
  | attributes_opt SET interface_empty_body  {cout<<"interface_accessors \n";}
  | attributes_opt GET interface_empty_body attributes_opt SET interface_empty_body  {cout<<"interface_accessors \n";}
  | attributes_opt SET interface_empty_body attributes_opt GET interface_empty_body  {cout<<"interface_accessors \n";}
  ;
interface_event_declaration 
  : attributes_opt new_opt EVENT type IDENTIFIER interface_empty_body  {cout<<"interface_event_declaration \n";}
  | attributes_opt new_opt EVENT qualifier IDENTIFIER IDENTIFIER interface_empty_body  {cout<<"interface_event_declaration \n";}
  | attributes_opt new_opt EVENT IDENTIFIER IDENTIFIER interface_empty_body  {cout<<"interface_event_declaration \n";}
  ;

/* mono seems to allow this */
interface_empty_body
  :  FA9LE   {cout<<"interface_empty_body \n";}
  |  LEFTB1  RIGHTB1  {cout<<"interface_empty_body \n";}
  ;

/***** C.2.10 Enums *****/
enum_declaration
  : attributes_opt modifiers_opt ENUM IDENTIFIER enum_base_opt enum_body comma_opt  {cout<<"enum_declaration \n";}
  ;
enum_base_opt
  : /* Nothing */  {cout<<"enum_base_opt nothing \n";}
  | enum_base  {cout<<"enum_base_opt \n";}
  ;
enum_base
  :  TWO_DOT integral_type  {cout<<"enum_base \n";}
  ;
enum_body
  :  LEFTB1 enum_member_declarations_opt  RIGHTB1  {cout<<"enum_body \n";}
  |  LEFTB1 enum_member_declarations COMMA  RIGHTB1  {cout<<"enum_body \n";}
  ;
enum_member_declarations_opt
  : /* Nothing */  {cout<<"enum_member_declarations_opt nothing \n";}
  | enum_member_declarations  {cout<<"enum_member_declarations_opt \n";}
  ;
enum_member_declarations
  : enum_member_declaration  {cout<<"enum_member_declarations \n";}
  | enum_member_declarations COMMA enum_member_declaration  {cout<<"enum_member_declarations \n";}
  ;
enum_member_declaration
  : attributes_opt IDENTIFIER  {cout<<"enum_member_declaration \n";}
  | attributes_opt IDENTIFIER  EQUAL  constant_expression  {cout<<"enum_member_declaration \n";}
  ;

/***** C.2.11 Delegates *****/
delegate_declaration
  : attributes_opt modifiers_opt DELEGATE return_type IDENTIFIER  LEFTB2  formal_parameter_list_opt  RIGHTB2   FA9LE   {cout<<"delegate_declaration \n";}
  ;

/***** C.2.12 Attributes *****/
attributes
  : attribute_sections  {cout<<"attributes \n";}
  ;
attribute_sections
  : attribute_section  {cout<<"attribute_sections \n";}
  | attribute_sections attribute_section  {cout<<"attribute_sections \n";}
  ;
attribute_section
  : ENTER_attrib LEFT_BRACKET attribute_target_specifier_opt attribute_list RIGHT_BRACKET EXIT_attrib  {cout<<"attribute_section \n";}
  | ENTER_attrib LEFT_BRACKET attribute_target_specifier_opt attribute_list COMMA RIGHT_BRACKET EXIT_attrib  {cout<<"attribute_section \n";}
  ;
attribute_target_specifier_opt
  : /* Nothing */  {cout<<"attribute_target_specifier_opt nothing \n";}
  | attribute_target_specifier  {cout<<"attribute_target_specifier_opt \n";}
  ;
attribute_target_specifier
  : attribute_target  TWO_DOT  {cout<<"attribute_target_specifier \n";}
  ;
attribute_target
  : ASSEMBLY  {cout<<"attribute_target \n";}
  | FIELD  {cout<<"attribute_target \n";}
  | EVENT  {cout<<"attribute_target \n";}
  | METHOD  {cout<<"attribute_target \n";}
  | MODULE  {cout<<"attribute_target \n";}
  | PARAM  {cout<<"attribute_target \n";}
  | PROPERTY  {cout<<"attribute_target \n";}
  | RETURN  {cout<<"attribute_target \n";}
  | TYPE  {cout<<"attribute_target \n";}
  ;
attribute_list
  : attribute  {cout<<"attribute_list \n";}
  | attribute_list COMMA attribute  {cout<<"attribute_list \n";}
  ;
attribute
  : attribute_name attribute_arguments_opt  {cout<<"attribute \n";}
  ;
attribute_arguments_opt
  : /* Nothing */  {cout<<"attribute_arguments_opt nothing \n";}
  | attribute_arguments  {cout<<"attribute_arguments_opt \n";}
  ;
attribute_name
  :  qualifier IDENTIFIER   {cout<<"attribute_name \n";}
  | IDENTIFIER  {cout<<"attribute_name \n";}
  ;
attribute_arguments
  :  LEFTB2  expression_list_opt  RIGHTB2  {cout<<"attribute_arguments \n";} 
  ;



/** Dummy rules for those context-sensitive "keywords" **/
ENTER_attrib 
  : { ; }
  ;
EXIT_attrib 
  : { ; }
  ;
ENTER_accessor_decl 
  : { ; }
  ;
EXIT_accessor_decl
  : { ; }
  ;
ENTER_getset
  : { ; }
  ;
EXIT_getset
  : { ; }
  ;


%%

void yyerror(char *s) 
{
	;
}

int yylex()
{
	return lexer->yylex();
}

void main(void)
{
	freopen("in.cs","r",stdin);
	freopen("out.txt","w",stdout);
	
	Parser* p = new Parser();
	p->parse();
	err->printErrQueue();
	_fcloseall();
}
