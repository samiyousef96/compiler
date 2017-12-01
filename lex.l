%{
	#include "yacc.hpp"
	#include <iostream>
	#include <istream>
	#include <ostream>
	#include <string>		
	#include <FlexLexer.h>	

	int lineNo = 1;
	int colNo = 1;
	int brackets = 0 ;
%}
%option c++
%option noyywrap

%x multilinecomment

		
Single_line_comment		(\/\/)+[^\n]*
Integer					([+-]?[1-9][0-9]*)|([+-]?0)
Float					([+-]?[0-9][1-9]*"."[0-9]+["f""F"])|([+-]?[0-9][1-9]*["f""F"]?)       
String					[\"][a-zA-Z _ 0-9 \\ \" ? ! < > @ # $ % ^ & * ( )  _  \/ + \- = | { } ' : ; . ]+[\"]
Char					['][a-zA-Z _ 0-9][']
Double					([+-]?[0-9][1-9]*"."[0-9]+["d""D"]?)|([+-]?[0-9][1-9]*["d""D"]?)
Bool					("true")|("false")
BADID					[0-9\\\"?!@#$%^&*\/+\-|{}':.~][a-zA-Z0-9_]*
Identifier              [a-zA-Z_][a-zA-Z0-9_]*

%% 
"="						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  EQUAL;
						}

"++"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  PLUSPLUS;
						}

"--"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  MINUSMINUS;
						}
":"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  TWO_DOT;
						}
"%"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  PERCENT;
						}
"^"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  CHAPO;
						}
"if"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  IF;

						}
"else"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  ELSE;	
						}
						
"string"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  STRING;
						}

"for"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FOR;
						}

"class"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);

							return  CLASS;
						}
"extends"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  EXTENDS;
						}
						
";"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FA9LE;
						}
"break"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  BREAK;
						}

"{"						{
							colNo += strlen(yytext);
							brackets  += 1;
							yylval.r.brackets = brackets;
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str=NULL;
										yylval.r.str = yytext;
										 
							return  LEFTB1;
						}
"}"						{
							colNo += strlen(yytext);
							brackets  -= 1;
							yylval.r.brackets = brackets;
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str=NULL;
										yylval.r.str = yytext;
										 
							return  RIGHTB1;
						}
"("						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  LEFTB2;
						}
")"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  RIGHTB2;
						}
"["						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  LEFT_BRACKET;
						}
"]"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  RIGHT_BRACKET;
						}
"."						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  DOT;
						}
"&"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  AND;
						}
"|"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  OR;
						}
"!"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  T3GOB;
						}
"?"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  QUESTION_MARK;
						}
"~"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  THAL;
						}
">"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  MORE_THAN;
						}
"<"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  LESS_THAN;
						}
"public"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  PUBLIC;
						}
						
"private"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  PRIVATE;
						}

"int"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  INT;
						}

"float"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FLOAT;
						}

"double"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  DOUBLE;
						}

\n						{
							colNo=1;
							lineNo++;
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
						}

" "						{
							            colNo += 1;
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
						}
						
"\t"					{
							colNo += 6; 
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
						}

"+"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return PLUS;
						}
"-"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return MINUS;
						}
						
"*"						{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return STAR;
						}
						
"/"						{
							colNo += strlen(yytext);  
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return DIV;
						}

"using"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  USING;
						}
"true"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  TRUE;
						}
"false"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FALSE;
						}
"null"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  NULL_LITERAL;
						}
"return"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  RETURN;
						}
"finally"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FINALLY;
						}
"bool"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  BOOL;
						}
"char"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  CHAR;
						}
"continue"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  CONTINUE;
						}
"const"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  CONST;
						}
"default"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  DEFAULT;
						}
"do"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  DO;
						}
"foreach"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FOREACH;
						}
"from"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  FROM;
						}
"global"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  GLOBAL;
						}
"goto"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  GOTO;
						}
"is"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  IS;
						}
"lock"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  LOCK;
						}
"long"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  LONG;
						}
"new"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  NEW;
						}
"object"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  OBJECT;
						}
"out"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  OUT;
						}
"ref"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  REF;
						}
"short"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  SHORT;
						}
"sizeof"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  SIZEOF;
						}
"stackalloc"			{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  STACKALLOC;
						}
"switch"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  SWITCH;
						}
"this"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  THIS;
						}
"byte"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  BYTE;
						}
"abstract"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  ABSTRACT;
						}

						
"protected"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  PROTECTED;
						}
"static"				{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  STATIC;
						}

"async"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  ASYNC;
						}
"try"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  TRY;
						}
						
"catch"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  CATCH;
						}
"case"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  CASE;
						}
"uint"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  UINT;
						}
"ulong"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  ULONG;
						}
"ushort"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  USHORT;
						}
"unsafe"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  UNSAFE;
						}
"unchecked"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  UNCHECKED;
						}
						
"var"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  VAR;
						}

"void"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  VOID;
						}
"while"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  WHILE;
						}

"volatile"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  VOLATILE;
						}
"virtual"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  VIRTUAL;
						}
"sealed"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  SEALED;
						}
"readonly"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  READONLY;
						}
"override"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  OVERRIDE;
						}
"internal"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  INTERNAL;
						}
"extern"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  EXTERN;
						}
"fixed"				{
							colNo +=strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return FIXED;
						}

"yield"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  YIELD;
						}

"namespace"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  NAMESPACE;
						}

","					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  COMMA;
						}

"get"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  GET;
						}

"set"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  SET;
						}

"event"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  EVENT;
						}

"add"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  ADD;
						}

"remove"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  REMOVE;
						}

"operator"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  OPERATOR;
						}

"implicit"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  IMPLICIT;
						}

"explicit"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  EXPLICIT;
						}

"base"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  BASE;
						}

"struct"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  STRUCT;
						}

"interface"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  INTERFACE;
						}

"enum"					{
							colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										 
							return  ENUM;
						}




\/\*                                        { BEGIN multilinecomment; }
<multilinecomment>[^*\n]*					{;}
<multilinecomment>"*"+[^*/\n]*				{lineNo++;}
<multilinecomment>"*"+"/"                   { BEGIN INITIAL;cout<<"\t it is a Multi Line Comment \n";}

{Single_line_comment}				{ 
										lineNo++;
									}

{Integer}							{
										colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										int x=atoi(yytext);
										if(strlen(yytext)<=4)
											{
												yylval.r.i=x;
												return INTEGER_LITERAL;
											}
										else{
												yylval.r.i=x;
												return INTEGER_LITERAL;
											}
										
									}
{Float}								{
										colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.f = atof(yytext) ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										return FLOAT_LITERAL;
										
									}
									
{Double}								{
										colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.d = strtod(yytext,NULL) ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										return DOUBLE_LITERAL;
									}
									
{String}							{
										int i;
										colNo += strlen(yytext);
										int x = strlen(yytext);
										char *c = new char [x-2];
										for(i = 0 ; i < x-2 ; i ++)
										{
												c[i]=yytext[i+1];
										}
										c[i]='\0';
										
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,c);
										return STRING_LITERAL;
									}
{Char}								{
										colNo += strlen(yytext);
										char c = yytext[1];
										yylval.r.c = c;
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										yylval.r.str[0]=c;
										return CHARACTER_LITERAL;
									}
{BADID}								{	colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										return  BADID;
									}

{Identifier}						{ colNo += strlen(yytext);
										yylval.r.myColno = colNo ;
										yylval.r.myLineNo = lineNo ;
										yylval.r.str = new char[256];
										yylval.r.str[0]=NULL;
										strcat(yylval.r.str,yytext);
										return  IDENTIFIER;
									}

											

%%
