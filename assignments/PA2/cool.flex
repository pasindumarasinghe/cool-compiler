/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

 /*
 *  Add Your own definitions here
 */
int comment_depth;
int str_length;


%}

%option noyywrap

 /*
 *	Start conditions
 */

%x 							single_comment
%x							multi_comment
%x 							string
%x 							hold_string_buffer


 /*
 * Define names for regular expressions here.
 */

 /*
 *	Whitespaces
 */
NEWLINE 					[\n]
BLANK_CHAR					[ \f\r\t\v]+

 /*
 *	Regex for keywords
 */

CLASS 						(?i:class)
ELSE 						(?i:else)
IF 							(?i:if)
IN 							(?i:in)
INHERITS 					(?i:inherits)
LET 						(?i:let)
LOOP 						(?i:loop)
POOL 						(?i:pool) 				
THEN 						(?i:then)
WHILE 						(?i:while)
CASE 						(?i:case)
ESAC 						(?i:esac)
OF 							(?i:of)
NEW 						(?i:new)
ISVOID 						(?i:isvoid)
NOT 						(?i:not)
BOOL_TRUE 					t(?i:rue)
BOOL_FALSE 					f(?i:alse)

DARROW          			=>
ASSIGN						<-
LE 							<=
SINGLE_COMMENT_BEGIN		--
MULTI_COMMENT_BEGIN			"(\*"
MULTI_COMMENT_END 			"\*)"
STR_BEGIN					\"
STR_END 					\"



DIGIT						[0-9]
INTEGER						{DIGIT}+
LETTER 						[a-zA-Z]
TYPEID 						[A-Z][a-zA-Z0-9_]*
OBJECTID 					[a-z][a-zA-Z0-9_]*
%%

 /*
  * Nested comments
  */


 /*
  * The multiple-character operators.
  */
{DARROW}					{ return (DARROW); }
{ASSIGN}					{ return (ASSIGN); }
{LE}						{ return (LE); }

 /*
 * The single-character operators
 */

";"							{ return ';'; }
":"							{ return ':'; }
"("							{ return '('; }
")"							{ return ')'; }
"{"							{ return '{'; }
"}"							{ return '}'; }
"+"							{ return '+'; }
"-"							{ return '-'; }
"*"							{ return '*'; }
"/"							{ return '/'; }
"~"							{ return '~'; }
"<"							{ return '<'; }
"="							{ return '='; }
"@"							{ return '@'; }




 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

{CLASS} 					{ return (CLASS); }
{ELSE}						{ return (ELSE); }
{IF}						{ return (IF); }
{IN}						{ return (IN); }
{INHERITS}					{ return (INHERITS); }
{LET}						{ return (LET); }
{LOOP}	 					{ return (LOOP); }
{POOL}						{ return (POOL); }
{THEN}						{ return (THEN); }
{WHILE}	 					{ return (WHILE); }
{CASE}	 					{ return (CASE); }
{ESAC}	 					{ return (ESAC); }
{OF}						{ return (OF); }
{NEW}						{ return (NEW); }
{ISVOID}					{ return (ISVOID); }
{NOT}						{ return (NOT); }
{BOOL_TRUE}					{ 
								cool_yylval.boolean = true;
								return BOOL_CONST;
							}
{BOOL_FALSE}				{ 
								cool_yylval.boolean = false;
							return BOOL_CONST; 
							}



{NEWLINE} 					{ curr_lineno++; }
{BLANK_CHAR} 				{}





 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

 /*
 * Comments
 */

 /* single line comments */

<INITIAL>{SINGLE_COMMENT_BEGIN}	{ BEGIN(single_comment); }
	
<single_comment>{
	. {}
	\n 	{ curr_lineno++; BEGIN(INITIAL);}
}

 /* multiline comments */
<INITIAL>{MULTI_COMMENT_BEGIN} { comment_depth++; BEGIN(multi_comment);}	

<multi_comment>{
	{MULTI_COMMENT_BEGIN}	{ comment_depth++; }
	{MULTI_COMMENT_END}		{ 
		comment_depth--;
		if(comment_depth == 0) BEGIN(INITIAL);
	}

	<<EOF>>	{
		cool_yylval.error_msg = "EOF in comment";
		BEGIN(INITIAL);
		return(ERROR);
	}

	<multi_comment>\n 	{
		curr_lineno++;
	}

	<multi_comment>.



}


{MULTI_COMMENT_END}	{
	cool_yylval.error_msg = "Unmatched *)";
	return(ERROR);
}


 /*
 * String constants
 */

{STR_BEGIN}		{ BEGIN(string); }

<string>{
	{STR_END}	{ BEGIN(INITIAL); }
	
	
	\n 		{
		curr_lineno++;
		cool_yylval.error_msg = "Unterminated string constant";
		BEGIN(INITIAL);
		return(ERROR);
	}

	\0		{
		string_buf[str_length] = '0';
		str_length++;
		cool_yylval.error_msg = "String contains null character";
		return(ERROR);
	}

	\\n 	{

		if(str_length+1 >= MAX_STR_CONST){
			cool_yylval.error_msg = "String constant too long";
			str_length = 0;
			BEGIN(hold_string_buffer);
			return(ERROR);
		} else {
			string_buf[str_length] = '\n';
			str_length++;
			curr_lineno++; 
		}

	}
	\\b 	{

		if(str_length+1 >= MAX_STR_CONST){
			cool_yylval.error_msg = "String constant too long";
			str_length = 0;
			BEGIN(hold_string_buffer);
			return(ERROR);
		} else {
			string_buf[str_length] = '\b';
			str_length++;
		}

	}

	\\t 	{

		if(str_length+1 >= MAX_STR_CONST){
			cool_yylval.error_msg = "String constant too long";
			str_length = 0;
			BEGIN(hold_string_buffer);
			return(ERROR);
		} else{
			string_buf[str_length] = '\t';
			str_length++;
		}

	}				

	<<EOF>>	{
		cool_yylval.error_msg = "EOF in string constant";
		BEGIN(INITIAL);
		return(ERROR);
	}
}


 /*
 * Integers
 */

{INTEGER} 					{
								cool_yylval.symbol = inttable.add_string(yytext);
								return(INT_CONST);
							}		

 /*
 * Identifiers
 */

{TYPEID}					{
								cool_yylval.symbol = idtable.add_string(yytext);
								return(TYPEID);
							}	

{OBJECTID}					{
								cool_yylval.symbol = idtable.add_string(yytext);
								return(OBJECTID);
							}




 /*
 * When no rule matches
 */

. {
	cool_yylval.error_msg = yytext;
	return(ERROR);
}

%%
