/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_Y_TAB_H_INCLUDED
# define YY_YY_Y_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    STRING = 258,
    COMMENT = 259,
    OP_MULT = 260,
    OP_OP = 261,
    OP_CP = 262,
    OP_DBLMULT = 263,
    OP_CC = 264,
    OP_COMMA = 265,
    OP_PLUS = 266,
    OP_MINUS = 267,
    OP_DIV = 268,
    OP_OC = 269,
    KW_SET = 270,
    KW_DEFFUN = 271,
    KW_FOR = 272,
    KW_IF = 273,
    KW_EXIT = 274,
    KW_LOAD = 275,
    KW_DISP = 276,
    KW_TRUE = 277,
    KW_FALSE = 278,
    KW_AND = 279,
    KW_OR = 280,
    KW_NOT = 281,
    KW_EQUAL = 282,
    KW_LESS = 283,
    KW_NIL = 284,
    KW_LIST = 285,
    KW_APPEND = 286,
    KW_CONCAT = 287,
    NEWLINE = 288,
    VALUE = 289,
    ID = 290
  };
#endif
/* Tokens.  */
#define STRING 258
#define COMMENT 259
#define OP_MULT 260
#define OP_OP 261
#define OP_CP 262
#define OP_DBLMULT 263
#define OP_CC 264
#define OP_COMMA 265
#define OP_PLUS 266
#define OP_MINUS 267
#define OP_DIV 268
#define OP_OC 269
#define KW_SET 270
#define KW_DEFFUN 271
#define KW_FOR 272
#define KW_IF 273
#define KW_EXIT 274
#define KW_LOAD 275
#define KW_DISP 276
#define KW_TRUE 277
#define KW_FALSE 278
#define KW_AND 279
#define KW_OR 280
#define KW_NOT 281
#define KW_EQUAL 282
#define KW_LESS 283
#define KW_NIL 284
#define KW_LIST 285
#define KW_APPEND 286
#define KW_CONCAT 287
#define NEWLINE 288
#define VALUE 289
#define ID 290

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 19 "gpp_interpreter.y"

    int num;
    int *nums;
    char id[20];

#line 133 "y.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
