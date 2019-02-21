/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

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
    GE = 258,
    LE = 259,
    EQ = 260,
    NE = 261,
    FALSE = 262,
    TRUE = 263,
    IF = 264,
    ELSE = 265,
    WHILE = 266,
    PRINT = 267,
    BOOL_TYPE = 268,
    INT_TYPE = 269,
    INT_ARR = 270,
    BOL_ARR = 271,
    ID = 272,
    VAL = 273,
    IF_ALONE = 274
  };
#endif
/* Tokens.  */
#define GE 258
#define LE 259
#define EQ 260
#define NE 261
#define FALSE 262
#define TRUE 263
#define IF 264
#define ELSE 265
#define WHILE 266
#define PRINT 267
#define BOOL_TYPE 268
#define INT_TYPE 269
#define INT_ARR 270
#define BOL_ARR 271
#define ID 272
#define VAL 273
#define IF_ALONE 274

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 24 "parser.y" /* yacc.c:1909  */

  int value;
  size_t id;
  struct expr *expr;
  enum value_type {
    ERROR   = -1,
    UNTYPED = 0,
    INTEGER = 1,
    BOOLEAN = 2,
    INT_ARR = 3,
    BOL_ARR = 4,
  } type;
  struct stmt *stmt;

#line 107 "y.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void *module, void *builder);

#endif /* !YY_YY_Y_TAB_H_INCLUDED  */
