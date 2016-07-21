	
/*
 [The "BSD licence"]
 Copyright (c) 2014 Leonardo Lucena
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
   Derived from http://www.scala-lang.org/files/archive/spec/2.11/13-syntax-summary.html
 */

grammar ScalaLang;

program           : blockExpr
                  | compilationUnit ;

literal           : '-'? IntegerLiteral
                  | '-'? FloatingPointLiteral
                  | BooleanLiteral
                  | CharacterLiteral
                  | StringLiteral
                  | SymbolLiteral
                  | 'null' ; 
                  
qualId            : id ('.' id)* ;

ids               : id (WHITE_SPACE_IN_LINE*  ',' WHITE_SPACE_IN_LINE* id)* ;

path              :  stableId
                  |  (id '.')? 'this' ;

stableId          :  id stableId1
                  |	 id '.' 'this' '.' id stableId1
                  |	 'this' '.' id stableId1
                  |	 id '.' 'super' classQualifier '.' id stableId1
                  |	 'super' classQualifier '.' id stableId1
                  |	 id '.' 'super' '.' id stableId1
                  |	 'super' '.' id stableId1
                  |	 id '.' 'this' '.' id
                  |	 'this' '.' id
                  |	 id '.' 'super' classQualifier '.' id
                  |	 'super' classQualifier '.' id
                  |	 id '.' 'super' '.' id
                  |	 'super' '.' id
                  |  id ;

stableId1         :  '.' id stableId1
                  |  '.' id ;

classQualifier    : '[' id ']' ;

type              : functionArgTypes WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE* type
                  | infixType WHITE_SPACE_IN_LINE* existentialClause
                  | infixType WHITE_SPACE_IN_LINE*;

functionArgTypes  : infixType
                  | '(' WHITE_SPACE_IN_LINE* ( paramType (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* paramType )* )? WHITE_SPACE_IN_LINE* ')' ;

existentialClause : 'forSome' WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* existentialDcl (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* existentialDcl)* WHITE_SPACE_IN_LINE* '}';

existentialDcl    : 'type' WHITE_SPACE_IN_LINE+ typeDcl
                  | 'val' WHITE_SPACE_IN_LINE+ valDcl;

infixType         : compoundType ( WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* compoundType)*;

compoundType      : annotType (WHITE_SPACE_IN_LINE+ 'with' WHITE_SPACE_IN_LINE+ annotType)* WHITE_SPACE_IN_LINE* refinement?
                  | refinement;

annotType         : simpleType WHITE_SPACE_IN_LINE* annotation*;

simpleType        : simpleType WHITE_SPACE_IN_LINE* typeArgs
                  | simpleType '#' id
                  | stableId
                  | (stableId | (id '.')? 'this') '.' 'type'
                  | '(' WHITE_SPACE_IN_LINE* types WHITE_SPACE_IN_LINE* ')';

typeArgs          : '[' WHITE_SPACE_IN_LINE* types WHITE_SPACE_IN_LINE* ']';

types             : type (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* type)*;

refinement        : Nl? '{' WHITE_SPACE_IN_LINE* refineStat (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* refineStat)* WHITE_SPACE_IN_LINE* '}';

refineStat        : dcl
                  | 'type' WHITE_SPACE_IN_LINE+ typeDef
                  | ;

typePat           : type;

ascription        : ':' WHITE_SPACE_IN_LINE* infixType
                  | ':' WHITE_SPACE_IN_LINE* annotation+
                  | ':' '_' '*';

expr              : (bindings | ('implicit' WHITE_SPACE_IN_LINE+|WHITE_SPACE_IN_LINE*) id | '_') WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE* expr
                  | expr1 ;

expr1             : 'if' WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* expr WHITE_SPACE_IN_LINE* ')' WHITE_SPACE_IN_LINE* Nl* WHITE_SPACE_IN_LINE* expr (WHITE_SPACE_IN_LINE* semi? WHITE_SPACE_IN_LINE* 'else' WHITE_SPACE_IN_LINE* expr)?
                  | 'while' WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* expr WHITE_SPACE_IN_LINE* ')' WHITE_SPACE_IN_LINE* Nl* WHITE_SPACE_IN_LINE* expr
                  | 'try' (WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* block WHITE_SPACE_IN_LINE* '}' WHITE_SPACE_IN_LINE* | WHITE_SPACE_IN_LINE* expr) ( WHITE_SPACE_IN_LINE*'catch' WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* caseClauses WHITE_SPACE_IN_LINE* '}')? (WHITE_SPACE_IN_LINE* 'finally' WHITE_SPACE_IN_LINE* expr)?
                  | 'do' WHITE_SPACE_IN_LINE* expr WHITE_SPACE_IN_LINE* semi? WHITE_SPACE_IN_LINE* 'while' WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* expr WHITE_SPACE_IN_LINE* ')'
                  | 'for' WHITE_SPACE_IN_LINE* ('(' WHITE_SPACE_IN_LINE* enumerators WHITE_SPACE_IN_LINE* ')' | '{' WHITE_SPACE_IN_LINE* enumerators WHITE_SPACE_IN_LINE* '}') WHITE_SPACE_IN_LINE* Nl* WHITE_SPACE_IN_LINE* 'yield'? WHITE_SPACE_IN_LINE* expr
                  | 'throw' WHITE_SPACE_IN_LINE* expr
                  | 'return' WHITE_SPACE_IN_LINE* expr?
                  | (('new' WHITE_SPACE_IN_LINE* (classTemplate | templateBody)| blockExpr | simpleExpr1  '_'?) '.')  id WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr
                  | simpleExpr1 WHITE_SPACE_IN_LINE* argumentExprs WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr
                  | postfixExpr
                  | postfixExpr WHITE_SPACE_IN_LINE* ascription
                  | postfixExpr WHITE_SPACE_IN_LINE* 'match' WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* caseClauses WHITE_SPACE_IN_LINE* '}' ;

postfixExpr       : infixExpr (WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* Nl?)? ;

infixExpr         : prefixExpr
                  | infixExpr WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* infixExpr ;

prefixExpr        : ('-' | '+' | '~' | '!')? WHITE_SPACE_IN_LINE*
                    ('new' WHITE_SPACE_IN_LINE+ (classTemplate | templateBody)| blockExpr | simpleExpr1 '_'?) ;

simpleExpr1       :	literal WHITE_SPACE_IN_LINE* simpleExpr2
                  |	literal
                  |	path WHITE_SPACE_IN_LINE* simpleExpr2
                  |	path
                  |	'_' WHITE_SPACE_IN_LINE* simpleExpr2
                  |	'_'
                  |	'(' WHITE_SPACE_IN_LINE* ')' WHITE_SPACE_IN_LINE* simpleExpr2
                  |	'(' WHITE_SPACE_IN_LINE* ')'
                  |	'(' WHITE_SPACE_IN_LINE* exprs WHITE_SPACE_IN_LINE* ')' WHITE_SPACE_IN_LINE* simpleExpr2
                  |	'(' WHITE_SPACE_IN_LINE* exprs WHITE_SPACE_IN_LINE* ')'
                  |	'new' (WHITE_SPACE_IN_LINE* classTemplate WHITE_SPACE_IN_LINE* '.' WHITE_SPACE_IN_LINE* id | WHITE_SPACE_IN_LINE* 'new' (WHITE_SPACE_IN_LINE* classTemplate WHITE_SPACE_IN_LINE* '.' WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* simpleExpr2 | WHITE_SPACE_IN_LINE* templateBody) WHITE_SPACE_IN_LINE* '.' WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* | WHITE_SPACE_IN_LINE* templateBody) WHITE_SPACE_IN_LINE* '.' WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* simpleExpr2
                  |	blockExpr WHITE_SPACE_IN_LINE* '.' WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* simpleExpr2
                  |	blockExpr WHITE_SPACE_IN_LINE* '.' WHITE_SPACE_IN_LINE* id
                  |	'new' (WHITE_SPACE_IN_LINE* classTemplate WHITE_SPACE_IN_LINE* typeArgs WHITE_SPACE_IN_LINE* |	WHITE_SPACE_IN_LINE* 'new' (WHITE_SPACE_IN_LINE* classTemplate WHITE_SPACE_IN_LINE* typeArgs WHITE_SPACE_IN_LINE* simpleExpr2 WHITE_SPACE_IN_LINE* | WHITE_SPACE_IN_LINE* templateBody) WHITE_SPACE_IN_LINE* typeArgs | WHITE_SPACE_IN_LINE* templateBody) WHITE_SPACE_IN_LINE* typeArgs WHITE_SPACE_IN_LINE* simpleExpr2
                  |	blockExpr WHITE_SPACE_IN_LINE* typeArgs WHITE_SPACE_IN_LINE* simpleExpr2
                  |	blockExpr WHITE_SPACE_IN_LINE* typeArgs ;

simpleExpr2       :  '.' id  WHITE_SPACE_IN_LINE* simpleExpr2
                  |	'.' id
                  |	'_' '.' id  WHITE_SPACE_IN_LINE* simpleExpr2
                  |	'_' '.' id
                  |	typeArgs  WHITE_SPACE_IN_LINE* simpleExpr2
                  |	typeArgs
                  |	'_'  WHITE_SPACE_IN_LINE* typeArgs WHITE_SPACE_IN_LINE*  simpleExpr2
                  |	'_' WHITE_SPACE_IN_LINE*  typeArgs
                  |	argumentExprs  WHITE_SPACE_IN_LINE* simpleExpr2
                  |	argumentExprs ;
                  
exprs             : expr ( WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* expr)* ;

argumentExprs     : '(' WHITE_SPACE_IN_LINE* exprs? WHITE_SPACE_IN_LINE* ')' WHITE_SPACE_IN_LINE*
                  | '(' WHITE_SPACE_IN_LINE* (exprs WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE*)? postfixExpr WHITE_SPACE_IN_LINE* ':' '_' '*' ')' WHITE_SPACE_IN_LINE*
                  | Nl? WHITE_SPACE_IN_LINE* blockExpr WHITE_SPACE_IN_LINE*;
                  
blockExpr         : '{' WHITE_SPACE_IN_LINE* caseClauses WHITE_SPACE_IN_LINE* '}'
                  | '{' WHITE_SPACE_IN_LINE* block WHITE_SPACE_IN_LINE* '}' ;
block             : blockStat (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* blockStat)* WHITE_SPACE_IN_LINE* resultExpr? ;

blockStat         : import_
                  | annotation* WHITE_SPACE_IN_LINE* ('implicit' | 'lazy')? WHITE_SPACE_IN_LINE* def
                  | annotation* WHITE_SPACE_IN_LINE* (localModifier WHITE_SPACE_IN_LINE*)* WHITE_SPACE_IN_LINE* tmplDef
                  | expr1
                  | ;

resultExpr        : expr1
                  | (bindings | ('implicit'? WHITE_SPACE_IN_LINE* id | '_') WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* compoundType) WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE* block ;

enumerators       : generator WHITE_SPACE_IN_LINE* (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* generator)* ;

generator         : pattern1 WHITE_SPACE_IN_LINE* '<-' WHITE_SPACE_IN_LINE* expr (WHITE_SPACE_IN_LINE* semi? WHITE_SPACE_IN_LINE* guard | WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* pattern1 WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr)* ;

caseClauses       : caseClause+ ;

caseClause        : 'case' WHITE_SPACE_IN_LINE* pattern WHITE_SPACE_IN_LINE* guard? WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE* block ;
  
guard             : 'if' WHITE_SPACE_IN_LINE* postfixExpr ;

pattern           : pattern1 (WHITE_SPACE_IN_LINE* '|' WHITE_SPACE_IN_LINE* pattern1 )* ;

pattern1          : ID WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* typePat
                  | '_' WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* typePat
                  | pattern2 ;

pattern2          : ID WHITE_SPACE_IN_LINE* ('@' pattern3)?
                  | pattern3 ;

pattern3          : simplePattern
                  | simplePattern (WHITE_SPACE_IN_LINE* id WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* simplePattern)* ;
                  
simplePattern     : '_'
                  | ID
                  | literal 
                  | stableId (WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* patterns WHITE_SPACE_IN_LINE* ')')?
                  | stableId WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* (patterns WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE*)? (ID WHITE_SPACE_IN_LINE* '@')? WHITE_SPACE_IN_LINE* '_' WHITE_SPACE_IN_LINE* '*' WHITE_SPACE_IN_LINE* ')'
                  | '(' WHITE_SPACE_IN_LINE* patterns? WHITE_SPACE_IN_LINE* ')' ;

patterns          : pattern (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* patterns)*
                  | ('_' WHITE_SPACE_IN_LINE*) * ;

typeParamClause   : '[' WHITE_SPACE_IN_LINE* variantTypeParam (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* variantTypeParam)* WHITE_SPACE_IN_LINE* ']' ;

funTypeParamClause: '[' WHITE_SPACE_IN_LINE* typeParam (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* typeParam)* WHITE_SPACE_IN_LINE* ']' ;

variantTypeParam  : annotation? WHITE_SPACE_IN_LINE* ('+' | '-')? WHITE_SPACE_IN_LINE* typeParam ;

typeParam         : (id | '_') WHITE_SPACE_IN_LINE* typeParamClause? (WHITE_SPACE_IN_LINE* '>:' WHITE_SPACE_IN_LINE* type)? (WHITE_SPACE_IN_LINE* '<:' WHITE_SPACE_IN_LINE* type)?
                    ('<%' WHITE_SPACE_IN_LINE* type)* (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type)* ;
                         
paramClauses      : paramClause* (WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* 'implicit' WHITE_SPACE_IN_LINE* params WHITE_SPACE_IN_LINE* ')')? ;

paramClause       : Nl? WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* params? WHITE_SPACE_IN_LINE*')' WHITE_SPACE_IN_LINE* ;

params            : param (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* param)* ;

param             : annotation* WHITE_SPACE_IN_LINE* id  (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* paramType)? (WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr)? ;

paramType         : type 
                  | '=>' WHITE_SPACE_IN_LINE* type
                  | type WHITE_SPACE_IN_LINE* '*';

classParamClauses : classParamClause* 
                    (WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* 'implicit' WHITE_SPACE_IN_LINE* classParams WHITE_SPACE_IN_LINE* ')')? ;
                         
classParamClause  : Nl? WHITE_SPACE_IN_LINE* '(' WHITE_SPACE_IN_LINE* classParams? WHITE_SPACE_IN_LINE* ')' WHITE_SPACE_IN_LINE* ;

classParams       : classParam (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* classParam)* ;

classParam        : annotation* WHITE_SPACE_IN_LINE* (modifier WHITE_SPACE_IN_LINE*)*  (WHITE_SPACE_IN_LINE* 'val' | WHITE_SPACE_IN_LINE* 'var')? WHITE_SPACE_IN_LINE*
                    id WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* paramType (WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr)? ;
                    
bindings          : '(' WHITE_SPACE_IN_LINE* binding (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* binding )* WHITE_SPACE_IN_LINE* ')' ;

binding           : (id | '_') (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type)? ;

modifier          : localModifier 
                  | accessModifier
                  | 'override' ;
                  
localModifier     : 'abstract'
                  | 'final'
                  | 'sealed'
                  | 'implicit'
                  | 'lazy' ;
                  
accessModifier    : ('private' WHITE_SPACE_IN_LINE* | 'protected' WHITE_SPACE_IN_LINE*)  accessQualifier? ;

accessQualifier   : '[' WHITE_SPACE_IN_LINE* (id | 'this') WHITE_SPACE_IN_LINE* ']' ;

annotation        : '@' simpleType WHITE_SPACE_IN_LINE* argumentExprs* WHITE_SPACE_IN_LINE*;

constrAnnotation  : '@' simpleType WHITE_SPACE_IN_LINE* argumentExprs ;

templateBody      : Nl? WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* selfType? WHITE_SPACE_IN_LINE* templateStat (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* templateStat)* WHITE_SPACE_IN_LINE* '}' ;

templateStat      : import_
                  | (annotation Nl?)* ((modifier WHITE_SPACE_IN_LINE*)+ | WHITE_SPACE_IN_LINE*) def
                  | (annotation Nl?)* ((modifier WHITE_SPACE_IN_LINE*)+ | WHITE_SPACE_IN_LINE*) dcl
                  |  expr
                  | ;
                  
selfType          : id (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type)? WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE*
                  | 'this' WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type WHITE_SPACE_IN_LINE* '=>' ;

import_           : 'import' WHITE_SPACE_IN_LINE+ importExpr (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* importExpr)* ;

importExpr        : stableId '.' (id | '_' | importSelectors) ;

importSelectors   : '{' WHITE_SPACE_IN_LINE* (WHITE_SPACE_IN_LINE* importSelector WHITE_SPACE_IN_LINE* ',')* (WHITE_SPACE_IN_LINE* importSelector | WHITE_SPACE_IN_LINE* '_') WHITE_SPACE_IN_LINE* '}' ;

importSelector    : id (WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE* id | WHITE_SPACE_IN_LINE* '=>' WHITE_SPACE_IN_LINE* '_')? ;
 
dcl               : 'val' WHITE_SPACE_IN_LINE+ valDcl
                  | 'var' WHITE_SPACE_IN_LINE+ varDcl
                  | 'def' WHITE_SPACE_IN_LINE+ funDcl
                  | 'type' WHITE_SPACE_IN_LINE* Nl* WHITE_SPACE_IN_LINE* typeDcl ;

valDcl            : ids WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type ;

varDcl            : ids WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type ;

funDcl            : funSig (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type)? ;

funSig            : id WHITE_SPACE_IN_LINE* funTypeParamClause? WHITE_SPACE_IN_LINE* paramClauses ;

typeDcl           : id WHITE_SPACE_IN_LINE* typeParamClause? WHITE_SPACE_IN_LINE* ('>:' WHITE_SPACE_IN_LINE* type)? (WHITE_SPACE_IN_LINE* '<:' WHITE_SPACE_IN_LINE* type)? ;

patVarDef         : 'val' WHITE_SPACE_IN_LINE+ patDef
                  | 'var' WHITE_SPACE_IN_LINE+ varDef ;

def               : patVarDef
                  | 'def' WHITE_SPACE_IN_LINE* funDef
                  | 'type' WHITE_SPACE_IN_LINE* Nl* WHITE_SPACE_IN_LINE* typeDef
                  | tmplDef ;
                  
patDef            : pattern2 (WHITE_SPACE_IN_LINE* ',' WHITE_SPACE_IN_LINE* pattern2)* (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type)* WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr ;

varDef            : patDef
                  | ids WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* '_' ;
                  
funDef            : funSig (WHITE_SPACE_IN_LINE* ':' WHITE_SPACE_IN_LINE* type)? WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* expr
                  | funSig WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* block WHITE_SPACE_IN_LINE* '}'
                  | 'this' WHITE_SPACE_IN_LINE* paramClause WHITE_SPACE_IN_LINE* paramClauses
                    ('=' WHITE_SPACE_IN_LINE* constrExpr | WHITE_SPACE_IN_LINE* Nl WHITE_SPACE_IN_LINE* constrBlock) ;

typeDef           :  id WHITE_SPACE_IN_LINE* typeParamClause? WHITE_SPACE_IN_LINE* '=' WHITE_SPACE_IN_LINE* type ;

tmplDef           : 'case'? WHITE_SPACE_IN_LINE* 'class' WHITE_SPACE_IN_LINE+ classDef
                  | 'case'? WHITE_SPACE_IN_LINE* 'object' WHITE_SPACE_IN_LINE+ objectDef
                  | 'trait' WHITE_SPACE_IN_LINE+ traitDef ;

classDef          : id WHITE_SPACE_IN_LINE* typeParamClause? WHITE_SPACE_IN_LINE* (constrAnnotation WHITE_SPACE_IN_LINE*)* WHITE_SPACE_IN_LINE* accessModifier?
                    classParamClauses WHITE_SPACE_IN_LINE* classTemplateOpt ;
                      
traitDef          : id WHITE_SPACE_IN_LINE* typeParamClause? WHITE_SPACE_IN_LINE* traitTemplateOpt ;

objectDef         : id WHITE_SPACE_IN_LINE* classTemplateOpt ;

classTemplateOpt  : 'extends' WHITE_SPACE_IN_LINE+ classTemplate | ('extends'? WHITE_SPACE_IN_LINE* templateBody)? ;

traitTemplateOpt  : ('extends' WHITE_SPACE_IN_LINE+ traitTemplate)
                  | ('extends'? WHITE_SPACE_IN_LINE* templateBody)? ;

classTemplate     : earlyDefs? WHITE_SPACE_IN_LINE* classParents WHITE_SPACE_IN_LINE* templateBody? ;

traitTemplate     : earlyDefs? WHITE_SPACE_IN_LINE* traitParents WHITE_SPACE_IN_LINE* templateBody? ;

classParents      : constr (WHITE_SPACE_IN_LINE+ 'with' WHITE_SPACE_IN_LINE+ annotType)* ;

traitParents      : annotType (WHITE_SPACE_IN_LINE+ 'with' WHITE_SPACE_IN_LINE+ annotType)* ;

constr            : annotType WHITE_SPACE_IN_LINE* argumentExprs* ;

earlyDefs         : '{' WHITE_SPACE_IN_LINE* (earlyDef (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* earlyDef)*)? WHITE_SPACE_IN_LINE* '}' WHITE_SPACE_IN_LINE* 'with' ;

earlyDef          : (annotation WHITE_SPACE_IN_LINE* Nl?)* WHITE_SPACE_IN_LINE* (modifier WHITE_SPACE_IN_LINE*)* WHITE_SPACE_IN_LINE* patVarDef ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{' WHITE_SPACE_IN_LINE* selfInvocation (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* blockStat)* WHITE_SPACE_IN_LINE* '}' ;
selfInvocation    : 'this' WHITE_SPACE_IN_LINE* argumentExprs+ ;

topStatSeq        : topStat (WHITE_SPACE_IN_LINE* semi WHITE_SPACE_IN_LINE* topStat)* ;

topStat           : (annotation WHITE_SPACE_IN_LINE* Nl?)* WHITE_SPACE_IN_LINE* (modifier WHITE_SPACE_IN_LINE*)* WHITE_SPACE_IN_LINE* tmplDef
                  | import_
                  | packaging
                  | packageObject
                  | ;
                    
packaging         : 'package' WHITE_SPACE_IN_LINE+ qualId WHITE_SPACE_IN_LINE* Nl? WHITE_SPACE_IN_LINE* '{' WHITE_SPACE_IN_LINE* topStatSeq WHITE_SPACE_IN_LINE* '}' ;

packageObject     : 'package' WHITE_SPACE_IN_LINE+ 'object' WHITE_SPACE_IN_LINE+ objectDef ;

compilationUnit   : ('package' WHITE_SPACE_IN_LINE+ qualId WHITE_SPACE_IN_LINE* semi)* WHITE_SPACE_IN_LINE* topStatSeq ;

id                : ID
                  | '\'' StringLiteral '\''
                  | OP_1
                  | OP_2
                  | OP_3
                  | EPT
                  | TLD
                  | ASSIGN
                  | UNDER
                  | FUNTYPE;

semi              :  SEMICOLON |  Nl+;

// Lexer

SEMICOLON   :  ';';
LBRACE		:  '{';
RBRACE		:  '}';
LSQBRACKET	:  '[';
RSQBRACKET	:  ']';
LPARENTHESIS		:  '(';
RPARENTHESIS		:  ')';
DOT				:  '.';
COMMA			:  ',';

Q_MARK			:  '\'';
Q_MARK_2		:  '"';

COLON			:  ':';
UNDER		:  '_';
ASSIGN				:  '=';
FUNTYPE				:  '=>';
CHOOSE			:  '<-';
UPPER_BOUND			    :  '<:';
LOWER_BOUND			    :  '>:';
INNER_CLASS			:  '#';
AT		  	    :  '@';
VIEW				:  '<%';

SLASH			:  '\\';

ABSTRACT		:  'abstract';
CASE			:  'case';
CATCH			:  'catch';
CLASS			:  'class';
DEF				:  'def';
DO				:  'do';
ELSE			:  'else';
EXTENDS			:  'extends';
FINAL			:  'final';
FINALLY			:  'finally';
FOR				:  'for';
FOR_SOME			:  'forSome';
IF				:  'if';
IMPLICIT		:  'implicit';
IMPORT			:  'import';
LAZY			:  'lazy';
MACRO			:  'macro';
MATCH			:  'match';
NEW				:  'new';
NULL			:  'null';
OBJECT			:  'object';
OVERRIDE		:  'override';
PACKAGE			:  'package';
PRIVATE			:  'private';
PROTECTED		:  'protected';
RETURN			:  'return';
SEALED			:  'sealed';
SUPER			:  'super';
THIS			:  'this';
THROW			:  'throw';
TRAIT			:  'trait';
TRY				:  'try';
TYPE			:  'type';
VAL				:  'val';
VAR				:  'var';
WHILE			:  'while';
WITH			:  'with';
YIELD			:  'yield';

OP_1		    :  '+';
OP_2			:  '-';
OP_3			:  '*';
EPT				:  '!';
TLD				:  '~';


BooleanLiteral   :  'true' | 'false';
CharacterLiteral :  '\'' (PrintableChar | CharEscapeSeq) '\'';
StringLiteral    :  '"' StringElement* '"'
                 |  '"""' MultiLineChars '"""';
SymbolLiteral    :  '\'' ID;
IntegerLiteral   :  (DecimalNumeral | HexNumeral) ('L' | 'l')?;
FloatingPointLiteral
                 :  Digit+ '.' Digit+ ExponentPart? FloatType?
                 |  '.' Digit+ ExponentPart? FloatType?
                 |  Digit ExponentPart FloatType?
                 |  Digit+ ExponentPart? FloatType;

Nl               :  '\r'? '\n';

ID               : Op
                 | Upper Idrest
                 | Lower Idrest ;

LINE_COMMENT : '//' .*? Nl          -> channel(HIDDEN) ;
BLOCK_COMMENT      : '/*' .*? '*/'    	-> channel(HIDDEN) ;

WHITE_SPACE_IN_LINE       :  [ \t]+ ;



// fragments
fragment UnicodeEscape    :	'\\' 'u' 'u'? HexDigit HexDigit HexDigit HexDigit ;


fragment Op               :  Opchar+;
fragment Opchar           :  ~[a-zA-Z0-9()[\]{}.;, \r\t\n'"$_] ;

fragment Idrest           :  (Letter | Digit)* ('_' Op)?;

fragment StringElement    :  '\u0020'| '\u0021'|'\u0023' .. '\u007F'
                          |  CharEscapeSeq;
fragment MultiLineChars   :  ('"'? '"'? .*?)* '"'*;

fragment HexDigit         :  '0' .. '9'  |  'A' .. 'F'  |  'a' .. 'f' ;
fragment FloatType        :  'F' | 'f' | 'D' | 'd';
fragment Upper            :  'A'  ..  'Z' | '$' | '_';  // and Unicode category Lu
fragment Lower            :  'a' .. 'z'; // and Unicode category Ll
fragment Letter           :  Upper | Lower; // and Unicode categories Lo, Lt, Nl
fragment ExponentPart     :  ('E' | 'e') ('+' | '-')? Digit+;
fragment PrintableChar    : '\u0020' .. '\u007F' ;
fragment CharEscapeSeq    : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\');
fragment DecimalNumeral   :  '0' | NonZeroDigit Digit*;
fragment HexNumeral       :  '0' 'x' HexDigit HexDigit+;
fragment Digit            :  '0' | NonZeroDigit;
fragment NonZeroDigit     :  '1' .. '9';

