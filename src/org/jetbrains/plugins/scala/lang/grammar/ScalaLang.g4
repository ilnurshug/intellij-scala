	
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

ids               : id (  ','  id)* ;

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

type              : functionArgTypes  '=>'  type
                  | infixType  existentialClause
                  | infixType ;

functionArgTypes  : infixType
                  | '('  ( paramType ( ','  paramType )* )?  ')' ;

existentialClause : 'forSome'  '{'  existentialDcl ( semi  existentialDcl)*  '}';

existentialDcl    : 'type'  typeDcl
                  | 'val'  valDcl;

infixType         : compoundType (  id  Nl?  compoundType)*;

compoundType      : annotType ( 'with'  annotType)*  refinement?
                  | refinement;

annotType         : simpleType  annotation*;

simpleType        : simpleType  typeArgs
                  | simpleType '#' id
                  | stableId
                  | (stableId | (id '.')? 'this') '.' 'type'
                  | '('  types  ')';

typeArgs          : '['  types  ']';

types             : type ( ','  type)*;

refinement        : Nl? '{'  refineStat ( semi  refineStat)*  '}';

refineStat        : dcl
                  | 'type'  typeDef
                  | ;

typePat           : type;

ascription        : ':'  infixType
                  | ':'  annotation+
                  | ':' '_' '*';

expr              : (bindings | ('implicit' |) id | '_')  '=>'  expr
                  | expr1 ;

expr1             : 'if'  '('  expr  ')'  Nl*  expr ( semi?  'else'  expr)?
                  | 'while'  '('  expr  ')'  Nl*  expr
                  | 'try' ( '{'  block  '}'  |  expr) ( 'catch'  '{'  caseClauses  '}')? ( 'finally'  expr)?
                  | 'do'  expr  semi?  'while'  '('  expr  ')'
                  | 'for'  ('('  enumerators  ')' | '{'  enumerators  '}')  Nl*  'yield'?  expr
                  | 'throw'  expr
                  | 'return'  expr?
                  | (('new'  (classTemplate | templateBody)| blockExpr | simpleExpr1  '_'?) '.')  id  '='  expr
                  | simpleExpr1  argumentExprs  '='  expr
                  | postfixExpr
                  | postfixExpr  ascription
                  | postfixExpr  'match'  '{'  caseClauses  '}' ;

postfixExpr       : infixExpr ( id  Nl?)? ;

infixExpr         : prefixExpr
                  | infixExpr  id  Nl?  infixExpr ;

prefixExpr        : ('-' | '+' | '~' | '!')? 
                    ('new'  (classTemplate | templateBody)| blockExpr | simpleExpr1 '_'?) ;

simpleExpr1       :	literal  simpleExpr2
                  |	literal
                  |	path  simpleExpr2
                  |	path
                  |	'_'  simpleExpr2
                  |	'_'
                  |	'('  ')'  simpleExpr2
                  |	'('  ')'
                  |	'('  exprs  ')'  simpleExpr2
                  |	'('  exprs  ')'
                  |	'new' ( classTemplate  '.'  id |  'new' ( classTemplate  '.'  id  simpleExpr2 |  templateBody)  '.'  id  |  templateBody)  '.'  id  simpleExpr2
                  |	blockExpr  '.'  id  simpleExpr2
                  |	blockExpr  '.'  id
                  |	'new' ( classTemplate  typeArgs  |	 'new' ( classTemplate  typeArgs  simpleExpr2  |  templateBody)  typeArgs |  templateBody)  typeArgs  simpleExpr2
                  |	blockExpr  typeArgs  simpleExpr2
                  |	blockExpr  typeArgs ;

simpleExpr2       :  '.' id   simpleExpr2
                  |	'.' id
                  |	'_' '.' id   simpleExpr2
                  |	'_' '.' id
                  |	typeArgs   simpleExpr2
                  |	typeArgs
                  |	'_'   typeArgs   simpleExpr2
                  |	'_'   typeArgs
                  |	argumentExprs   simpleExpr2
                  |	argumentExprs ;
                  
exprs             : expr (  ','  expr)* ;

argumentExprs     : '('  exprs?  ')' 
                  | '('  (exprs  ',' )? postfixExpr  ':' '_' '*' ')' 
                  | Nl?  blockExpr ;
                  
blockExpr         : '{'  caseClauses  '}'
                  | '{'  block  '}' ;
block             : blockStat ( semi  blockStat)*  resultExpr? ;

blockStat         : import_
                  | annotation*  ('implicit' | 'lazy')?  def
                  | annotation*  (localModifier )*  tmplDef
                  | expr1
                  | ;

resultExpr        : expr1
                  | (bindings | ('implicit'?  id | '_')  ':'  compoundType)  '=>'  block ;

enumerators       : generator  ( semi  generator)* ;

generator         : pattern1  '<-'  expr ( semi?  guard |  semi  pattern1  '='  expr)* ;

caseClauses       : caseClause+ ;

caseClause        : 'case'  pattern  guard?  '=>'  block ;
  
guard             : 'if'  postfixExpr ;

pattern           : pattern1 ( '|'  pattern1 )* ;

pattern1          : ID  ':'  typePat
                  | '_'  ':'  typePat
                  | pattern2 ;

pattern2          : ID  ('@' pattern3)?
                  | pattern3 ;

pattern3          : simplePattern
                  | simplePattern ( id  Nl?  simplePattern)* ;
                  
simplePattern     : '_'
                  | ID
                  | literal 
                  | stableId ( '('  patterns  ')')?
                  | stableId  '('  (patterns  ',' )? (ID  '@')?  '_'  '*'  ')'
                  | '('  patterns?  ')' ;

patterns          : pattern ( ','  patterns)*
                  | ('_' ) * ;

typeParamClause   : '['  variantTypeParam ( ','  variantTypeParam)*  ']' ;

funTypeParamClause: '['  typeParam ( ','  typeParam)*  ']' ;

variantTypeParam  : annotation?  ('+' | '-')?  typeParam ;

typeParam         : (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)?
                    ('<%'  type)* ( ':'  type)* ;
                         
paramClauses      : paramClause* ( Nl?  '('  'implicit'  params  ')')? ;

paramClause       : Nl?  '('  params? ')'  ;

params            : param ( ','  param)* ;

param             : annotation*  id  ( ':'  paramType)? ( '='  expr)? ;

paramType         : type 
                  | '=>'  type
                  | type  '*';

classParamClauses : classParamClause* 
                    ( Nl?  '('  'implicit'  classParams  ')')? ;
                         
classParamClause  : Nl?  '('  classParams?  ')'  ;

classParams       : classParam ( ','  classParam)* ;

classParam        : annotation*  (modifier )*  ( 'val' |  'var')? 
                    id  ':'  paramType ( '='  expr)? ;
                    
bindings          : '('  binding ( ','  binding )*  ')' ;

binding           : (id | '_') ( ':'  type)? ;

modifier          : localModifier 
                  | accessModifier
                  | 'override' ;
                  
localModifier     : 'abstract'
                  | 'final'
                  | 'sealed'
                  | 'implicit'
                  | 'lazy' ;
                  
accessModifier    : ('private'  | 'protected' )  accessQualifier? ;

accessQualifier   : '['  (id | 'this')  ']' ;

annotation        : '@' simpleType  argumentExprs* ;

constrAnnotation  : '@' simpleType  argumentExprs ;

templateBody      : Nl?  '{'  selfType?  templateStat ( semi  templateStat)*  '}' ;

templateStat      : import_
                  | (annotation Nl?)* ((modifier )+ | ) def
                  | (annotation Nl?)* ((modifier )+ | ) dcl
                  |  expr
                  | ;
                  
selfType          : id ( ':'  type)?  '=>' 
                  | 'this'  ':'  type  '=>' ;

import_           : 'import'  importExpr ( ','  importExpr)* ;

importExpr        : stableId '.' (id | '_' | importSelectors) ;

importSelectors   : '{'  ( importSelector  ',')* ( importSelector |  '_')  '}' ;

importSelector    : id ( '=>'  id |  '=>'  '_')? ;
 
dcl               : 'val'  valDcl
                  | 'var'  varDcl
                  | 'def'  funDcl
                  | 'type'  Nl*  typeDcl ;

valDcl            : ids  ':'  type ;

varDcl            : ids  ':'  type ;

funDcl            : funSig ( ':'  type)? ;

funSig            : id  funTypeParamClause?  paramClauses ;

typeDcl           : id  typeParamClause?  ('>:'  type)? ( '<:'  type)? ;

patVarDef         : 'val'  patDef
                  | 'var'  varDef ;

def               : patVarDef
                  | 'def'  funDef
                  | 'type'  Nl*  typeDef
                  | tmplDef ;
                  
patDef            : pattern2 ( ','  pattern2)* ( ':'  type)*  '='  expr ;

varDef            : patDef
                  | ids  ':'  type  '='  '_' ;
                  
funDef            : funSig ( ':'  type)?  '='  expr
                  | funSig  Nl?  '{'  block  '}'
                  | 'this'  paramClause  paramClauses
                    ('='  constrExpr |  Nl  constrBlock) ;

typeDef           :  id  typeParamClause?  '='  type ;

tmplDef           : 'case'?  'class'  classDef
                  | 'case'?  'object'  objectDef
                  | 'trait'  traitDef ;

classDef          : id  typeParamClause?  (constrAnnotation )*  accessModifier?
                    classParamClauses  classTemplateOpt ;
                      
traitDef          : id  typeParamClause?  traitTemplateOpt ;

objectDef         : id  classTemplateOpt ;

classTemplateOpt  : 'extends'  classTemplate | ('extends'?  templateBody)? ;

traitTemplateOpt  : ('extends'  traitTemplate)
                  | ('extends'?  templateBody)? ;

classTemplate     : earlyDefs?  classParents  templateBody? ;

traitTemplate     : earlyDefs?  traitParents  templateBody? ;

classParents      : constr ( 'with'  annotType)* ;

traitParents      : annotType ( 'with'  annotType)* ;

constr            : annotType  argumentExprs* ;

earlyDefs         : '{'  (earlyDef ( semi  earlyDef)*)?  '}'  'with' ;

earlyDef          : (annotation  Nl?)*  (modifier )*  patVarDef ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{'  selfInvocation ( semi  blockStat)*  '}' ;
selfInvocation    : 'this'  argumentExprs+ ;

topStatSeq        : topStat ( semi  topStat)* ;

topStat           : (annotation  Nl?)*  (modifier )*  tmplDef
                  | import_
                  | packaging
                  | packageObject
                  | ;
                    
packaging         : 'package'  qualId  Nl?  '{'  topStatSeq  '}' ;

packageObject     : 'package'  'object'  objectDef ;

compilationUnit   : ('package'  qualId  semi)*  topStatSeq ;

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

