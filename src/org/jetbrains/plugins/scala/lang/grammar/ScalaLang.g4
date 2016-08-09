	
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
                  | compilationUnit
                  | selfType?  templateStat ( semi  templateStat)*    // for debug purposes
                  | block;                                            // for debug purposes

literal           : '-'? IntegerLiteral
                  | '-'? FloatingPointLiteral
                  | BooleanLiteral
                  | CharacterLiteral
                  | StringLiteral
                  | SymbolLiteral
                  | 'null' ; 
                  
qualId            : qualId '.' id | id ;

ids               : id (  ','  id)* ;

path              :  stableId
                  |  (id '.')? 'this' ;

stableId          :  id
                  |  stableId '.' id
                  |  (id '.')? 'this' '.' id
                  |  (id '.')? 'super' (classQualifier)? '.' id ;

classQualifier    : '[' id ']' ;

type              : typeType
                  | wildcardType
                  | existentialType
                  | infixType ;

typeType          : infixType  '=>'  type
                  | '(' ('=>' type)? ')' '=>' type ;
wildcardType      : '_' ('>:' type)? ('<:' type)? ;
existentialType   : infixType  existentialClause ;

functionArgTypes  : infixType
                  | '('  ( paramType ( ','  paramType )* )?  ')' ;

existentialClause : 'forSome'  '{'  existentialDcl ( semi  existentialDcl)*  '}';

existentialDcl    : typeDeclaration
                  | valueDeclaration;

infixType         : compoundType (  id  Nl?  compoundType)*;

compoundType      : annotType ( 'with'  annotType)*  refinement?
                  | refinement;

annotType         : simpleType  annotationsNonEmpty?;

simpleType        : simpleType  typeArgs
                  | simpleType '#' id
                  | stableId
                  | path '.' 'type'
                  | '('  types ','? ')';

typeArgs          : '['  types  ']';

types             : type ( ','  type)*;

refinement        : Nl? '{'  refineStat ( semi  refineStat)*  '}';

refineStat        : dcl
                  | typeDefinition
                  | ;

typePat           : type;

ascription        : ':'  infixType
                  | ':'  annotationsNonEmpty
                  | ':'  sequenceArg;

sequenceArg       : '_' '*' ;

expr              : (bindings | id | '_')  '=>'  expr
                  | expr1 ;

expr1             : ifStmt
                  | whileStmt
                  | tryStmt
                  | doStmt
                  | forStmt
                  | throwStmt
                  | returnStmt
                  | assignStmt
                  | postfixExpr
                  | typedExprStmt
                  | matchStmt ;
//-----------------------------------------------------------------------------
ifStmt            : 'if'  '('  expr  ')'  Nl*  expr ( semi?  'else'  expr)? ;

whileStmt         : 'while'  '('  expr  ')'  Nl*  expr ;

tryStmt           : 'try' tryBlock catchBlock? finallyBlock? ;
tryBlock          : '{'  block  '}'  |  expr ;
catchBlock        : 'catch'  '{'  caseClauses  '}' ;
finallyBlock      : 'finally'  expr ;

doStmt            : 'do'  expr  semi?  'while'  '('  expr  ')' ;

forStmt           : 'for'  ('('  enumerators  ')' | '{'  enumerators  '}')  Nl*  'yield'?  expr ;

throwStmt         : 'throw'  expr ;

returnStmt        : 'return'  expr? ;

assignStmt        : postfixExpr '=' expr ;

typedExprStmt     : postfixExpr  ascription ;

matchStmt         : postfixExpr  'match'  '{'  caseClauses  '}' ;
//-----------------------------------------------------------------------------
postfixExpr       : infixExpr ( id  Nl?)? ;

/*infixExpr         : infixExpr  id  Nl?  infixExpr
                  | prefixExpr ;*/
infixExpr         : prefixExpr (id typeArgs? Nl? prefixExpr)* ;

prefixExpr        : ('-' | '+' | '~' | '!')? simpleExpr ;

simpleExpr        : newTemplate | blockExpr | simpleExpr1 '_'?;

newTemplate       : 'new' (classTemplate | templateBody) ;

simpleExpr1       : literal
                  | path
                  | '_'
                  | '(' (exprs ','?)? ')'
                  | (newTemplate | blockExpr) '.' id
                  | simpleExpr1 '_'? '.' id
                  | (newTemplate | blockExpr) typeArgs
                  | simpleExpr1 '_'? typeArgs
                  | simpleExpr1 argumentExprs ;

exprs             : expr (  ','  expr)* ;

argumentExprs     : '('  exprs?  ')' 
                  | '('  (exprs  ',' )? postfixExpr  ':' '_' '*' ')' 
                  | Nl?  blockExpr ;
                  
blockExpr         : '{'  caseClauses  '}'
                  | '{'  block  '}' ;
block             : blockStat ( semi  blockStat)*
                  | blockStat ( semi  blockStat)* resultExpr ;
/*
    DO NOT try to replace enterances of blockNode symbol with block symbol
*/
blockNode         : block ;

blockStat         : import_
                  | 'implicit'? def
                  | tmplDef
                  | expr1
                  | ;

resultExpr        : bindings  '=>'  blockNode
                  | ('implicit'?  id | '_')  ':'  paramType '=>'  blockNode ;

enumerators       : generator  ( semi  generator)* ;

//generator         : pattern1  '<-'  expr ( semi?  guard |  semi  pattern1  '='  expr)* ;
generator         : generatorNoGuard guard? ;

generatorNoGuard  : pattern1  '<-'  expr ;

caseClauses       : caseClause+ ;

caseClause        : 'case'  pattern  guard?  '=>'  blockNode ;
  
guard             : 'if'  postfixExpr ;

pattern           : pattern1 ( '|'  pattern1 )* ;

pattern1          : typedPattern
                  | pattern2 ;

typedPattern      : ID  ':'  typePat
                  | '_'  ':'  typePat ;

pattern2          : referencePattern
                  | namingPattern
                  | pattern3 ;

referencePattern  : ID ;
namingPattern     : ID '@' pattern3 ;

pattern3          : simplePattern
                  | simplePattern ( id  Nl?  simplePattern)* ;
//-----------------------------------------------------------------------------
simplePattern     : wildcardPattern
                  | patternInParenthesis
                  | referencePattern
                  | literalPattern
                  | stableReferencePattern
                  | constructorPattern
                  | tuplePattern ;

wildcardPattern   : '_' ;

patternInParenthesis
                  : '(' pattern ')' ;

tuplePattern      : '(' patterns? ')' ;

literalPattern    : literal ;

stableReferencePattern
                  : stableId ;

constructorPattern: patternArgs ;

patternArgs       : '('  patterns  ')'
                  | '('  (patterns  ',' )? namingPattern2  ')'
                  | '('  (patterns  ',' )? seqWildcard  ')';

namingPattern2    : ID  '@'  seqWildcard ;

//-----------------------------------------------------------------------------
patterns          : patternSeq
                  | pattern
                  | seqWildcard ;

patternSeq        : pattern ( ','  patterns)* ;
seqWildcard       : '_' '*' ;

typeParamClause   : '['  variantTypeParam ( ','  variantTypeParam)*  ']' ;

variantTypeParam  : annotationsNonEmpty? (OP_1|OP_2)? (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)? ('<%'  type)* ( ':'  type)* ;

funTypeParamClause: '['  typeParam ( ','  typeParam)*  ']' ;

typeParam         : annotationsNonEmpty? (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)?
                    ('<%'  type)* ( ':'  type)* ;
                         
paramClauses      : paramClause* implicitParamClause?;

implicitParamClause
                  : ( Nl?  '('  'implicit'  params  ')') ;

paramClause       : Nl?  '('  params? ')'  ;

params            : param ( ','  param)* ;

param             : annotations modifiersOrEmpty id  ( ':'  paramType)? ( '='  expr)? ;

paramType         : type 
                  | '=>'  type
                  | type  '*';

classParamClauses : classParamClause* implicitClassParamClause? ;

implicitClassParamClause
                  : Nl?  '(' 'implicit'  classParams ')' ;

classParamClause  : Nl?  '(' classParams? ')'  ;

classParams       : classParam ( ','  classParam)* ;

classParam        : annotations  modifiersOrEmpty  ( 'val' |  'var')?
                    id  ':'  paramType ( '='  expr)? ;
                    
bindings          : '('  binding ( ','  binding )*  ')' ;

binding           : (id | '_') ( ':'  type)? ;

modifier          : localModifier 
                  | accessModifier
                  | 'override' ;

modifiersOrEmpty  : modifier* ;
                  
localModifier     : 'abstract'
                  | 'final'
                  | 'sealed'
                  | 'implicit'
                  | 'lazy' ;
                  
accessModifier    : ('private'  | 'protected' )  accessQualifier? ;

accessQualifier   : '['  (id | 'this')  ']' ;

annotation        : '@' annotationExpr ;

annotationExpr    : constr ;

annotations       : annotation* ;
annotationsNonEmpty
                  : annotation+ ;

templateBody      : Nl?  '{'  selfType?  templateStat ( semi  templateStat)*  '}' ;

templateStat      : import_
                  | def
                  | dcl
                  | expr
                  | ;
                  
selfType          : id ( ':'  type)?  '=>' 
                  | 'this'  ':'  type  '=>' ;

import_           : 'import'  importExpr ( ','  importExpr)* ;

importExpr        : stableId ('.' '_' | '.' importSelectors)? ;

importSelectors   : '{'  ( importSelector  ',')* ( importSelector |  '_')  '}' ;

importSelector    : id ( '=>'  id |  '=>'  '_')? ;
 
dcl               : valueDeclaration
                  | variableDeclaration
                  | functionDeclaration
                  | typeDeclaration ;

valueDeclaration  : annotations modifiersOrEmpty 'val'  valDcl ;

variableDeclaration
                  : annotations modifiersOrEmpty 'var' varDcl ;

functionDeclaration
                  : annotations modifiersOrEmpty 'def' funDcl ;

typeDeclaration   : annotations modifiersOrEmpty 'type'  Nl*  typeDcl ;

valDcl            : ids  ':'  type ;

varDcl            : ids  ':'  type ;

funDcl            : funSig ( ':'  type)? ;

funSig            : id  funTypeParamClause?  paramClauses ;

typeDcl           : id  typeParamClause?  ('>:'  type)? ( '<:'  type)? ;

patVarDef         : patternDefinition
                  | variableDefinition;

def               : patternDefinition
                  | variableDefinition
                  | functionDefinition
                  | typeDefinition
                  | templateDefinition ;

patternDefinition        : annotations modifiersOrEmpty 'val'  patDef ;
variableDefinition       : annotations modifiersOrEmpty 'var'  varDef ;
functionDefinition       : annotations modifiersOrEmpty 'def'  funDef ;
typeDefinition           : annotations modifiersOrEmpty 'type'  Nl*  typeDef ;
templateDefinition       : tmplDef ;
                  
patDef            : patternList (':'  type)?  '='  expr ;

patternList       : pattern2 ( ','  pattern2)* ;

varDef            : patDef
                  | ids  ':'  type  '='  '_' ;
                  
funDef            : funSig ( ':'  type)?  '='  expr
                  | funSig  Nl?  blockWithBraces
                  | 'this'  paramClause  paramClauses
                    ('='  constrExpr |  Nl  constrBlock) ;

blockWithBraces   : '{'  block  '}' ;

typeDef           :  id  typeParamClause?  '='  type ;

tmplDef           : classDefinition
                  | objectDefinition
                  | traitDefinition ;

classDefinition   : annotations modifiersOrCase  'class'  classDef ;

objectDefinition  : annotations modifiersOrCase  'object'  objectDef ;

traitDefinition   : annotations modifiersOrEmpty 'trait'  traitDef ;

modifiersOrCase   : modifier* 'case'? ;

classDef          : id  typeParamClause?  primaryConstructor classTemplateOpt ;

primaryConstructor: annotations  accessModifierOrEmpty classParamClauses ;

accessModifierOrEmpty
                  : accessModifier ? ;
                      
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

earlyDefs         : '{'  (patVarDef ( semi  patVarDef)*)?  '}'  'with' ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{'  selfInvocation ( semi  blockStat)*  '}' ;
selfInvocation    : 'this'  argumentExprs+ ;

topStatSeq        : topStat ( semi  topStat)* ;

topStat           : tmplDef
                  | import_
                  | packaging
                  | packageObject
                  | ;
                    
packaging         : 'package'  qualId  Nl?  '{'  topStatSeq  '}' ;

packageObject     : 'package'  'object'  objectDef ;

compilationUnit   : packageDcl? topStatSeq ;

packageDcl        : 'package'  qualId  semi? packageDcl? ;

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

