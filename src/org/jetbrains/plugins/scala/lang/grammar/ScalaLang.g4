	
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

@parser::header {
 import com.intellij.openapi.util.text.StringUtil;
 }

@parser::members {
Boolean isVarId() {
    return Character.isLowerCase(getCurrentToken().getText().charAt(0));
}

int getOccurrenceCount(char c) {
    CommonToken curToken = (CommonToken)_input.LT(1);
    CommonToken prevToken = (CommonToken)_input.LT(-1);

    if (curToken == null || prevToken == null) return 0;

    int prevTokenStart = prevToken.getStartIndex();
    int curTokenStart = curToken.getStartIndex();

    String substr = _input.getTokenSource().getInputStream().getText(new Interval(prevTokenStart, curTokenStart));

    return StringUtil.getOccurrenceCount(substr, c);
}

Boolean isSingleNl() {
    int nlCount = getOccurrenceCount('\n');

    return (nlCount == 1);
}

Boolean isNl() {
    return (getOccurrenceCount('\n') >= 1);
}

}

testRule          : id ({isNl()}? emptyNl id)+ ;

emptyNl           :  ;

program           : blockExpr
                  | selfType?  templateStat? ( (SEMICOLON | {isNl()}? emptyNl)  templateStat)*    // for debug purposes
                  | compilationUnit
                  | block;                                            // for debug purposes

literal           : '-'? IntegerLiteral
                  | '-'? FloatingPointLiteral
                  | BooleanLiteral
                  | CharacterLiteral
                  | StringLiteral
                  | SymbolLiteral
                  | 'null' ; 
                  
qualId            : qualId '.' id | id ;

ids               : fieldId (  ','  fieldId)* ;
fieldId           : id ;

pathRef           :  stableIdRef '.' id
                  |  thisReference '.' id
                  |  superReference '.' id
                  |  thisReference
                  |  id ;

pathRefExpr       :  stableIdRefExpr '.' id
                  |  thisReference '.' id
                  |  superReference '.' id
                  |  thisReference
                  |  id ;

reference         : id ;
thisReference     : (reference '.')? 'this' ;

stableIdRef       :  stableIdRef '.' id
                  |  thisReference '.' id
                  |  superReference '.' id
                  |  id ;

stableIdRefExpr   :  stableIdRefExpr '.' id
                  |  thisReference '.' id
                  |  superReference '.' id
                  |  id ;

superReference    : (reference '.')? 'super' (classQualifier)? ;

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

existentialClause : 'forSome'  '{'  existentialDcl ( (SEMICOLON | {isNl()}? emptyNl)  existentialDcl)*  '}';

existentialDcl    : typeDeclaration
                  | valueDeclaration;

infixType         : compoundType (  id  Nl?  compoundType)*;

compoundType      : annotType ( 'with'  annotType)*  refinement?
                  | refinement;

annotType         : simpleType  annotationsNonEmpty?;

simpleType        : simpleType  typeArgs
                  | simpleType '#' id
                  | stableIdRef
                  | pathRef '.' 'type'
                  | '('  types ','? ')';

typeArgs          : '['  type ( ','  type)*  ']';

types             : type ( ','  type)*;

refinement        : Nl? '{'  refineStat? ( (SEMICOLON | {isNl()}? emptyNl)  refineStat)*  '}';

refineStat        : dcl
                  | typeDefinition
                  ; //| ;

typePat           : type;

ascription        : ':'  sequenceArg
                  | ':'  annotationsNonEmpty
                  | ':'  infixType;

sequenceArg       : '_' '*' ;

expr              : (bindings | id | '_')  '=>'  expr
                  | expr1 ;

expr1             : assignStmt
                  | ifStmt
                  | whileStmt
                  | tryStmt
                  | doStmt
                  | forStmt
                  | throwStmt
                  | returnStmt
                  | postfixExpr
                  | typedExprStmt
                  | matchStmt ;
//-----------------------------------------------------------------------------
ifStmt            : 'if'  '('  expr  ')'  Nl*  expr ( (SEMICOLON | {isNl()}? emptyNl)?  'else'  expr)? ;

whileStmt         : 'while'  '('  expr  ')'  Nl*  expr ;

tryStmt           : tryBlock catchBlock? finallyBlock? ;
tryBlock          : 'try' ('{'  block  '}'  |  expr) ;
catchBlock        : 'catch'  expr /*'{'  caseClauses  '}'*/ ;
finallyBlock      : 'finally'  expr ;

doStmt            : 'do'  expr  (SEMICOLON | {isNl()}? emptyNl)?  'while'  '('  expr  ')' ;

forStmt           : 'for'  ('('  enumerators  ')' | '{'  enumerators  '}')  Nl*  'yield'?  expr ;

throwStmt         : 'throw'  expr ;

returnStmt        : 'return'  expr? ;

assignStmt        : postfixExpr '=' expr ;

typedExprStmt     : postfixExpr  ascription ;

matchStmt         : postfixExpr  'match'  '{'  caseClauses  '}' ;
//-----------------------------------------------------------------------------
postfixExpr       : infixExpr ( {!isNl()}? id  Nl?)? ;

/*infixExpr         : infixExpr  id  Nl?  infixExpr
                  | prefixExpr ;*/
infixExpr         : prefixExpr (id typeArgs? Nl? prefixExpr)* ;

prefixExpr        : ('-' | '+' | '~' | '!')? simpleExpr ;

simpleExpr        : newTemplate | blockExpr | placeholderExpr | simpleExpr1 ;

placeholderExpr   : simpleExpr1 '_' ;

newTemplate       : 'new' extendsBlock ;
extendsBlock      : classTemplate | templateBody ;

simpleExpr1       : literal
                  | '_'
                  | pathRefExpr
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

block             : blockStat ( (SEMICOLON | {isNl()}? emptyNl)  blockStat )*
                  | blockStat ( (SEMICOLON | {isNl()}? emptyNl)  blockStat )* resultExpr
                  | ;

blockNode         : block ;

blockStat         : import_
                  | 'implicit'? def
                  | tmplDef
                  | expr1
                  ; // | ;

resultExpr        : bindings  '=>'  blockNode
                  | ('implicit'?  id | '_')  ':'  paramType '=>'  blockNode ;

enumerators       : generator  ( (SEMICOLON | {isNl()}? emptyNl)  enumerator)* ;

enumerator        : generator // no enumerator
                  | guard // no enumerator
                  | 'val'? pattern1 '=' expr ; // enumerator

//generator         : pattern1  '<-'  expr ( semi?  guard |  semi  pattern1  '='  expr)* ;
generator         : generatorNoGuard guard? ;

generatorNoGuard  : pattern1  '<-'  expr ;

caseClauses       : caseClause+ ;

caseClause        : 'case'  pattern  guard?  '=>'  blockNode ;
  
guard             : 'if'  postfixExpr ;

pattern           : pattern1 ( VDASH  pattern1 )* ;

pattern1          : typedPattern
                  | pattern2 ;

typedPattern      : {isVarId()}? ID  ':'  typePat
                  | '_'  ':'  typePat ;

pattern2          : {isVarId()}? referencePattern
                  | namingPattern
                  | pattern3 ;

referencePattern  : ID ;
namingPattern     : ID '@' pattern3 ;

pattern3          : simplePattern
                  | simplePattern ( /*{getCurrentToken().getText().compareTo("|") != 0}?*/ (id  Nl?  simplePattern))* ;
//-----------------------------------------------------------------------------
simplePattern     : wildcardPattern
                  | tuplePattern
                  | {isVarId()}? referencePattern
                  | literalPattern
                  | stableReferencePattern
                  | constructorPattern
                  | patternInParenthesis ;

wildcardPattern   : '_' ;

patternInParenthesis
                  : '(' pattern ')' ;

tuplePattern      : '(' patterns? ')' ;

literalPattern    : literal ;

stableReferencePattern
                  : stableIdRefExpr ;

constructorPattern: stableIdRef patternArgs ;

patternArgs       : '('  pattern ( ','  pattern)*  ')'
                  | '('  (pattern ( ','  pattern)*  ',' )? namingPattern2  ')'
                  | '('  (pattern ( ','  pattern)*  ',' )? seqWildcard  ')';

namingPattern2    : ID  '@'  seqWildcard ;

//-----------------------------------------------------------------------------
patterns          : patternSeq
                  | seqWildcard ;

patternSeq        : pattern ( ','  pattern)+ ;
seqWildcard       : '_' '*' ;

typeParamClause   : '['  variantTypeParam ( ','  variantTypeParam)*  ']' ;

variantTypeParam  : annotationsNonEmpty? (OP_1|OP_2)? (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)? ('<%'  type)* ( ':'  type)* ;

funTypeParamClause: '['  typeParam ( ','  typeParam)*  ']' ;

typeParam         : annotationsNonEmpty? (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)?
                    ('<%'  type)* ( ':'  type)* ;
                         
paramClauses      : implicitParamClause
                  | paramClause* implicitParamClause?;

implicitParamClause
                  : ( Nl?  '('  'implicit'  params  ')') ;

paramClause       : Nl?  '('  params? ')'  ;

params            : param ( ','  param)* ;

param             : annotations emptyModifiers id  ( ':'  paramType)? ( '='  expr)? ;

emptyModifiers    : ;

paramType         : type 
                  | '=>'  type
                  | type  '*';

classParamClauses : implicitClassParamClause
                  | classParamClause* implicitClassParamClause? ;

implicitClassParamClause
                  : Nl?  '(' 'implicit'  classParams ')' ;

classParamClause  : Nl?  '(' classParams? ')'  ;

classParams       : classParam ( ','  classParam)* ;

classParam        : annotations  modifiersOrEmpty  ( 'val' |  'var')?
                    id  ':'  paramType ( '='  expr)? ;
                    
bindings          : '('  binding ( ','  binding )*  ')' ;

binding           : (id | '_') ( ':'  paramType)? ;

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

annotationExpr    : constrAnnotation;

constrAnnotation  : simpleType argumentExprs*;

annotations       : annotation* ;
annotationsNonEmpty
                  : ({!isNl()}? annotation)+ ;

templateBody      : Nl?  '{'  selfType?  templateStat? ( (SEMICOLON | {isNl()}? emptyNl)  templateStat)*  '}' ;

templateStat      : import_
                  | def
                  | dcl
                  | expr
                  ;// | ;
                  
selfType          : id ( ':'  type)?  '=>' 
                  | 'this'  ':'  type  '=>' ;

import_           : 'import'  importExpr ( ','  importExpr)* ;

importExpr        : stableIdRef ('.' '_' | '.' importSelectors)
                  | stableIdRef ;

importSelectors   : '{'  ( importSelector  ',')* ('_' | importSelector)  '}' ;

importSelector    : reference ( '=>'  id |  '=>'  '_')? ;
 
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
                  | 'this'  paramClauses
                    ('='  constrExpr |  {isSingleNl()}? Nl  constrBlock) ;

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

constr            : annotType  ({!isNl()}? argumentExprs)* ;

earlyDefs         : '{'  (patVarDef ( (SEMICOLON | {isNl()}? emptyNl)  patVarDef )* )?  '}'  'with' ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{'  selfInvocation ( (SEMICOLON | {isNl()}? emptyNl)  blockStat)*  '}' ;
selfInvocation    : 'this'  argumentExprs+ ;

topStatSeq        : topStat ( (SEMICOLON | {isNl()}? emptyNl)  topStat)*
                  | ;

topStat           : tmplDef
                  | import_
                  | packaging
                  | packageObject
                  ;// | ;
                    
packaging         : 'package'  qualId  Nl?  '{'  topStatSeq  '}' ;

packageObject     : emptyAnnotations emptyModifiers 'package'  'object'  objectDef ;

emptyAnnotations  : ;

compilationUnit   : packageDcl? topStatSeq ;

packageDcl        : 'package'  qualId  (SEMICOLON | {isNl()}? emptyNl)? packageDcl? ;

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

semi              :  SEMICOLON | Nl+ ;

// Lexer
VDASH       :  '|';
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

