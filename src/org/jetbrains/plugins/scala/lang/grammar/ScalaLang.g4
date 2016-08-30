	
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
                  | selfType?  templateStatSeq
                  | compilationUnit
                  | block;

literal           : '-'? IntegerLiteral
                  | '-'? FloatingPointLiteral
                  | interpolatedStringPartReference InterpolatedString ( STRING_INJECTION (blockExpr | referenceExp | thisReference) | InterpolatedString )* InterpolatedStringEnd
                  | interpolatedStringPartReference InterpolatedMultilineString ( STRING_INJECTION (blockExpr | referenceExp | thisReference) | InterpolatedMultilineString )* InterpolatedStringEnd
                  | BooleanLiteral
                  | CharacterLiteral
                  | StringLiteral
                  | MultilineStringLiteral
                  | SymbolLiteral
                  | 'null' ;

referenceExp      : id ;

interpolatedStringPartReference
                  : id ;

interpolatedStringPattern
                  : interpolatedPrefixPatternReference stringPatternArgs InterpolatedStringEnd;

interpolatedPrefixPatternReference
                  : id ;

stringPatternArgs : InterpolatedString (STRING_INJECTION (referencePattern | '{' pattern '}') | InterpolatedString)*
                  | InterpolatedMultilineString (STRING_INJECTION (referencePattern | '{' pattern '}') | InterpolatedMultilineString)*;
                  
qualId            : qualId Nl* '.' Nl* id | id ;

ids               : fieldId ( Nl* ',' Nl* fieldId)* ;
fieldId           : id ;

pathRef           :  stableIdRef Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  |  id ;

pathRefExpr       :  stableIdRefExpr Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  |  id ;

reference         : id ;
thisReference     : (reference Nl* '.' Nl*)? 'this' ;

stableIdRef       :  stableIdRef Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  |  id ;

stableIdRefExpr   :  stableIdRefExpr Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  |  id ;

superReference    : (reference Nl* '.' Nl*)? 'super' (Nl* classQualifier)? ;

classQualifier    : '[' Nl* id Nl* ']' ;

type              : typeType
                  | infixType
                  | existentialType
                  | wildcardType ;

typeType          : infixType Nl* '=>' Nl* type ;

wildcardType      : '_' (Nl* '>:' Nl* type)? (Nl* '<:' Nl* type)? ;

existentialType   : infixType Nl* existentialClause ;

existentialClause : 'forSome' Nl* '{' /*{enableNewlines();}*/ Nl* (existentialDcl ( semi existentialDcl)*)? Nl* '}' /*{restoreNewlinesState();}*/ ;

existentialDcl    : typeDeclaration
                  | valueDeclaration;

infixType         : compoundOrWildType (id  Nl? compoundOrWildType )*;

compoundOrWildType: wildcardType2 | compoundType ;

wildcardType2     : '_' ;

compoundType      : annotType (Nl* 'with' Nl* annotType)* refinement?
                  | refinement;

annotType         : simpleType  annotationsNonEmpty?;

simpleType        : simpleType Nl* typeArgs
                  | simpleType Nl* '#' Nl* id
                  | simpleTypeSub ;

simpleTypeSub     : stableIdRef
                  | (kThisReference | thisReference | pathRef) Nl* '.' Nl* 'type'
                  | '(' Nl* ')'
                  | '(' /*{disableNewlines();}*/ Nl* types Nl* ','? Nl* ')' /*{restoreNewlinesState();}*/ ;

kThisReference    : 'this' ;

simpleTypeNoMultipleSQBrackets
                  : simpleTypeSub Nl* typeArgs
                  | simpleTypeNoMultipleSQBrackets Nl* '#' Nl* id Nl* typeArgs
                  | simpleTypeNoMultipleSQBrackets Nl* '#' Nl* id
                  | simpleTypeSub ;

typeArgs          : '[' /*{disableNewlines();}*/ Nl* type ( Nl* ',' Nl* type)* Nl* ']' /*{restoreNewlinesState();}*/ ;

types             : ( '=>' Nl* type | type Nl* '*' | type) (Nl* ',' Nl* ( '=>' Nl* type | type Nl* '*' | type) )*;

refinement        : Nl? '{' /*{enableNewlines();}*/ refineStat (semi refineStat)*  '}' /*{restoreNewlinesState();}*/ ;

refineStat        : dcl
                  | typeDefinition
                  | ;

typePat           : type;

ascription        : ':' Nl* sequenceArg
                  | ':' Nl* type
                  | ':' annotationsNonEmpty;

sequenceArg       : '_' Nl* '*' ;

expr              : (bindings | id | '_') Nl* '=>' Nl* expr
                  | expr1 ;

expr1             : ifStmt
                  | whileStmt
                  | tryStmt
                  | doStmt
                  | forStmt
                  | throwStmt
                  | implicitClosure
                  | returnStmt
                  | expr1Sub ;

expr1Sub          : postfixExpr (
                        Nl* '=' Nl* expr
                      | Nl* ascription
                      | Nl* 'match' Nl* '{' Nl* caseClauses Nl* '}'
                    )?;

exprInParen       : '(' /*{disableNewlines();}*/ Nl* expr Nl* ')' /*{restoreNewlinesState();}*/ ;

ifStmt            : 'if' Nl* exprInParen Nl*  expr ( semi? 'else' Nl* expr)? ;

whileStmt         : 'while' Nl* exprInParen Nl*  expr ;

tryStmt           : tryBlock (Nl* catchBlock)? (Nl* finallyBlock)? ;
tryBlock          : 'try' Nl* ('{' /*{enableNewlines();}*/ Nl* block Nl* '}' /*{restoreNewlinesState();}*/ |  expr) ;
catchBlock        : 'catch' Nl*  expr ;
finallyBlock      : 'finally' Nl* expr ;

doStmt            : 'do' Nl* expr semi? 'while' Nl* exprInParen ;

forStmt           : 'for' Nl* ('(' /*{disableNewlines();}*/ Nl* enumerators Nl* ')' /*{restoreNewlinesState();}*/ | '{' Nl* /*{enableNewlines();}*/ enumerators Nl* '}' /*{restoreNewlinesState();}*/)  (Nl*  'yield')? Nl* expr ;

throwStmt         : 'throw' Nl* expr ;

implicitClosure   : 'implicit' Nl* id Nl* '=>' Nl* expr ;

returnStmt        : 'return'  expr? ;

postfixExpr       : infixExpr (id  Nl?)? ;

infixExpr         : prefixExpr subInfixExpr ;

subInfixExpr      : id typeArgs? Nl? prefixExpr subInfixExpr
                  | ;

prefixExpr        : (('-' | '+' | '~' | '!') Nl*)?  simpleExpr ;

simpleExpr        : placeholderExpr
                  | simpleExpr1
                  ;

placeholderExpr   : simpleExpr1 Nl* '_' ;

newTemplate       : 'new' Nl* extendsBlock ;
extendsBlock      : classTemplate | templateBody ;

simpleExpr1       : literal
                  | '_'
                  | (thisReference | pathRefExpr)
                  | '(' /*{disableNewlines();}*/ Nl* (exprs Nl* ','?)? Nl* ')' /*{restoreNewlinesState();}*/
                  | (newTemplate | blockExpr) (Nl* '.' Nl* id)?
                  | simpleExpr1 Nl* '_'? Nl* '.' Nl* id
                  | (newTemplate | blockExpr) (Nl* typeArgs)?
                  | simpleExpr1 Nl* '_'? Nl* typeArgs
                  | simpleExpr1 argumentExprsBlock
                  | simpleExpr1 argumentExprsParen
                  | xmlExpr;

exprs             : expr ( Nl* ',' Nl* expr)* ;

argumentExprs     : '(' /*{disableNewlines();}*/ Nl* exprs? Nl*  ')' /*{restoreNewlinesState();}*/
                  | Nl? blockExpr ;

argumentExprsNoNl : '(' /*{disableNewlines();}*/ Nl* exprs? Nl*  ')' /*{restoreNewlinesState();}*/
                  | blockExpr ;

argumentExprsBlock: Nl? blockExpr ;
                  
blockExpr         : '{' /*{enableNewlines();}*/ Nl* caseClauses Nl* '}' /*{restoreNewlinesState();}*/
                  | '{' /*{enableNewlines();}*/ Nl* block Nl* '}' /*{restoreNewlinesState();}*/ ;

block             : resultExpr
                  | blockStat subBlock
                  | subBlock
                  | ;

subBlock          : Nl+ resultExpr
                  | SEMICOLON resultExpr
                  | Nl+ blockStat subBlock
                  | SEMICOLON blockStat subBlock
                  | ;

blockNode         : block ;

blockStat         : import_
                  | def
                  | tmplDef
                  | expr1
                  | ;

resultExpr        : bindings Nl* '=>' Nl* blockNode
                  | (('implicit' Nl*)? id | '_')  (Nl* ':' Nl* compoundType)? Nl* '=>' Nl* blockNode ;

enumerators       : generator  (Nl* ( semi enumerator | guard) )* ;

enumerator        : generator
                  | guard
                  | ('val' Nl*)? pattern1 Nl* '=' Nl* expr ;

generator         : generatorNoGuard (Nl* guard)? ;

generatorNoGuard  : pattern1 Nl* '<-' Nl* expr ;

caseClauses       : caseClause (Nl* caseClause)* ;

caseClause        : 'case' /*{disableNewlines();}*/ Nl* pattern Nl* guard? Nl* '=>' Nl* /*{restoreNewlinesState();}*/ blockNode ;
  
guard             : 'if' Nl* postfixExpr ;

pattern           : pattern1 ( Nl* VDASH Nl* pattern1 )* ;

pattern1          : typedPattern
                  | pattern2 ;

typedPattern      : VARID Nl* ':' Nl* typePat
                  | '_' Nl* ':' Nl* typePat ;

pattern2          : namingPattern
                  | pattern3 ;

pattern2RefPat    : referencePattern
                  | namingPattern
                  | pattern3 ;

referencePattern  : id ;

referencePatternVarId
                  : VARID ;

namingPattern     : ('_' | VARID) Nl* '@' Nl* pattern3 ;

pattern3          : simplePattern subPattern3 ;

subPattern3       : idNoVDash  Nl?  simplePattern Nl* subPattern3
                  | ;

simplePattern     : wildcardPattern
                  | tuplePattern
                  | referencePatternVarId
                  | interpolationPattern
                  | literalPattern
                  | stableReferencePattern
                  | constructorPattern
                  | patternInParenthesis
                  | xmlPattern;

wildcardPattern   : '_' ;

patternInParenthesis
                  : '(' /*{disableNewlines();}*/ Nl* pattern Nl* ')' /*{restoreNewlinesState();}*/ ;

tuplePattern      : '(' /*{disableNewlines();}*/ Nl* (patterns)? Nl* ')' /*{restoreNewlinesState();}*/ ;

interpolationPattern
                  : interpolatedStringPattern;

literalPattern    : literal ;

stableReferencePattern
                  : stableIdRefExpr ;

constructorPattern: stableIdRef Nl* patternArgs ;

patternArgs       : '(' /*{disableNewlines();}*/ Nl* patternArgsSub Nl* ')' /*{restoreNewlinesState();}*/ ;

patternArgsSub    : (pattern (Nl* ',' Nl* pattern)* Nl* ',' Nl*)? seqWildcard
                  | (pattern (Nl* ',' Nl* pattern)* Nl* ',' Nl*)? namingPattern2
                  | pattern (Nl* ',' Nl* pattern)*
                  | ;

namingPattern2    : ('_' | VARID) Nl* '@' Nl* seqWildcard ;


patterns          : patternSeq
                  | seqWildcard ;

patternSeq        : pattern ( Nl* ',' Nl* (seqWildcard | pattern))+ (Nl* ',')?
                  | pattern Nl* ',' ;
seqWildcard       : '_' Nl* '*' ;

typeParamClause   : '[' /*{disableNewlines();}*/ Nl*  variantTypeParam (Nl* ',' Nl* variantTypeParam)* Nl* ']' /*{restoreNewlinesState();}*/ ;

variantTypeParam  : (annotationsNonEmpty Nl*)?  ((OP_1|OP_2) Nl*)? (id | '_') Nl*  typeParamClause? (Nl* '>:' Nl* type)? (Nl* '<:' Nl* type)? (Nl* '<%' Nl* type)* (Nl* ':' Nl* type)* ;

funTypeParamClause: '[' /*{disableNewlines();}*/ Nl* typeParam (Nl* ',' Nl* typeParam)* Nl* ']' /*{restoreNewlinesState();}*/ ;

typeParam         : (annotationsNonEmpty Nl*)? (id | '_') (Nl* typeParamClause)? (Nl* '>:' Nl* type)? (Nl* '<:' Nl* type)? (Nl* '<%' Nl*  type)* (Nl* ':' Nl* type)* ;
                         
paramClauses      : implicitParamClause
                  | paramClause* implicitParamClause?;

implicitParamClause
                  : Nl?  '(' /*{disableNewlines();}*/ Nl* 'implicit' Nl* params Nl* ')' /*{restoreNewlinesState();}*/ ;

paramClause       : Nl?  '(' /*{disableNewlines();}*/ Nl* params? Nl* ')' /*{restoreNewlinesState();}*/ ;

params            : param ( Nl* ',' Nl* param)* ;

param             : annotations Nl* emptyModifiers Nl* id  ( Nl* ':' Nl*  paramType)? ( Nl* '=' Nl* expr)? ;

emptyModifiers    : ;

paramType         : type 
                  | '=>' Nl* type
                  | type Nl* '*';

classParamClauses : implicitClassParamClause
                  | classParamClause* implicitClassParamClause? ;

implicitClassParamClause
                  : Nl?  '(' /*{disableNewlines();}*/ Nl* 'implicit' Nl* classParams Nl* ')' /*{restoreNewlinesState();}*/ ;

classParamClause  : Nl?  '(' /*{disableNewlines();}*/ Nl* classParams? Nl* ')' /*{restoreNewlinesState();}*/ ;

classParams       : classParam (Nl* ',' Nl* classParam)* ;

classParam        : annotations Nl* modifiersOrEmpty Nl* ( 'val' |  'var')? Nl* id Nl* ':' Nl* paramType ( Nl* '=' Nl* expr)? ;
                    
bindings          : '(' /*{disableNewlines();}*/ Nl* (binding ( Nl* ',' Nl* binding )*)? Nl* ')' /*{restoreNewlinesState();}*/ ;

binding           : (id | '_') (Nl* ':' Nl* paramType)? ;

modifier          : localModifier 
                  | accessModifier
                  | 'override' ;

modifiersOrEmpty  : modifier (Nl* modifier)*
                  | ;
                  
localModifier     : 'abstract'
                  | 'final'
                  | 'sealed'
                  | 'implicit'
                  | 'lazy' ;
                  
accessModifier    : ('private'  | 'protected' ) (Nl* accessQualifier)? ;

accessQualifier   : '[' /*{disableNewlines();}*/ Nl* (id | 'this') Nl* ']' /*{restoreNewlinesState();}*/ ;

annotation        : '@' Nl* annotationExpr ;

annotationExpr    : constrAnnotation ( Nl? '{' /*{enableNewlines();}*/ Nl* (nameValuePair (Nl* ',' Nl* nameValuePair)*)? Nl* '}' /*{restoreNewlinesState();}*/ )?;

nameValuePair     : 'val' Nl* id '=' Nl* prefixExpr ;

constrAnnotation  : simpleType  argumentExprsParen*;

annotations       : annotation (Nl* annotation)*
                  | ;
annotationsNonEmpty
                  : annotation+ ;

templateBody      : Nl?  '{' /*{enableNewlines();}*/ Nl* selfType?  templateStatSeq Nl* '}' /*{restoreNewlinesState();}*/ ;

templateStatSeq   : templateStat (semi templateStat)*
                  ;
templateStat      : import_
                  | def
                  | dcl
                  | expr
                  | ;
                  
selfType          : id ( Nl* ':' Nl* type)? Nl* '=>'
                  | ('this' | '_') Nl* ':' Nl* type Nl*  '=>' ;

import_           : 'import' Nl* importExpr (Nl* ',' Nl* importExpr)* ;

importExpr        : stableIdRef ('.' Nl* '_' | '.' Nl* importSelectors)
                  | stableIdRef ;

importSelectors   : '{' /*{enableNewlines();}*/ Nl* (Nl* importSelector Nl* ',')* Nl* ('_' | importSelector) Nl* '}' /*{restoreNewlinesState();}*/ ;

importSelector    : reference (Nl*  '=>' Nl* id | Nl*  '=>' Nl*  '_')? ;
 
dcl               : valueDeclaration
                  | variableDeclaration
                  | functionDeclaration
                  | typeDeclaration ;

valueDeclaration  : annotations Nl* modifiersOrEmpty Nl*  'val' Nl*   valDcl ;

variableDeclaration
                  : annotations Nl* modifiersOrEmpty Nl*  'var' Nl*  varDcl ;

functionDeclaration
                  : annotations Nl* modifiersOrEmpty Nl*  'def' Nl*  funDcl ;

typeDeclaration   : annotations Nl* modifiersOrEmpty Nl*  'type'  Nl*  typeDcl ;

valDcl            : ids Nl*  ':' Nl*  type ;

varDcl            : ids Nl*  ':' Nl*  type ;

funDcl            : funSig (Nl*  ':' Nl*  type)? ;

funSig            : id (Nl*  funTypeParamClause)? paramClauses ;

typeDcl           : id (Nl*  typeParamClause)?  (Nl*  '>:' Nl*  type)? (Nl*  '<:' Nl*  type)? ;

patVarDef         : patternDefinition
                  | variableDefinition;

def               : patternDefinition
                  | variableDefinition
                  | macroDefinition
                  | functionDefinition
                  | typeDefinition
                  | templateDefinition ;

patternDefinition : annotations Nl* modifiersOrEmpty Nl* 'val' Nl*  patDef ;
variableDefinition: annotations Nl* modifiersOrEmpty Nl* 'var' Nl*  varDef ;
macroDefinition   : annotations Nl* modifiersOrEmpty Nl* 'def' Nl*  macroDef ;
functionDefinition: annotations Nl* modifiersOrEmpty Nl* 'def' Nl*  funDef ;
typeDefinition    : annotations Nl* modifiersOrEmpty Nl* 'type'  Nl*  typeDef ;
templateDefinition: tmplDef ;
                  
patDef            : patternList (Nl*  ':' Nl*  type)? Nl*  '=' Nl*  expr ;

patternList       : pattern2RefPat (Nl*  ',' Nl*  pattern2RefPat)* ;

varDef            : patDef
                  | ids Nl* ':' Nl* type Nl* '=' Nl* '_' ;

macroDef          : funSig (Nl*  ':' Nl*  type)? Nl*  '=' Nl*  'macro' Nl*  qualId (Nl*  typeArgs)? ;

funDef            : 'this' paramClauses ('=' Nl*  constrExpr | Nl? constrBlock)
                  | funSig ( Nl* ':' Nl* type)? Nl*  '=' Nl*  expr
                  | funSig Nl? blockWithBraces ;

blockWithBraces   : '{' /*{enableNewlines();}*/ Nl*  block Nl* '}' /*{restoreNewlinesState();}*/ ;

typeDef           : id Nl* typeParamClause? Nl* '=' Nl* type ;

tmplDef           : classDefinition
                  | objectDefinition
                  | traitDefinition ;

classDefinition   : annotations Nl* modifiersOrCase Nl* 'class' Nl* classDef ;

objectDefinition  : annotations Nl* modifiersOrCase Nl* 'object' Nl* objectDef ;

traitDefinition   : annotations Nl* modifiersOrEmpty Nl* 'trait' Nl* traitDef ;

modifiersOrCase   : (modifier Nl*)* (Nl* 'case')? ;

classDef          : id (Nl* typeParamClause)? primaryConstructor Nl* classTemplateOpt ;

primaryConstructor: annotationsNoNl  accessModifierOrEmpty classParamClauses ;

annotationsNoNl   : annotation*
                  | ;

accessModifierOrEmpty
                  : accessModifier ?
                  | ;
                      
traitDef          : id (Nl* typeParamClause)? Nl* traitTemplateOpt ;

objectDef         : id Nl* classTemplateOpt ;

classTemplateOpt  : ('extends'|UPPER_BOUND) Nl* classTemplate
                  | ('extends'|UPPER_BOUND)? templateBody
                  | ;

traitTemplateOpt  : ('extends'|UPPER_BOUND) Nl* traitTemplate
                  | ('extends'|UPPER_BOUND)? templateBody
                  | ;

classTemplate     : (earlyDefs Nl* 'with' Nl*)?  classParents templateBody? ;

traitTemplate     : (earlyDefs Nl* 'with' Nl*)? traitParents templateBody? ;

classParents      : constr ( Nl* 'with' Nl* annotType)*;

traitParents      : annotType ( Nl* 'with' Nl* annotType)* ;

constr            : annotTypeNoMultipleSQBrackets  argumentExprsParen* ;

annotTypeNoMultipleSQBrackets
                  : simpleTypeNoMultipleSQBrackets annotationsNonEmpty? ;

argumentExprsParen: '(' /*{disableNewlines();}*/ Nl* exprs? Nl* ')' /*{restoreNewlinesState();}*/ ;

earlyDefs         : '{' /*{enableNewlines();}*/ Nl* (patVarDef ( semi  patVarDef )* )? Nl* '}' /*{restoreNewlinesState();}*/  ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{' /*{enableNewlines();}*/ Nl* selfInvocation? blockStat (semi blockStat)* Nl* '}' /*{restoreNewlinesState();}*/ ;

selfInvocation    : 'this' ( Nl* argumentExprs argumentExprsNoNl*)? ;

topStatSeq        : topStat (semi topStat)*;

topStat           : tmplDef
                  | import_
                  | packaging
                  | packageObject
                  | ;
                    
packaging         : 'package' Nl* qualId  Nl?  '{' /*{enableNewlines();}*/  Nl* topStatSeq Nl* '}' /*{restoreNewlinesState();}*/ ;

packageObject     : emptyAnnotations emptyModifiers 'package' Nl* 'object' Nl* objectDef ;

emptyAnnotations  : ;

compilationUnit   : packageDcl ;

packageDcl        : Nl* 'package'  qualId  (semi packageDcl)?
                  | topStatSeq ;

id                : idNoVDash
                  | VDASH
                  ;

idNoVDash         : VARID
                  | ID
                  | '`' StringLiteral '`'
                  | OP_1
                  | OP_2
                  | OP_3
                  | EPM
                  | TLD
                  ;

semi              : SEMICOLON Nl*
                  | Nl+;

xmlExpr           : /*{disableNewlines();}*/ xmlContent Nl* element (Nl* element)* /*{restoreNewlinesState();}*/
                  | /*{disableNewlines();}*/ xmlContent /*{restoreNewlinesState();}*/;

element           : emptyElemTag
                  | sTag Nl* content Nl* eTag ;

emptyElemTag      : '<' Nl* XML_NAME (Nl* xmlAttribute)* Nl* '/>' ;

sTag              : '<' Nl* XML_NAME (Nl* xmlAttribute)* Nl* '>' ;

eTag              : '</' Nl* XML_NAME Nl* '>' ;

content           : charData? (Nl* content1 Nl* charData?)* ;

content1          : xmlContent
                  | scalaExpr ;

xmlContent        : element
                  | cDSect
                  | pI
                  | comment ;

comment           : '<!--' XML_COMMENT_CHARACTERS '-->' ;

cDSect            : '<![CDATA[' Nl* (XML_DATA_CHARACTERS | scalaExpr) Nl* ']]>' ;

pI                : '<?' Nl* XML_NAME  (Nl* xmlAttribute)* Nl* XML_TAG_CHARACTERS? Nl* '?>' ;

xmlAttribute      : XML_NAME Nl* XML_EQ Nl* attValue ;

attValue          : XML_ATTRIBUTE_VALUE_START_DELIMITER Nl* (XML_ATTRIBUTE_VALUE_TOKEN | XML_CHAR_ENTITY_REF)* Nl* XML_ATTRIBUTE_VALUE_END_DELIMITER
                  | scalaExpr ;

scalaExpr         : SCALA_IN_XML_INJECTION_START /*{enableNewlines();}*/ Nl* blockNode Nl* ';'? Nl* SCALA_IN_XML_INJECTION_END /*{restoreNewlinesState();}*/ ;

charData          : XML_DATA_CHARACTERS | XML_CHAR_ENTITY_REF ;

xmlPattern        : /*{disableNewlines();}*/ emptyElemTagP /*{restoreNewlinesState();}*/
                  | /*{disableNewlines();}*/ sTagP Nl* contentP Nl* eTagP /*{restoreNewlinesState();}*/ ;

emptyElemTagP     : '<' Nl* XML_NAME Nl* '/>' ;

sTagP             : '<' Nl* XML_NAME Nl* '>' ;

eTagP             : '</' Nl* XML_NAME Nl* '>' ;

contentP          : charData? (Nl* content1P Nl* charData?)* ;

content1P         : cDSect
                  | comment
                  | pI
                  | scalaPatterns
                  | xmlPattern;

scalaPatterns     : SCALA_IN_XML_INJECTION_START /*{enableNewlines();}*/ Nl* xmlPatterns Nl* SCALA_IN_XML_INJECTION_END /*{restoreNewlinesState();}*/ ;

xmlPatterns       : patternArgsSub ;


// Lexer
XML_START_TAG_START     : '<';
XML_EMPTY_ELEMENT_END   : '/>';
XML_TAG_END             : '>';
XML_END_TAG_START       : '</';
XML_COMMENT_START       : '<!--';
XML_COMMENT_END         : '-->';
XML_CDATA_START         : '<![CDATA[' ;
XML_CDATA_END           : ']]>' ;
XML_EQ                  : ASSIGN;
XML_PI_START            : '<?';
XML_PI_END              : '?>';
SCALA_IN_XML_INJECTION_START
                        : LBRACE ;
SCALA_IN_XML_INJECTION_END
                        : RBRACE ;
XML_ATTRIBUTE_VALUE_START_DELIMITER
                        : Q_MARK | Q_MARK_2 ;
XML_ATTRIBUTE_VALUE_END_DELIMITER
                        : Q_MARK | Q_MARK_2 ;


XML_NAME                : PrintableChar+;
XML_DATA_CHARACTERS     : PrintableChar+;
XML_TAG_CHARACTERS      : PrintableChar+;
XML_ATTRIBUTE_VALUE_TOKEN
                        : PrintableChar+;
XML_CHAR_ENTITY_REF     : PrintableChar+;
XML_COMMENT_CHARACTERS  : PrintableChar+;


VDASH           :  '|';
SEMICOLON       :  ';';
LBRACE		    :  '{';
RBRACE		    :  '}';
LSQBRACKET	    :  '[';
RSQBRACKET	    :  ']';
LPARENTHESIS	:  '(';
RPARENTHESIS	:  ')';
DOT				:  '.';
COMMA			:  ',';

Q_MARK			:  '\'';
Q_MARK_2		:  '"';

COLON			:  ':';
UNDER		    :  '_';
ASSIGN			:  '=';
FUNTYPE			:  '=>';
CHOOSE			:  '<-';
UPPER_BOUND		:  '<:';
LOWER_BOUND	    :  '>:';
INNER_CLASS		:  '#';
AT		  	    :  '@';
VIEW			:  '<%';

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
FOR_SOME		:  'forSome';
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
EPM				:  '!';
TLD				:  '~';


BooleanLiteral   :  'true' | 'false';
CharacterLiteral :  '\'' (PrintableChar | CharEscapeSeq) '\'';
StringLiteral    :  '"' StringElement* '"' ;
MultilineStringLiteral
                 : '"""' MultiLineChars '"""';

SymbolLiteral    :  '\'' ID;
IntegerLiteral   :  (DecimalNumeral | HexNumeral) ('L' | 'l')?;
FloatingPointLiteral
                 :  Digit+ '.' Digit+ ExponentPart? FloatType?
                 |  '.' Digit+ ExponentPart? FloatType?
                 |  Digit ExponentPart FloatType?
                 |  Digit+ ExponentPart? FloatType;

Nl               :  '\r'? '\n';

VARID            :  Lower Idrest ;

ID               : Op
                 | Upper Idrest
                 | Lower Idrest ;


WHITE_SPACE_IN_LINE       :  [ \t]+ ;

LINE_COMMENT : '//' .*? Nl          -> channel(HIDDEN) ;
BLOCK_COMMENT      : '/*' .*? '*/'    	-> channel(HIDDEN) ;
DOC_COMMENT : ;
SH_COMMENT : ;



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

