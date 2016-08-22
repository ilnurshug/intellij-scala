	
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
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypesEx;
import org.jetbrains.plugins.scala.lang.lexer.ScalaXmlTokenTypes;
import com.intellij.psi.tree.*;
import org.jetbrains.plugins.scala.lang.parser.*;
import org.antlr.jetbrains.adaptor.lexer.*;
import java.util.*;
import com.intellij.lang.PsiBuilder;
import org.jetbrains.plugins.scala.lang.*;
}

@parser::members {

String originalText = null;
PsiBuilder builder = null;

Deque<Boolean> newlinesEnabled = null;

void disableNewlines() {
    newlinesEnabled.push(false);
}

void enableNewlines() {
    newlinesEnabled.push(true);
}

void restoreNewlinesState() {
    if (newlinesEnabled.isEmpty()) {
        System.out.println("newlinesEnabled stack is empty");
    }
    assert(!newlinesEnabled.isEmpty());
    newlinesEnabled.pop();
}

boolean isVarId() {
    boolean f = Character.isLowerCase(getCurrentToken().getText().charAt(0));
    return f;
}

@Override
public void setTokenStream(TokenStream input) {
    this._input = null;
    this.reset();
    this._input = input;

    if (input == null) {
        builder = null;
        originalText = "";
    }
    else {
        builder = ((PSITokenSource)_input.getTokenSource()).getBuilder();
        originalText = builder.getOriginalText().toString();
    }
    //System.out.println(originalText);

    newlinesEnabled = new ArrayDeque<Boolean>();
}

int getOccurrenceCount(char c) {
    return getOccurrenceCount(c, 1);
}

int getOccurrenceCount(char c, int offset) {
    if (builder == null) return 0;

    int pos = builder.rawTokenIndex();

    int prev = (offset == 1 ? -1 : offset - 1);
    //System.out.println(pos);

    CustomPSITokenSource.CommonTokenAdaptor curToken = (CustomPSITokenSource.CommonTokenAdaptor)_input.LT(offset);
    CustomPSITokenSource.CommonTokenAdaptor prevToken = (CustomPSITokenSource.CommonTokenAdaptor)_input.LT(prev);

    if (curToken == null || prevToken == null) return 0;

    int prevTokenStart = prevToken.rawTokenIndex();
    int curTokenStart = curToken.rawTokenIndex();

    //System.out.println(originalText.substring(prevToken.getStartIndex(), curToken.getStartIndex()));

    int firstPos = (prevTokenStart < pos ? prevTokenStart - pos : pos - prevTokenStart);
    int count = 0;
    for (int i = prevTokenStart + 1, delta = 1; i < curTokenStart; i++, delta++) {
        IElementType t = builder.rawLookup(firstPos + delta);

        if (ScalaTokenTypes.COMMENTS_TOKEN_SET.contains(t)) continue;

        int start = builder.rawTokenTypeStart(firstPos + delta);
        int end   = builder.rawTokenTypeStart(firstPos + delta + 1);

        String substr = originalText.substring(start, end);

        //System.out.println(substr);

        count += StringUtil.getOccurrenceCount(substr, c);
    }

    return count;
}

int countNewlineBeforeToken(int offset) {
    if (builder == null || !newlinesEnabled.isEmpty() && !newlinesEnabled.peek()) return 0;

    int pos = builder.rawTokenIndex();

    CustomPSITokenSource.CommonTokenAdaptor curToken = (CustomPSITokenSource.CommonTokenAdaptor)_input.LT(offset);

    if (curToken == null) return 0;

    int curTokenStart = curToken.rawTokenIndex();

    int firstPos = (curTokenStart < pos ? curTokenStart - pos : pos - curTokenStart);
    int i = 1;
    while (i < curTokenStart && TokenSets.WHITESPACE_OR_COMMENT_SET().contains(builder.rawLookup(firstPos - i)))
        i += 1;

    String textBefore = originalText.substring(builder.rawTokenTypeStart(firstPos - i + 1), builder.rawTokenTypeStart(firstPos));
    if (!textBefore.contains("\n")) return 0;
    String[] lines = ("start " + textBefore + " end").split("\n");

    boolean exists = false;

    for (i = 0; i < lines.length && !exists; i++) {
        boolean f = true;
        for (int j = 0; j < lines[i].length(); j++)
            f &= StringUtil.isWhiteSpace(lines[i].charAt(j));
        exists = exists || f;
    }

    return (exists ? 2 : 1);
}

boolean isSingleNl() {
    return (countNewlineBeforeToken(1) == 1);
}

boolean isSingleNlOrNone() {
    return (countNewlineBeforeToken(1) <= 1);
}

boolean isNl() {
    return (countNewlineBeforeToken(1) > 0);
}

boolean equalTo(String s) {
    Token curToken = getCurrentToken();
    if (curToken == null) return false;

    return curToken.getText().compareTo(s) == 0;
}

boolean lookAhead(IElementType... tokens) {
    for (int i = 0; i < tokens.length; i++) {
        CustomPSITokenSource.CommonTokenAdaptor t = (CustomPSITokenSource.CommonTokenAdaptor)_input.LT(i + 1);

        if (t == null || t.getTokenType() != tokens[i]) return false;
    }
    return true;
}

}

testRule          : id ({isNl()}? emptyNl id)+ ;

emptyNl           :  ;

testRule2         : {lookAhead(ScalaTokenTypes.kTHIS) && !lookAhead(ScalaTokenTypes.kTHIS, ScalaTokenTypes.tDOT)}? thisReference;

program           : blockExpr
                  | selfType?  templateStatSeq    // for debug purposes
                  | compilationUnit
                  | block;                                            // for debug purposes

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
                  
qualId            : qualId '.' id | id ;

ids               : fieldId (  ','  fieldId)* ;
fieldId           : id ;

//pathRef_          : {lookAhead() && !lookAhead()}? thisReference;

pathRef           :  stableIdRef '.' id
                  |  thisReference '.' id
                  |  superReference '.' id
                  //|  thisReference
                  |  id ;

pathRefExpr       :  stableIdRefExpr '.' id
                  |  thisReference '.' id
                  |  superReference '.' id
                  //|  thisReference
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
                  | infixType
                  | existentialType
                  | wildcardType ;

typeType          : infixType  '=>'  type ;

wildcardType      : '_' ('>:' type)? ('<:' type)? ;

existentialType   : infixType  existentialClause ;

existentialClause : 'forSome'  '{' {enableNewlines();}  existentialDcl ( (SEMICOLON | {isNl()}? emptyNl)  existentialDcl)*  '}' {restoreNewlinesState();} ;

existentialDcl    : typeDeclaration
                  | valueDeclaration;

infixType         : compoundOrWildType ( {!isNl()}? id  Nl?  compoundOrWildType )*;

compoundOrWildType: wildcardType2 | compoundType ;

wildcardType2     : '_' ;

compoundType      : annotType ( 'with'  annotType)*  refinement?
                  | refinement;

annotType         : simpleType  annotationsNonEmpty?;

simpleType        : simpleType  typeArgs
                  | simpleType '#' id
                  | simpleTypeSub ;

simpleTypeSub     : stableIdRef
                  | (kThisReference | thisReference | pathRef) '.' 'type'
                  | '(' ')'
                  | '(' {disableNewlines();} types ','? ')' {restoreNewlinesState();} ;

kThisReference    : 'this' ;

simpleTypeNoMultipleSQBrackets
                  : simpleTypeSub typeArgs
                  | simpleTypeNoMultipleSQBrackets '#' id typeArgs
                  | simpleTypeNoMultipleSQBrackets '#' id
                  | simpleTypeSub ;

typeArgs          : '[' {disableNewlines();} type ( ','  type)*  ']' {restoreNewlinesState();} ;

types             : ( '=>'  type | type  '*' | type) ( ','  ( '=>'  type | type  '*' | type))*;

refinement        : Nl? '{' {enableNewlines();} refineStatSeq  '}' {restoreNewlinesState();} ;

refineStatSeq     : refineStat refineStatSub
                  | refineStatSub
                  | ;

refineStatSub     : {isNl()}? emptyNl refineStat refineStatSub
                  | SEMICOLON refineStat refineStatSub
                  | SEMICOLON refineStatSub
                  | ;

refineStat        : dcl
                  | typeDefinition
                  ; //| ;

typePat           : type;

ascription        : ':'  sequenceArg
                  | ':'  type
                  | ':'  annotationsNonEmpty;

sequenceArg       : '_' '*' ;

expr              : (bindings | id | '_')  '=>'  expr
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
                  /*| assignStmt
                  | typedExprStmt
                  | matchStmt
                  | postfixExpr ;*/

expr1Sub          : postfixExpr (
                        '=' expr
                      | ascription
                      | 'match'  '{'  caseClauses  '}'
                    )?;
//-----------------------------------------------------------------------------
exprInParen       : '(' {disableNewlines();} expr ')' {restoreNewlinesState();} ;

ifStmt            : 'if'  exprInParen Nl*  expr ( (SEMICOLON | {isNl()}? emptyNl)?  'else'  expr)? ;

whileStmt         : 'while'  exprInParen Nl*  expr ;

tryStmt           : tryBlock catchBlock? finallyBlock? ;
tryBlock          : 'try' ('{' {enableNewlines();} block  '}' {restoreNewlinesState();} |  expr) ;
catchBlock        : 'catch'  expr /*'{'  caseClauses  '}'*/ ;
finallyBlock      : 'finally'  expr ;

doStmt            : 'do'  expr  (SEMICOLON | {isNl()}? emptyNl)?  'while'  exprInParen ;

forStmt           : 'for'  ('(' {disableNewlines();} enumerators  ')' {restoreNewlinesState();} | '{' {enableNewlines();} enumerators  '}' {restoreNewlinesState();})  Nl*  'yield'?  expr ;

throwStmt         : 'throw'  expr ;

implicitClosure   : 'implicit' id '=>' expr ;

returnStmt        : 'return'  ({!isNl()}? expr)? ;

assignStmt        : postfixExpr '=' expr ;

typedExprStmt     : postfixExpr  ascription ;

matchStmt         : postfixExpr  'match'  '{' {enableNewlines();} caseClauses  '}' {restoreNewlinesState();} ;
//-----------------------------------------------------------------------------
postfixExpr       : infixExpr ( {!isNl()}? id  Nl?)? ;

/*infixExpr         : infixExpr  id  Nl?  infixExpr
                  | prefixExpr ;*/
infixExpr         : prefixExpr subInfixExpr ;

subInfixExpr      : {!isNl()}? id typeArgs Nl? prefixExpr subInfixExpr
                  | {!isNl() && countNewlineBeforeToken(2) <= 1}? id Nl? prefixExpr subInfixExpr
                  | ;

prefixExpr        : ('-' | '+' | '~' | '!')? simpleExpr ;

simpleExpr        : placeholderExpr
                  //| newTemplate
                  //| blockExpr
                  | simpleExpr1
                  ;

placeholderExpr   : simpleExpr1 '_' ;

newTemplate       : 'new' extendsBlock ;
extendsBlock      : classTemplate | templateBody ;

simpleExpr1       : literal
                  | '_'
                  | (thisReference | pathRefExpr)
                  | '(' {disableNewlines();} (exprs ','?)? ')' {restoreNewlinesState();}
                  | (newTemplate | blockExpr) ('.' id)?
                  | simpleExpr1 '_'? '.' id
                  | (newTemplate | blockExpr) (typeArgs)?
                  | simpleExpr1 '_'? typeArgs
                  | simpleExpr1 {!equalTo("(") || !isNl()}? argumentExprs
                  | xmlExpr;

exprs             : expr (  ','  expr)* ;

argumentExprs     : '(' {disableNewlines();}  exprs?  ')' {restoreNewlinesState();}
                  //| '(' {disableNewlines();} (exprs  ',' )? postfixExpr  ':' '_' '*' ')' {restoreNewlinesState();}
                  | Nl?  blockExpr ;
                  
blockExpr         : '{' {enableNewlines();} caseClauses  '}' {restoreNewlinesState();}
                  | '{' {enableNewlines();} block  '}' {restoreNewlinesState();} ;

block             : resultExpr
                  | blockStat subBlock
                  | subBlock
                  | ;

subBlock          : {isNl()}? emptyNl resultExpr
                  | SEMICOLON resultExpr
                  | {isNl()}? emptyNl blockStat subBlock
                  | SEMICOLON blockStat subBlock
                  | SEMICOLON subBlock
                  | ;

blockNode         : block ;

blockStat         : import_
                  | def
                  | tmplDef
                  | expr1
                  ;// | ;

resultExpr        : bindings  '=>'  blockNode
                  | ('implicit'?  id | '_')  (':'  compoundType)? '=>'  blockNode ;

enumerators       : generator  ( ((SEMICOLON | {isNl()}? emptyNl)  enumerator | guard) )* ;

enumerator        : generator // no enumerator
                  | guard // no enumerator
                  | 'val'? pattern1 '=' expr ; // enumerator

//generator         : pattern1  '<-'  expr ( semi?  guard |  semi  pattern1  '='  expr)* ;
generator         : generatorNoGuard guard? ;

generatorNoGuard  : pattern1  '<-'  expr ;

caseClauses       : caseClause+ ;

caseClause        : 'case' {disableNewlines();} pattern  guard?  '=>' {restoreNewlinesState();} blockNode ;
  
guard             : 'if'  postfixExpr ;

pattern           : pattern1 ( {equalTo("|")}? id  pattern1 )* ;

pattern1          : typedPattern
                  | pattern2 ;

typedPattern      : {isVarId()}? ID  ':'  typePat
                  | '_'  ':'  typePat ;

pattern2          : namingPattern
                  | pattern3 ;

pattern2RefPat    : referencePattern
                  | namingPattern
                  | pattern3 ;

referencePattern  : id ;
namingPattern     : ('_' | id) '@' pattern3 ;

pattern3          : simplePattern subPattern3 ;

subPattern3       : {!equalTo("|")}? id  Nl?  simplePattern subPattern3
                  | ;
//-----------------------------------------------------------------------------
simplePattern     : wildcardPattern
                  | tuplePattern
                  | {isVarId()}? referencePattern
                  | interpolationPattern
                  | literalPattern
                  | stableReferencePattern
                  | constructorPattern
                  | patternInParenthesis
                  | xmlPattern;

wildcardPattern   : '_' ;

patternInParenthesis
                  : '(' {disableNewlines();} pattern ')' {restoreNewlinesState();} ;

tuplePattern      : '(' {disableNewlines();} (patterns)? ')' {restoreNewlinesState();} ;

interpolationPattern
                  : interpolatedStringPattern;

literalPattern    : literal ;

stableReferencePattern
                  : stableIdRefExpr ;

constructorPattern: stableIdRef patternArgs ;

patternArgs       : '(' {disableNewlines();} patternArgsSub ')' {restoreNewlinesState();} ;

patternArgsSub    : (pattern ( ','  pattern)*  ',' )? seqWildcard
                  | (pattern ( ','  pattern)*  ',' )? namingPattern2
                  | pattern ( ','  pattern)*
                  | ;

namingPattern2    : ('_' | ID)  '@'  seqWildcard ;

//-----------------------------------------------------------------------------
patterns          : patternSeq
                  | seqWildcard ;

patternSeq        : pattern ( ','  (seqWildcard | pattern))+ ','?
                  | pattern ',' ;
seqWildcard       : '_' '*' ;

typeParamClause   : '[' {disableNewlines();}  variantTypeParam ( ','  variantTypeParam)*  ']' {restoreNewlinesState();} ;

variantTypeParam  : annotationsNonEmpty? (OP_1|OP_2)? (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)? ('<%'  type)* ( ':'  type)* ;

funTypeParamClause: '[' {disableNewlines();} typeParam ( ','  typeParam)*  ']' {restoreNewlinesState();} ;

typeParam         : annotationsNonEmpty? (id | '_')  typeParamClause? ( '>:'  type)? ( '<:'  type)?
                    ('<%'  type)* ( ':'  type)* ;
                         
paramClauses      : implicitParamClause
                  | paramClause* implicitParamClause?;

implicitParamClause
                  : Nl?  '(' {disableNewlines();} 'implicit'  params  ')' {restoreNewlinesState();} ;

paramClause       : Nl?  '(' {disableNewlines();} params? ')' {restoreNewlinesState();} ;

params            : param ( ','  param)* ;

param             : annotations emptyModifiers id  ( ':'  paramType)? ( '='  expr)? ;

emptyModifiers    : ;

paramType         : type 
                  | '=>'  type
                  | type  '*';

classParamClauses : implicitClassParamClause
                  | classParamClause* implicitClassParamClause? ;

implicitClassParamClause
                  : Nl?  '(' {disableNewlines();} 'implicit'  classParams ')' {restoreNewlinesState();} ;

classParamClause  : Nl?  '(' {disableNewlines();} classParams? ')' {restoreNewlinesState();} ;

classParams       : classParam ( ','  classParam)* ;

classParam        : annotations  modifiersOrEmpty  ( 'val' |  'var')?
                    id  ':'  paramType ( '='  expr)? ;
                    
bindings          : '(' {disableNewlines();} (binding ( ','  binding )*)?  ')' {restoreNewlinesState();} ;

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

accessQualifier   : '[' {disableNewlines();}  (id | 'this')  ']' {restoreNewlinesState();} ;

annotation        : '@' annotationExpr ;

annotationExpr    : constrAnnotation ( {isSingleNlOrNone()}? emptyNl '{' {enableNewlines();} (nameValuePair (',' nameValuePair)*)? '}' {restoreNewlinesState();} )?;

nameValuePair     : 'val' id '=' prefixExpr ;

constrAnnotation  : simpleType  ({!isNl()}? argumentExprsParen)*;

annotations       : (annotation)* ;
annotationsNonEmpty
                  : ({!isNl()}? annotation)+ ;

templateBody      : Nl?  '{' {enableNewlines();} selfType?  templateStatSeq  '}' {restoreNewlinesState();} ;

templateStatSeq   : templateStat templateStatSeqSub
                  | templateStatSeqSub
                  | ;

templateStatSeqSub: {isNl()}? emptyNl templateStat templateStatSeqSub
                  | SEMICOLON templateStat templateStatSeqSub
                  | SEMICOLON templateStatSeqSub
                  | ;

templateStat      : import_
                  | def
                  | dcl
                  | expr
                  ;// | ;
                  
selfType          : id ( ':'  type)?  '=>' 
                  | ('this' | '_')  ':'  type  '=>' ;

import_           : 'import'  importExpr ( ','  importExpr)* ;

importExpr        : stableIdRef ('.' '_' | '.' importSelectors)
                  | stableIdRef ;

importSelectors   : '{' {enableNewlines();} ( importSelector  ',')* ('_' | importSelector)  '}' {restoreNewlinesState();} ;

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
                  | macroDefinition
                  | functionDefinition
                  | typeDefinition
                  | templateDefinition ;

patternDefinition        : annotations modifiersOrEmpty 'val'  patDef ;
variableDefinition       : annotations modifiersOrEmpty 'var'  varDef ;
macroDefinition          : annotations modifiersOrEmpty 'def'  macroDef ;
functionDefinition       : annotations modifiersOrEmpty 'def'  funDef ;
typeDefinition           : annotations modifiersOrEmpty 'type'  Nl*  typeDef ;
templateDefinition       : tmplDef ;
                  
patDef            : patternList (':'  type)?  '='  expr ;

patternList       : pattern2RefPat ( ','  pattern2RefPat)* ;

varDef            : patDef
                  | ids  ':'  type  '='  '_' ;

macroDef          : funSig (':' type)? '=' 'macro' qualId typeArgs? ;

funDef            : 'this'  paramClauses ('='  constrExpr |  {isSingleNlOrNone()}? emptyNl constrBlock)
                  | funSig ( ':'  type)?  '='  expr
                  | funSig  Nl?  blockWithBraces ;

blockWithBraces   : '{' {enableNewlines();}  block  '}' {restoreNewlinesState();} ;

typeDef           :  id  typeParamClause?  '='  type ;

tmplDef           : classDefinition
                  | objectDefinition
                  | traitDefinition ;

classDefinition   : annotations modifiersOrCase  'class'  classDef ;

objectDefinition  : annotations modifiersOrCase  'object'  objectDef ;

traitDefinition   : annotations modifiersOrEmpty 'trait'  traitDef ;

modifiersOrCase   : modifier* 'case'? ;

classDef          : id  typeParamClause?  primaryConstructor classTemplateOpt ;

primaryConstructor: annotationsNoNl  accessModifierOrEmpty classParamClauses ;

annotationsNoNl   : {!isNl()}? annotation*
                  | ;

accessModifierOrEmpty
                  : {!isNl()}? accessModifier ?
                  | ;
                      
traitDef          : id  typeParamClause?  traitTemplateOpt ;

objectDef         : id  classTemplateOpt ;

classTemplateOpt  : ('extends'|UPPER_BOUND)  classTemplate | (('extends'|UPPER_BOUND)?  templateBody)? ;

traitTemplateOpt  : (('extends'|UPPER_BOUND)  traitTemplate)
                  | (('extends'|UPPER_BOUND)?  templateBody)? ;

classTemplate     : (earlyDefs 'with')?  classParents  templateBody? ;

traitTemplate     : (earlyDefs 'with')?  traitParents  templateBody? ;

classParents      : constr ( 'with'  annotType)* ;

traitParents      : annotType ( 'with'  annotType)* ;

constr            : annotTypeNoMultipleSQBrackets  ({!isNl()}? argumentExprsParen)* ;

annotTypeNoMultipleSQBrackets
                  : simpleTypeNoMultipleSQBrackets  annotationsNonEmpty? ;

argumentExprsParen: '(' {disableNewlines();}  exprs?  ')' {restoreNewlinesState();}
                  //| '('  (exprs  ',' )? postfixExpr  ':' '_' '*' ')'
                  ;

earlyDefs         : '{' {enableNewlines();} (patVarDef ( (SEMICOLON | {isNl()}? emptyNl)  patVarDef )* )?  '}' {restoreNewlinesState();}  ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{' {enableNewlines();} selfInvocation? blockStatSeqSub  '}' {restoreNewlinesState();} ;

blockStatSeqSub   : {isNl()}? emptyNl blockStat blockStatSeqSub
                  | SEMICOLON blockStat blockStatSeqSub
                  | SEMICOLON blockStatSeqSub
                  | ;

selfInvocation    : 'this'  (argumentExprs ({!isNl()}? argumentExprs)*)? ;

topStatSeq        : topStat topStatSeqSub
                  | topStatSeqSub
                  | ;

topStatSeqSub     : {isNl()}? emptyNl topStat topStatSeqSub
                  | SEMICOLON topStat topStatSeqSub
                  | SEMICOLON topStatSeqSub
                  | ;

topStat           : tmplDef
                  | import_
                  | packaging
                  | packageObject
                  ;// | ;
                    
packaging         : 'package'  qualId  Nl?  '{' {enableNewlines();} topStatSeq  '}' {restoreNewlinesState();} ;

packageObject     : emptyAnnotations emptyModifiers 'package'  'object'  objectDef ;

emptyAnnotations  : ;

compilationUnit   : packageDcl ;

packageDcl        : 'package'  qualId  (SEMICOLON | {isNl()}? emptyNl)? packageDcl?
                  | topStatSeq ;

id                : ID
                  | '\'' StringLiteral '\''
                  | OP_1
                  | OP_2
                  | OP_3
                  | EPT
                  | TLD
                  //| ASSIGN
                  //| UNDER
                  ;//| FUNTYPE;

semi              :  SEMICOLON | Nl+ ;

xmlExpr           :    {disableNewlines();} xmlContent (element)* {restoreNewlinesState();} ;

element           :    emptyElemTag
                  |    sTag content eTag ;

emptyElemTag      :    '<' XML_NAME (xmlAttribute)*  '/>' ;

sTag              :    '<' XML_NAME (xmlAttribute)*  '>' ;

eTag              :    '</' XML_NAME '>' ;

content           :    charData? (content1 charData?)* ;

content1          :    xmlContent
                  //|    xmlReference
                  |    scalaExpr ;

xmlContent        :    element
                  |    cDSect
                  |    pI
                  |    comment ;

comment           :    '<!--' '-->' ; // ???

cDSect            :    '<![CDATA[' (XML_DATA_CHARACTERS | scalaExpr) ']]>' ;

pI                :    '<?' XML_NAME xmlAttribute* XML_TAG_CHARACTERS? '?>' ;

xmlAttribute      :    XML_NAME XML_EQ attValue ;

attValue          :    XML_ATTRIBUTE_VALUE_START_DELIMITER (XML_ATTRIBUTE_VALUE_TOKEN | XML_CHAR_ENTITY_REF)* XML_ATTRIBUTE_VALUE_END_DELIMITER
                  |    scalaExpr ;

scalaExpr         :    SCALA_IN_XML_INJECTION_START {enableNewlines();} blockNode ';'? SCALA_IN_XML_INJECTION_END {restoreNewlinesState();} ;

charData          :    XML_DATA_CHARACTERS | XML_CHAR_ENTITY_REF ;

xmlPattern        :    {disableNewlines();} emptyElemTagP {restoreNewlinesState();}
                  |    {disableNewlines();} sTagP contentP eTagP {restoreNewlinesState();} ;

emptyElemTagP     :    '<' XML_NAME '/>' ;

sTagP             :    '<' XML_NAME '>' ;

eTagP             :    '</' XML_NAME '>' ;

contentP          :    charData? (content1P charData?)* ;

content1P         :    cDSect
                  |    comment
                  |    pI
                  //|    xmlReference
                  |    scalaPatterns
                  |    xmlPattern;

scalaPatterns     :    SCALA_IN_XML_INJECTION_START {enableNewlines();} xmlPatterns SCALA_IN_XML_INJECTION_END {restoreNewlinesState();} ;

xmlPatterns       :    patternArgsSub ;


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
EPT				:  '!';
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

