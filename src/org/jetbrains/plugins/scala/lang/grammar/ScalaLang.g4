	
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

    CommonTokenAdaptor curToken = (CommonTokenAdaptor)_input.LT(offset);
    CommonTokenAdaptor prevToken = (CommonTokenAdaptor)_input.LT(prev);

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

    CommonTokenAdaptor curToken = (CommonTokenAdaptor)_input.LT(offset);

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
        CommonTokenAdaptor t = (CommonTokenAdaptor)_input.LT(i + 1);

        if (t == null || t.getTokenType() != tokens[i]) return false;
    }
    return true;
}

}

testRule          : id ({isNl()}? emptyNl id)+ ;

emptyNl           :  ;

testRule2         : {lookAhead(ScalaTokenTypes.kTHIS) && !lookAhead(ScalaTokenTypes.kTHIS, ScalaTokenTypes.tDOT)}? thisReference;

testNlRule        : id (Nl+ id)+ ;

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
                  
qualId            : qualId Nl* '.' Nl* id | id ;

ids               : fieldId ( Nl* ',' Nl* fieldId)* ;
fieldId           : id ;

//pathRef_          : {lookAhead() && !lookAhead()}? thisReference;

pathRef           :  stableIdRef Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  //|  thisReference
                  |  id ;

pathRefExpr       :  stableIdRefExpr Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  //|  thisReference
                  |  id ;

reference         : id ;
thisReference     : (reference Nl* '.' Nl*)? Nl* 'this' ;

stableIdRef       :  stableIdRef Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  |  id ;

stableIdRefExpr   :  stableIdRefExpr Nl* '.' Nl* id
                  |  thisReference Nl* '.' Nl* id
                  |  superReference Nl* '.' Nl* id
                  |  id ;

superReference    : (reference Nl* '.')? Nl* 'super' Nl* (classQualifier)? ;

classQualifier    : '[' Nl* id Nl* ']' ;

type              : typeType
                  | infixType
                  | existentialType
                  | wildcardType ;

typeType          : infixType Nl* '=>' Nl* type ;

wildcardType      : '_' Nl* ('>:' Nl* type)? Nl* ('<:' Nl* type)? ;

existentialType   : infixType Nl* existentialClause ;

existentialClause : 'forSome' Nl* '{' /*{enableNewlines();}*/ Nl* existentialDcl ( /* (SEMICOLON | {isNl()}? emptyNl) */ semi  existentialDcl)* Nl* '}' /*{restoreNewlinesState();}*/ ;

existentialDcl    : typeDeclaration
                  | valueDeclaration;

infixType         : compoundOrWildType ( /*{!isNl()}?*/ id  Nl? compoundOrWildType )*;

compoundOrWildType: wildcardType2 | compoundType ;

wildcardType2     : '_' ;

compoundType      : annotType (Nl* 'with' Nl*  annotType)* Nl* refinement?
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

types             : Nl* ( '=>' Nl* type | type Nl* '*' | type) Nl* ( ',' Nl* ( '=>' Nl* type | type Nl* '*' | type) Nl*)*;

refinement        : /*{isSingleNlOrNone()}?*/ Nl? '{' /*{enableNewlines();}*/ Nl* refineStatSeq Nl* '}' /*{restoreNewlinesState();}*/ ;

refineStatSeq     : refineStat refineStatSub
                  | refineStatSub
                  | ;

refineStatSub     : /*{isNl()}? emptyNl*/ Nl+ refineStat refineStatSub
                  | SEMICOLON refineStat refineStatSub
                  | SEMICOLON refineStatSub
                  | ;

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
                  /*| assignStmt
                  | typedExprStmt
                  | matchStmt
                  | postfixExpr ;*/

expr1Sub          : postfixExpr Nl* (
                        '=' Nl* expr
                      | ascription
                      | 'match' Nl* '{' Nl* caseClauses Nl* '}'
                    )?;

exprInParen       : '(' /*{disableNewlines();}*/ Nl* expr Nl* ')' /*{restoreNewlinesState();}*/ ;

ifStmt            : 'if' Nl* exprInParen Nl*  expr ( /*(SEMICOLON | {isNl()}? emptyNl)?*/ semi?  'else' Nl* expr)? ;

whileStmt         : 'while' Nl* exprInParen Nl*  expr ;

tryStmt           : tryBlock Nl* catchBlock? Nl* finallyBlock? ;
tryBlock          : 'try' Nl* ('{' /*{enableNewlines();}*/ Nl* block Nl* '}' /*{restoreNewlinesState();}*/ |  expr) ;
catchBlock        : 'catch' Nl*  expr ;
finallyBlock      : 'finally' Nl* expr ;

doStmt            : 'do' Nl* expr  /*(SEMICOLON | {isNl()}? emptyNl)?*/ semi?  'while' Nl* exprInParen ;

forStmt           : 'for' Nl* ('(' /*{disableNewlines();}*/ Nl* enumerators Nl* ')' /*{restoreNewlinesState();}*/ | '{' Nl* /*{enableNewlines();}*/ enumerators Nl* '}' /*{restoreNewlinesState();}*/)  Nl*  'yield'? Nl* expr ;

throwStmt         : 'throw' Nl* expr ;

implicitClosure   : 'implicit' Nl* id Nl* '=>' Nl* expr ;

returnStmt        : 'return'  (/*{!isNl()}?*/ expr)? ;

assignStmt        : postfixExpr Nl* '=' Nl* expr ;

typedExprStmt     : postfixExpr Nl* ascription ;

matchStmt         : postfixExpr Nl* 'match' Nl* '{' /*{enableNewlines();}*/ Nl* caseClauses Nl* '}' /*{restoreNewlinesState();}*/ ;

postfixExpr       : infixExpr ( /*{!isNl()}?*/ id  Nl?)? ;

infixExpr         : prefixExpr subInfixExpr ;

subInfixExpr      : /*{!isNl()}?*/ id typeArgs Nl? prefixExpr subInfixExpr
                  | /*{!isNl() && countNewlineBeforeToken(2) <= 1}?*/ id Nl? prefixExpr subInfixExpr
                  | ;

prefixExpr        : ('-' | '+' | '~' | '!')? Nl* simpleExpr ;

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
                  | simpleExpr1 /*{!equalTo("(") || !isNl()}?*/ argumentExprs
                  | xmlExpr;

exprs             : expr ( Nl* ',' Nl* expr)* ;

argumentExprs     : '(' /*{disableNewlines();}*/ Nl* exprs? Nl*  ')' /*{restoreNewlinesState();}*/
                  | /*{isSingleNlOrNone()}?*/ Nl? blockExpr ;
                  
blockExpr         : '{' /*{enableNewlines();}*/ Nl* caseClauses Nl* '}' /*{restoreNewlinesState();}*/
                  | '{' /*{enableNewlines();}*/ Nl* block Nl* '}' /*{restoreNewlinesState();}*/ ;

block             : resultExpr
                  | blockStat subBlock
                  | subBlock
                  | ;

subBlock          : /*{isNl()}?*/ Nl+ resultExpr
                  | SEMICOLON resultExpr
                  | /*{isNl()}?*/ Nl+ blockStat subBlock
                  | SEMICOLON blockStat subBlock
                  | SEMICOLON subBlock
                  | ;

blockNode         : block ;

blockStat         : import_
                  | def
                  | tmplDef
                  | expr1
                  | ;

resultExpr        : bindings Nl* '=>' Nl* blockNode
                  | ('implicit'? Nl* id | '_')  (Nl* ':' Nl* compoundType)? Nl* '=>' Nl* blockNode ;

enumerators       : generator  ( ( /*(SEMICOLON | {isNl()}? emptyNl)*/ semi  enumerator | guard) )* ;

enumerator        : generator
                  | guard
                  | 'val'? pattern1 Nl* '=' Nl* expr ;

generator         : generatorNoGuard Nl* guard? ;

generatorNoGuard  : pattern1 Nl* '<-' Nl* expr ;

caseClauses       : (caseClause Nl*)+ ;

caseClause        : 'case' /*{disableNewlines();}*/ Nl* pattern Nl* guard? Nl* '=>' Nl* /*{restoreNewlinesState();}*/ blockNode ;
  
guard             : 'if' Nl* postfixExpr ;

pattern           : pattern1 ( Nl* {equalTo("|")}? id Nl* pattern1 )* ;

pattern1          : typedPattern
                  | pattern2 ;

typedPattern      : {isVarId()}? ID Nl* ':' Nl* typePat
                  | '_' Nl* ':' Nl* typePat ;

pattern2          : namingPattern
                  | pattern3 ;

pattern2RefPat    : referencePattern
                  | namingPattern
                  | pattern3 ;

referencePattern  : id ;
namingPattern     : ('_' | id) Nl* '@' Nl* pattern3 ;

pattern3          : simplePattern subPattern3 ;

subPattern3       : {!equalTo("|")}? id  /*{isSingleNlOrNone()}?*/ Nl?  simplePattern Nl* subPattern3
                  | ;

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

namingPattern2    : ('_' | ID) Nl* '@' Nl* seqWildcard ;


patterns          : patternSeq
                  | seqWildcard ;

patternSeq        : pattern ( Nl* ',' Nl* (seqWildcard | pattern))+ Nl* ','?
                  | pattern Nl* ',' ;
seqWildcard       : '_' Nl* '*' ;

typeParamClause   : '[' /*{disableNewlines();}*/ Nl*  variantTypeParam (Nl* ',' Nl* variantTypeParam)* Nl* ']' /*{restoreNewlinesState();}*/ ;

variantTypeParam  : annotationsNonEmpty? Nl* (OP_1|OP_2)? Nl* (id | '_') Nl*  typeParamClause? (Nl* '>:' Nl* type)? (Nl* '<:' Nl* type)? (Nl* '<%' Nl* type)* (Nl* ':' Nl* type)* ;

funTypeParamClause: '[' /*{disableNewlines();}*/ Nl* typeParam (Nl* ',' Nl* typeParam)* Nl* ']' /*{restoreNewlinesState();}*/ ;

typeParam         : annotationsNonEmpty? Nl* (id | '_') Nl* typeParamClause? (Nl* '>:' Nl* type)? (Nl* '<:' Nl* type)? (Nl* '<%' Nl*  type)* (Nl* ':' Nl* type)* ;
                         
paramClauses      : implicitParamClause
                  | paramClause* implicitParamClause?;

implicitParamClause
                  : /*{isSingleNlOrNone()}?*/ Nl?  '(' /*{disableNewlines();}*/ Nl* 'implicit' Nl* params Nl* ')' /*{restoreNewlinesState();}*/ ;

paramClause       : /*{isSingleNlOrNone()}?*/ Nl?  '(' /*{disableNewlines();}*/ Nl* params? Nl* ')' /*{restoreNewlinesState();}*/ ;

params            : param ( Nl* ',' Nl* param)* ;

param             : annotations emptyModifiers Nl* id  ( Nl* ':' Nl*  paramType)? ( Nl* '=' Nl* expr)? ;

emptyModifiers    : ;

paramType         : type 
                  | '=>' Nl* type
                  | type Nl* '*';

classParamClauses : implicitClassParamClause
                  | classParamClause* implicitClassParamClause? ;

implicitClassParamClause
                  : /*{isSingleNlOrNone()}?*/ Nl?  '(' /*{disableNewlines();}*/ Nl* 'implicit' Nl* classParams Nl* ')' /*{restoreNewlinesState();}*/ ;

classParamClause  : /*{isSingleNlOrNone()}?*/ Nl?  '(' /*{disableNewlines();}*/ Nl* classParams? Nl* ')' /*{restoreNewlinesState();}*/ ;

classParams       : classParam (Nl* ',' Nl* classParam)* ;

classParam        : annotations  modifiersOrEmpty  ( 'val' |  'var')? Nl* id Nl* ':' Nl* paramType ( Nl* '=' Nl* expr)? ;
                    
bindings          : '(' /*{disableNewlines();}*/ Nl* (binding ( Nl* ',' Nl* binding )*)? Nl* ')' /*{restoreNewlinesState();}*/ ;

binding           : (id | '_') (Nl* ':' Nl* paramType)? ;

modifier          : localModifier 
                  | accessModifier
                  | 'override' ;

modifiersOrEmpty  : (modifier Nl*)* ;
                  
localModifier     : 'abstract'
                  | 'final'
                  | 'sealed'
                  | 'implicit'
                  | 'lazy' ;
                  
accessModifier    : ('private'  | 'protected' ) Nl* accessQualifier? ;

accessQualifier   : '[' /*{disableNewlines();}*/ Nl* (id | 'this') Nl* ']' /*{restoreNewlinesState();}*/ ;

annotation        : '@' Nl* annotationExpr ;

annotationExpr    : constrAnnotation ( /*{isSingleNlOrNone()}? emptyNl*/ Nl? '{' /*{enableNewlines();}*/ Nl* (nameValuePair (Nl* ',' Nl* nameValuePair)*)? Nl* '}' /*{restoreNewlinesState();}*/ )?;

nameValuePair     : 'val' Nl* id '=' Nl* prefixExpr ;

constrAnnotation  : simpleType  (/*{!isNl()}?*/ argumentExprsParen)*;

annotations       : (Nl* annotation Nl*)* ;
annotationsNonEmpty
                  : (/*{!isNl()}?*/ annotation)+ ;

templateBody      : /*{isSingleNlOrNone()}?*/ Nl?  '{' /*{enableNewlines();}*/ Nl* selfType?  templateStatSeq Nl* '}' /*{restoreNewlinesState();}*/ ;

templateStatSeq   : templateStat templateStatSeqSub
                  | templateStatSeqSub
                  | ;

templateStatSeqSub: /*{isNl()}? emptyNl*/ Nl+ templateStat templateStatSeqSub
                  | SEMICOLON templateStat templateStatSeqSub
                  | SEMICOLON templateStatSeqSub
                  | ;

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

valueDeclaration  : annotations modifiersOrEmpty Nl*  'val' Nl*   valDcl ;

variableDeclaration
                  : annotations modifiersOrEmpty Nl*  'var' Nl*  varDcl ;

functionDeclaration
                  : annotations modifiersOrEmpty Nl*  'def' Nl*  funDcl ;

typeDeclaration   : annotations modifiersOrEmpty Nl*  'type'  Nl*  typeDcl ;

valDcl            : ids Nl*  ':' Nl*  type ;

varDcl            : ids Nl*  ':' Nl*  type ;

funDcl            : funSig (Nl*  ':' Nl*  type)? ;

funSig            : id Nl*  funTypeParamClause? Nl*   paramClauses ;

typeDcl           : id Nl*  typeParamClause?  (Nl*  '>:' Nl*  type)? (Nl*  '<:' Nl*  type)? ;

patVarDef         : patternDefinition
                  | variableDefinition;

def               : patternDefinition
                  | variableDefinition
                  | macroDefinition
                  | functionDefinition
                  | typeDefinition
                  | templateDefinition ;

patternDefinition : annotations modifiersOrEmpty Nl* 'val' Nl*  patDef ;
variableDefinition: annotations modifiersOrEmpty Nl* 'var' Nl*  varDef ;
macroDefinition   : annotations modifiersOrEmpty Nl* 'def' Nl*  macroDef ;
functionDefinition: annotations modifiersOrEmpty Nl* 'def' Nl*  funDef ;
typeDefinition    : annotations modifiersOrEmpty Nl* 'type'  Nl*  typeDef ;
templateDefinition: tmplDef ;
                  
patDef            : patternList (Nl*  ':' Nl*  type)? Nl*  '=' Nl*  expr ;

patternList       : pattern2RefPat (Nl*  ',' Nl*  pattern2RefPat)* ;

varDef            : patDef
                  | ids Nl*  ':'Nl*   type Nl*  '=' Nl*  '_' ;

macroDef          : funSig (Nl*  ':' Nl*  type)? Nl*  '=' Nl*  'macro' Nl*  qualId Nl*  typeArgs? ;

funDef            : 'this' paramClauses ('=' Nl*  constrExpr |  /*{isSingleNlOrNone()}?*/ Nl? constrBlock)
                  | funSig ( Nl* ':' Nl* type)? Nl*  '=' Nl*  expr
                  | funSig  /*{isSingleNlOrNone()}?*/ Nl?  blockWithBraces ;

blockWithBraces   : '{' /*{enableNewlines();}*/ Nl*  block Nl* '}' /*{restoreNewlinesState();}*/ ;

typeDef           :  id Nl* typeParamClause? Nl* '=' Nl* type ;

tmplDef           : classDefinition
                  | objectDefinition
                  | traitDefinition ;

classDefinition   : annotations modifiersOrCase Nl* 'class' Nl* classDef ;

objectDefinition  : annotations modifiersOrCase Nl* 'object' Nl* objectDef ;

traitDefinition   : annotations modifiersOrEmpty Nl* 'trait' Nl* traitDef ;

modifiersOrCase   : (modifier Nl*)* Nl* 'case'? ;

classDef          : id Nl* typeParamClause? Nl* primaryConstructor Nl* classTemplateOpt ;

primaryConstructor: annotationsNoNl  accessModifierOrEmpty classParamClauses ;

annotationsNoNl   : /*{!isNl()}?*/ annotation*
                  | ;

accessModifierOrEmpty
                  : /*{!isNl()}?*/ accessModifier ?
                  | ;
                      
traitDef          : id Nl* typeParamClause? Nl* traitTemplateOpt ;

objectDef         : id Nl* classTemplateOpt ;

classTemplateOpt  : ('extends'|UPPER_BOUND) Nl* classTemplate | (('extends'|UPPER_BOUND)? Nl* templateBody)? ;

traitTemplateOpt  : (('extends'|UPPER_BOUND) Nl* traitTemplate)
                  | (('extends'|UPPER_BOUND)? Nl* templateBody)? ;

classTemplate     : (earlyDefs Nl* 'with')? Nl* classParents Nl* templateBody? ;

traitTemplate     : (earlyDefs Nl* 'with')? Nl* traitParents Nl* templateBody? ;

classParents      : constr ( Nl* 'with' Nl* annotType)* Nl*;

traitParents      : annotType ( Nl* 'with' Nl* annotType)* Nl* ;

constr            : annotTypeNoMultipleSQBrackets  (/*{!isNl()}?*/ argumentExprsParen)* ;

annotTypeNoMultipleSQBrackets
                  : simpleTypeNoMultipleSQBrackets annotationsNonEmpty? ;

argumentExprsParen: '(' /*{disableNewlines();}*/ Nl* exprs? Nl* ')' /*{restoreNewlinesState();}*/
                  ;

earlyDefs         : '{' /*{enableNewlines();}*/ Nl* (patVarDef ( /*(SEMICOLON | {isNl()}? emptyNl)*/ semi  patVarDef )* )? Nl* '}' /*{restoreNewlinesState();}*/  ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{' /*{enableNewlines();}*/ Nl* selfInvocation? blockStatSeqSub Nl* '}' /*{restoreNewlinesState();}*/ ;

blockStatSeqSub   : /*{isNl()}? emptyNl*/ Nl+ blockStat blockStatSeqSub
                  | SEMICOLON blockStat blockStatSeqSub
                  | SEMICOLON blockStatSeqSub
                  | ;

selfInvocation    : 'this' Nl* (argumentExprs (/*{!isNl()}?*/ argumentExprs)*)? ;

topStatSeq        : topStat topStatSeqSub
                  | topStatSeqSub
                  | ;

topStatSeqSub     : /*{isNl()}? emptyNl*/ Nl+ topStat topStatSeqSub
                  | SEMICOLON topStat topStatSeqSub
                  | SEMICOLON topStatSeqSub
                  | ;

topStat           : tmplDef
                  | import_
                  | packaging
                  | packageObject
                  | ;
                    
packaging         : 'package' Nl* qualId  /*{isSingleNlOrNone()}?*/ Nl?  '{' Nl* /*{enableNewlines();}*/ topStatSeq Nl* '}' /*{restoreNewlinesState();}*/ ;

packageObject     : emptyAnnotations emptyModifiers 'package' Nl* 'object' Nl* objectDef ;

emptyAnnotations  : ;

compilationUnit   : packageDcl ;

packageDcl        : 'package'  qualId  /*(SEMICOLON | {isNl()}? emptyNl)?*/ semi? packageDcl?
                  | topStatSeq ;

id                : ID
                  | '\'' StringLiteral '\''
                  | OP_1
                  | OP_2
                  | OP_3
                  | EPT
                  | TLD
                  ;

semi              : SEMICOLON
                  | Nl+;

xmlExpr           : /*{disableNewlines();}*/ xmlContent (Nl* element Nl*)* /*{restoreNewlinesState();}*/ ;

element           : emptyElemTag
                  | sTag Nl* content Nl* eTag ;

emptyElemTag      : '<' Nl* XML_NAME (Nl* xmlAttribute Nl*)* Nl* '/>' ;

sTag              : '<' Nl* XML_NAME (Nl* xmlAttribute Nl*)* Nl* '>' ;

eTag              : '</' Nl* XML_NAME Nl* '>' ;

content           : charData? (Nl* content1 Nl* charData? Nl*)* ;

content1          : xmlContent
                  | scalaExpr ;

xmlContent        : element
                  | cDSect
                  | pI
                  | comment ;

comment           : '<!--' XML_COMMENT_CHARACTERS '-->' ;

cDSect            : '<![CDATA[' Nl* (XML_DATA_CHARACTERS | scalaExpr) Nl* ']]>' ;

pI                : '<?' Nl* XML_NAME  (Nl* xmlAttribute Nl*)* Nl* XML_TAG_CHARACTERS? Nl* '?>' ;

xmlAttribute      : XML_NAME Nl* XML_EQ Nl* attValue ;

attValue          : XML_ATTRIBUTE_VALUE_START_DELIMITER Nl* (XML_ATTRIBUTE_VALUE_TOKEN | XML_CHAR_ENTITY_REF)* Nl* XML_ATTRIBUTE_VALUE_END_DELIMITER
                  | scalaExpr ;

scalaExpr         : SCALA_IN_XML_INJECTION_START /*{enableNewlines();}*/ Nl* blockNode Nl* ';'? Nl* SCALA_IN_XML_INJECTION_END /*{restoreNewlinesState();}*/ ;

charData          : XML_DATA_CHARACTERS | XML_CHAR_ENTITY_REF ;

xmlPattern        : /*{disableNewlines();}*/ Nl* emptyElemTagP Nl* /*{restoreNewlinesState();}*/
                  | /*{disableNewlines();}*/ Nl* sTagP Nl* contentP Nl* eTagP Nl* /*{restoreNewlinesState();}*/ ;

emptyElemTagP     : '<' Nl* XML_NAME Nl* '/>' ;

sTagP             : '<' Nl* XML_NAME Nl* '>' ;

eTagP             : '</' Nl* XML_NAME Nl* '>' ;

contentP          : charData? (Nl* content1P Nl* charData? Nl*)* ;

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

