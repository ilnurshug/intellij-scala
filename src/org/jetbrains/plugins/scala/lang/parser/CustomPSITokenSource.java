package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.HashMap;
import org.antlr.jetbrains.adaptor.lexer.PSITokenSource;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenSource;
import org.antlr.v4.runtime.misc.Pair;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypesEx;
import org.jetbrains.plugins.scala.lang.lexer.ScalaXmlTokenTypes;

/**
 * Created by ilnur on 06.08.16.
 */
public class CustomPSITokenSource extends PSITokenSource {

    private final HashMap<IElementType, Integer> map = new HashMap<IElementType, Integer>();

    public CustomPSITokenSource(PsiBuilder builder) {
        super(builder);
        fillMap();
    }

    private void fillMap() {
        map.put(ScalaTokenTypes.tLBRACE, ScalaLangParser.LBRACE);
        map.put(ScalaTokenTypes.tRBRACE, ScalaLangParser.RBRACE);
        map.put(ScalaTokenTypes.tLSQBRACKET, ScalaLangParser.LSQBRACKET);
        map.put(ScalaTokenTypes.tRSQBRACKET, ScalaLangParser.RSQBRACKET);
        map.put(ScalaTokenTypes.tLPARENTHESIS, ScalaLangParser.LPARENTHESIS);
        map.put(ScalaTokenTypes.tRPARENTHESIS, ScalaLangParser.RPARENTHESIS);
        map.put(ScalaTokenTypes.tDOT, ScalaLangParser.DOT);
        map.put(ScalaTokenTypes.tCOMMA, ScalaLangParser.COMMA);
        map.put(ScalaTokenTypes.tSEMICOLON, ScalaLangParser.SEMICOLON);
        map.put(ScalaTokenTypes.tUNDER, ScalaLangParser.UNDER);
        map.put(ScalaTokenTypes.tCOLON, ScalaLangParser.COLON);
        map.put(ScalaTokenTypes.tASSIGN, ScalaLangParser.ASSIGN);
        map.put(ScalaTokenTypes.tFUNTYPE, ScalaLangParser.FUNTYPE);
        map.put(ScalaTokenTypes.tCHOOSE, ScalaLangParser.CHOOSE);
        map.put(ScalaTokenTypes.tUPPER_BOUND, ScalaLangParser.UPPER_BOUND);
        map.put(ScalaTokenTypes.tLOWER_BOUND, ScalaLangParser.LOWER_BOUND);
        map.put(ScalaTokenTypes.tINNER_CLASS, ScalaLangParser.INNER_CLASS);
        map.put(ScalaTokenTypes.tAT, ScalaLangParser.AT);
        map.put(ScalaTokenTypes.tVIEW, ScalaLangParser.VIEW);
        map.put(ScalaTokenTypes.tSTRING, ScalaLangParser.StringLiteral);
        map.put(ScalaTokenTypes.tMULTILINE_STRING, ScalaLangParser.MultilineStringLiteral);
        map.put(ScalaTokenTypes.tCHAR, ScalaLangParser.CharacterLiteral);
        map.put(ScalaTokenTypes.tSYMBOL, ScalaLangParser.SymbolLiteral);
        map.put(ScalaTokenTypes.tINTEGER, ScalaLangParser.IntegerLiteral);
        map.put(ScalaTokenTypes.tFLOAT, ScalaLangParser.FloatingPointLiteral);
        //map.put(ScalaTokenTypes.tWHITE_SPACE_IN_LINE, ScalaLangParser.WHITE_SPACE_IN_LINE);
        map.put(ScalaTokenTypes.tINTERPOLATED_STRING_INJECTION, ScalaLangParser.STRING_INJECTION);
        map.put(ScalaTokenTypes.tINTERPOLATED_STRING, ScalaLangParser.InterpolatedString);
        map.put(ScalaTokenTypes.tINTERPOLATED_MULTILINE_STRING, ScalaLangParser.InterpolatedMultilineString);
        map.put(ScalaTokenTypes.tINTERPOLATED_STRING_END, ScalaLangParser.InterpolatedStringEnd);
        map.put(ScalaTokenTypes.kABSTRACT, ScalaLangParser.ABSTRACT);
        map.put(ScalaTokenTypes.kCASE, ScalaLangParser.CASE);
        map.put(ScalaTokenTypes.kCATCH, ScalaLangParser.CATCH);
        map.put(ScalaTokenTypes.kCLASS, ScalaLangParser.CLASS);
        map.put(ScalaTokenTypes.kDEF, ScalaLangParser.DEF);
        map.put(ScalaTokenTypes.kDO, ScalaLangParser.DO);
        map.put(ScalaTokenTypes.kELSE, ScalaLangParser.ELSE);
        map.put(ScalaTokenTypes.kEXTENDS, ScalaLangParser.EXTENDS);
        map.put(ScalaTokenTypes.kFALSE, ScalaLangParser.BooleanLiteral);
        map.put(ScalaTokenTypes.kTRUE, ScalaLangParser.BooleanLiteral);
        map.put(ScalaTokenTypes.kFINAL, ScalaLangParser.FINAL);
        map.put(ScalaTokenTypes.kFINALLY, ScalaLangParser.FINALLY);
        map.put(ScalaTokenTypes.kFOR, ScalaLangParser.FOR);
        map.put(ScalaTokenTypes.kFOR_SOME, ScalaLangParser.FOR_SOME);
        map.put(ScalaTokenTypes.kIF, ScalaLangParser.IF);
        map.put(ScalaTokenTypes.kIMPLICIT, ScalaLangParser.IMPLICIT);
        map.put(ScalaTokenTypes.kIMPORT, ScalaLangParser.IMPORT);
        map.put(ScalaTokenTypes.kLAZY, ScalaLangParser.LAZY);
        map.put(ScalaTokenTypes.kMATCH, ScalaLangParser.MATCH);
        map.put(ScalaTokenTypes.kNEW, ScalaLangParser.NEW);
        map.put(ScalaTokenTypes.kNULL, ScalaLangParser.NULL);
        map.put(ScalaTokenTypes.kOBJECT, ScalaLangParser.OBJECT);
        map.put(ScalaTokenTypes.kOVERRIDE, ScalaLangParser.OVERRIDE);
        map.put(ScalaTokenTypes.kPACKAGE, ScalaLangParser.PACKAGE);
        map.put(ScalaTokenTypes.kPRIVATE, ScalaLangParser.PRIVATE);
        map.put(ScalaTokenTypes.kPROTECTED, ScalaLangParser.PROTECTED);
        map.put(ScalaTokenTypes.kRETURN, ScalaLangParser.RETURN);
        map.put(ScalaTokenTypes.kSEALED, ScalaLangParser.SEALED);
        map.put(ScalaTokenTypes.kSUPER, ScalaLangParser.SUPER);
        map.put(ScalaTokenTypes.kTHIS, ScalaLangParser.THIS);
        map.put(ScalaTokenTypes.kTHROW, ScalaLangParser.THROW);
        map.put(ScalaTokenTypes.kTRAIT, ScalaLangParser.TRAIT);
        map.put(ScalaTokenTypes.kTRY, ScalaLangParser.TRY);
        map.put(ScalaTokenTypes.kTYPE, ScalaLangParser.TYPE);
        map.put(ScalaTokenTypes.kVAL, ScalaLangParser.VAL);
        map.put(ScalaTokenTypes.kVAR, ScalaLangParser.VAR);
        map.put(ScalaTokenTypes.kWHILE, ScalaLangParser.WHILE);
        map.put(ScalaTokenTypes.kWITH, ScalaLangParser.WITH);
        map.put(ScalaTokenTypes.kYIELD, ScalaLangParser.YIELD);
        map.put(ScalaTokenTypes.kMACRO, ScalaLangParser.MACRO);
        map.put(ScalaTokenTypes.tDOC_COMMENT, ScalaLangParser.DOC_COMMENT);
        map.put(ScalaTokenTypes.tLINE_COMMENT, ScalaLangParser.LINE_COMMENT);
        map.put(ScalaTokenTypes.tBLOCK_COMMENT, ScalaLangParser.BLOCK_COMMENT);
        map.put(ScalaTokenTypes.tSH_COMMENT, ScalaLangParser.SH_COMMENT);

        map.put(ScalaXmlTokenTypes.XML_START_TAG_START(), ScalaLangParser.XML_START_TAG_START);
        map.put(ScalaXmlTokenTypes.XML_NAME(), ScalaLangParser.XML_NAME);
        map.put(ScalaXmlTokenTypes.XML_EMPTY_ELEMENT_END(), ScalaLangParser.XML_EMPTY_ELEMENT_END);
        map.put(ScalaXmlTokenTypes.XML_TAG_END(), ScalaLangParser.XML_TAG_END);
        map.put(ScalaXmlTokenTypes.XML_END_TAG_START(), ScalaLangParser.XML_END_TAG_START);
        map.put(ScalaXmlTokenTypes.XML_COMMENT_START(), ScalaLangParser.XML_COMMENT_START);
        map.put(ScalaXmlTokenTypes.XML_COMMENT_END(), ScalaLangParser.XML_COMMENT_END);
        map.put(ScalaXmlTokenTypes.XML_CDATA_START(), ScalaLangParser.XML_CDATA_START);
        map.put(ScalaXmlTokenTypes.XML_CDATA_END(), ScalaLangParser.XML_CDATA_END);
        map.put(ScalaXmlTokenTypes.XML_DATA_CHARACTERS(), ScalaLangParser.XML_DATA_CHARACTERS);
        map.put(ScalaXmlTokenTypes.XML_PI_START(), ScalaLangParser.XML_PI_START);
        map.put(ScalaXmlTokenTypes.XML_PI_END(), ScalaLangParser.XML_PI_END);
        map.put(ScalaXmlTokenTypes.XML_TAG_CHARACTERS(), ScalaLangParser.XML_TAG_CHARACTERS);
        map.put(ScalaXmlTokenTypes.XML_EQ(), ScalaLangParser.XML_EQ);
        map.put(ScalaXmlTokenTypes.XML_ATTRIBUTE_VALUE_START_DELIMITER(), ScalaLangParser.XML_ATTRIBUTE_VALUE_START_DELIMITER);
        map.put(ScalaXmlTokenTypes.XML_ATTRIBUTE_VALUE_TOKEN(), ScalaLangParser.XML_ATTRIBUTE_VALUE_TOKEN);
        map.put(ScalaXmlTokenTypes.XML_CHAR_ENTITY_REF(), ScalaLangParser.XML_CHAR_ENTITY_REF);
        map.put(ScalaXmlTokenTypes.XML_ATTRIBUTE_VALUE_END_DELIMITER(), ScalaLangParser.XML_ATTRIBUTE_VALUE_END_DELIMITER);
        map.put(ScalaTokenTypesEx.SCALA_IN_XML_INJECTION_START, ScalaLangParser.SCALA_IN_XML_INJECTION_START);
        map.put(ScalaTokenTypesEx.SCALA_IN_XML_INJECTION_END, ScalaLangParser.SCALA_IN_XML_INJECTION_END);
    }

    public class CommonTokenAdaptor extends CommonToken {
        private final CommonToken token;
        private final IElementType type;

        public CommonTokenAdaptor(CommonToken token, IElementType type) {
            super(-1);
            this.token = token;
            this.type = type;
        }

        public IElementType getTokenType() {
            return type;
        }

        @Override
        public int getType() {
            return token.getType();
        }

        @Override
        public void setLine(int line) {
            token.setLine(line);
        }

        @Override
        public String getText() {
            return token.getText();
        }

        @Override
        public void setText(String text) {
            token.setText(text);
        }

        @Override
        public int getLine() {
            return token.getLine();
        }

        @Override
        public int getCharPositionInLine() {
            return token.getCharPositionInLine();
        }

        @Override
        public void setCharPositionInLine(int charPositionInLine) {
            token.setCharPositionInLine(charPositionInLine);
        }

        @Override
        public int getChannel() {
            return token.getChannel();
        }

        @Override
        public void setChannel(int channel) {
            token.setChannel(channel);
        }

        @Override
        public void setType(int type) {
            token.setType(type);
        }

        @Override
        public int getStartIndex() {
            return token.getStartIndex();
        }

        @Override
        public void setStartIndex(int start) {
            token.setStartIndex(start);
        }

        @Override
        public int getStopIndex() {
            return token.getStopIndex();
        }

        @Override
        public void setStopIndex(int stop) {
            token.setStopIndex(stop);
        }

        @Override
        public int getTokenIndex() {
            return token.getTokenIndex();
        }

        @Override
        public void setTokenIndex(int index) {
            token.setTokenIndex(index);
        }

        @Override
        public TokenSource getTokenSource() {
            return token.getTokenSource();
        }

        @Override
        public CharStream getInputStream() {
            return token.getInputStream();
        }

        @Override
        public String toString() {
            return token.toString();
        }
    }

    @Override
    public Token nextToken() {
        ProgressIndicatorProvider.checkCanceled();

        int type = convertScalaTokenTypeToInt(builder.getTokenType(), builder.getTokenText());
        return new CommonTokenAdaptor((CommonToken) nextTokenHelper(type), builder.getTokenType());
    }

    private int convertScalaTokenTypeToInt(IElementType t, String tokenText) {
        if (t == null) return Token.EOF;
        else if (t == ScalaTokenTypes.tWHITE_SPACE_IN_LINE) {
            if (tokenText.contains("\n")) return ScalaLangParser.Nl;
            else return ScalaLangParser.WHITE_SPACE_IN_LINE;
        }
        else if (t == ScalaTokenTypes.tIDENTIFIER || t == ScalaTokenTypes.tINTERPOLATED_STRING_ID) return identifierTextToTokenType(tokenText);
        else {
            if (map.containsKey(t)) return map.get(t);
            else return 1;
        }
    }

    private int identifierTextToTokenType(String tokenText) {
        if 		(tokenText.compareTo("+") == 0) return ScalaLangParser.OP_1;
        else if (tokenText.compareTo("-") == 0) return ScalaLangParser.OP_2;
        else if (tokenText.compareTo("*") == 0) return ScalaLangParser.OP_3;
        else if (tokenText.compareTo("!") == 0) return ScalaLangParser.EPT;
        else if (tokenText.compareTo("~") == 0) return ScalaLangParser.TLD;
        //else if (tokenText.compareTo("=") == 0) return ScalaLangParser.ASSIGN;
        //else if (tokenText.compareTo("_") == 0) return ScalaLangParser.UNDER;
        //else if (tokenText.compareTo("=>") == 0) return ScalaLangParser.FUNTYPE;
        else return ScalaLangParser.ID;
    }


}
