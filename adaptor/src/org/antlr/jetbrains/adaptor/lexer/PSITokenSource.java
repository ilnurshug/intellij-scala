package org.antlr.jetbrains.adaptor.lexer;

import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiBuilder;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenFactory;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.TokenFactory;
import org.antlr.v4.runtime.TokenSource;
import org.antlr.v4.runtime.misc.Pair;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes;

/** Make a PsiBuilder look like a source of ANTLR tokens. PsiBuilder
 *  provides tokens created by the lexer created in
 *  {@link ParserDefinition#createLexer(Project)}. This is the bridge
 *  between the ANTLR lexer and parser objects. Normally we just create
 *  a {@link org.antlr.v4.runtime.CommonTokenStream} but the IDE has
 *  control and asks our ParserDefinition for the lexer and parser. This
 *  is how we hook them together. When IDE ask ParserDefinition for a
 *  parser, we will create one of these attached to the PsiBuilder.
 */
public class PSITokenSource implements TokenSource {
	protected PsiBuilder builder;
	protected TokenFactory tokenFactory = CommonTokenFactory.DEFAULT;

	public PSITokenSource(PsiBuilder builder) {
		this.builder = builder;
	}

	@Override
	public int getCharPositionInLine() {
		return 0;
	}

	/** Create an ANTLR Token from the current token type of the builder
	 *  then advance the builder to next token (which ultimately calls an
	 *  ANTLR lexer).  The {@link ANTLRLexerAdaptor} creates tokens via
	 *  an ANTLR lexer but converts to {@link TokenIElementType} and here
	 *  we have to convert back to an ANTLR token using what info we
	 *  can get from the builder. We lose info such as the original channel.
	 *  So, whitespace and comments (typically hidden channel) will look like
	 *  real tokens. Jetbrains uses {@link ParserDefinition#getWhitespaceTokens()}
	 *  and {@link ParserDefinition#getCommentTokens()} to strip these before
	 *  our ANTLR parser sees them.
	 */
	@Override
	public Token nextToken() {
		ProgressIndicatorProvider.checkCanceled();
		/* from now on getTokenTypes returns types from ScalaTokenTypes
		  so we need to do something like this
		  IElementType t = builder.getTokenType();
		  if (t == ScalaTokenTypes.kABSTRACT) type = 23;
		  ...

		 */
		//TokenIElementType ideaTType = (TokenIElementType)builder.getTokenType();
		//int type = ideaTType!=null ? ideaTType.getANTLRTokenType() : Token.EOF;

		int type = convertScalaTokenTypeToInt(builder.getTokenType(), builder.getTokenText());

		int channel = Token.DEFAULT_CHANNEL;
		Pair<TokenSource, CharStream> source = new Pair<TokenSource, CharStream>(this, null);
		String text = builder.getTokenText();
		int start = builder.getCurrentOffset();
		int length = text != null ? text.length() : 0;
		int stop = start + length - 1;
		// PsiBuilder doesn't provide line, column info
		int line = 0;
		int charPositionInLine = 0;
		Token t = tokenFactory.create(source, type, text, channel, start, stop, line, charPositionInLine);
		builder.advanceLexer();
//		System.out.println("TOKEN: "+t);
		return t;
	}

	private int convertScalaTokenTypeToInt(IElementType t, String tokenText) {
		if (t == null) return Token.EOF;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// Braces ///////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		else if (t == ScalaTokenTypes.tLBRACE) return ScalaLangParser.LBRACE;
		else if (t == ScalaTokenTypes.tRBRACE) return ScalaLangParser.RBRACE;
		else if (t == ScalaTokenTypes.tLSQBRACKET) return ScalaLangParser.LSQBRACKET;
		else if (t == ScalaTokenTypes.tRSQBRACKET) return ScalaLangParser.RSQBRACKET;
		else if (t == ScalaTokenTypes.tLPARENTHESIS) return ScalaLangParser.LPARENTHESIS;
		else if (t == ScalaTokenTypes.tRPARENTHESIS) return ScalaLangParser.RPARENTHESIS;

		else if (t == ScalaTokenTypes.tDOT) return ScalaLangParser.DOT;
		else if (t == ScalaTokenTypes.tCOMMA) return ScalaLangParser.COMMA;
		else if (t == ScalaTokenTypes.tSEMICOLON) return ScalaLangParser.SEMICOLON;

		else if (t == ScalaTokenTypes.tUNDER) return ScalaLangParser.UNDER;
		else if (t == ScalaTokenTypes.tCOLON) return ScalaLangParser.COLON;
		else if (t == ScalaTokenTypes.tASSIGN) return ScalaLangParser.ASSIGN;
		else if (t == ScalaTokenTypes.tFUNTYPE) return ScalaLangParser.FUNTYPE;
		else if (t == ScalaTokenTypes.tCHOOSE) return ScalaLangParser.CHOOSE;
		else if (t == ScalaTokenTypes.tUPPER_BOUND) return ScalaLangParser.UPPER_BOUND;
		else if (t == ScalaTokenTypes.tLOWER_BOUND) return ScalaLangParser.LOWER_BOUND;
		else if (t == ScalaTokenTypes.tINNER_CLASS) return ScalaLangParser.INNER_CLASS;
		else if (t == ScalaTokenTypes.tAT) return ScalaLangParser.AT;
		else if (t == ScalaTokenTypes.tVIEW) return ScalaLangParser.VIEW;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// Strings & chars //////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		else if (t == ScalaTokenTypes.tSTRING) return ScalaLangParser.StringLiteral;
		else if (t == ScalaTokenTypes.tCHAR) return ScalaLangParser.CharacterLiteral;

		else if (t == ScalaTokenTypes.tSYMBOL) return ScalaLangParser.SymbolLiteral;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// integer and float literals ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		else if (t == ScalaTokenTypes.tINTEGER) return ScalaLangParser.IntegerLiteral;
		else if (t == ScalaTokenTypes.tFLOAT) return ScalaLangParser.FloatingPointLiteral;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// White spaces in line /////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		else if (t == ScalaTokenTypes.tWHITE_SPACE_IN_LINE) return ScalaLangParser.WHITE_SPACE_IN_LINE;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// keywords /////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		else if (t == ScalaTokenTypes.kABSTRACT) return ScalaLangParser.ABSTRACT;
		else if (t == ScalaTokenTypes.kCASE) return ScalaLangParser.CASE;
		else if (t == ScalaTokenTypes.kCATCH) return ScalaLangParser.CATCH;
		else if (t == ScalaTokenTypes.kCLASS) return ScalaLangParser.CLASS;
		else if (t == ScalaTokenTypes.kDEF) return ScalaLangParser.DEF;
		else if (t == ScalaTokenTypes.kDO) return ScalaLangParser.DO;
		else if (t == ScalaTokenTypes.kELSE) return ScalaLangParser.ELSE;
		else if (t == ScalaTokenTypes.kEXTENDS) return ScalaLangParser.EXTENDS;
		else if (t == ScalaTokenTypes.kFALSE || t == ScalaTokenTypes.kTRUE) return ScalaLangParser.BooleanLiteral;
		else if (t == ScalaTokenTypes.kFINAL) return ScalaLangParser.FINAL;
		else if (t == ScalaTokenTypes.kFINALLY) return ScalaLangParser.FINALLY;
		else if (t == ScalaTokenTypes.kFOR) return ScalaLangParser.FOR;
		else if (t == ScalaTokenTypes.kFOR_SOME) return ScalaLangParser.FOR_SOME;
		else if (t == ScalaTokenTypes.kIF) return ScalaLangParser.IF;
		else if (t == ScalaTokenTypes.kIMPLICIT) return ScalaLangParser.IMPLICIT;
		else if (t == ScalaTokenTypes.kIMPORT) return ScalaLangParser.IMPORT;
		else if (t == ScalaTokenTypes.kLAZY) return ScalaLangParser.LAZY;
		else if (t == ScalaTokenTypes.kMATCH) return ScalaLangParser.MATCH;
		else if (t == ScalaTokenTypes.kNEW) return ScalaLangParser.NEW;
		else if (t == ScalaTokenTypes.kNULL) return ScalaLangParser.NULL;
		else if (t == ScalaTokenTypes.kOBJECT) return ScalaLangParser.OBJECT;
		else if (t == ScalaTokenTypes.kOVERRIDE) return ScalaLangParser.OVERRIDE;
		else if (t == ScalaTokenTypes.kPACKAGE) return ScalaLangParser.PACKAGE;
		else if (t == ScalaTokenTypes.kPRIVATE) return ScalaLangParser.PRIVATE;
		else if (t == ScalaTokenTypes.kPROTECTED) return ScalaLangParser.PROTECTED;
		else if (t == ScalaTokenTypes.kRETURN) return ScalaLangParser.RETURN;
		else if (t == ScalaTokenTypes.kSEALED) return ScalaLangParser.SEALED;
		else if (t == ScalaTokenTypes.kSUPER) return ScalaLangParser.SUPER;
		else if (t == ScalaTokenTypes.kTHIS) return ScalaLangParser.THIS;
		else if (t == ScalaTokenTypes.kTHROW) return ScalaLangParser.THROW;
		else if (t == ScalaTokenTypes.kTRAIT) return ScalaLangParser.TRAIT;
		else if (t == ScalaTokenTypes.kTRY) return ScalaLangParser.TRY;
		else if (t == ScalaTokenTypes.kTYPE) return ScalaLangParser.TYPE;
		else if (t == ScalaTokenTypes.kVAL) return ScalaLangParser.VAL;
		else if (t == ScalaTokenTypes.kVAR) return ScalaLangParser.VAR;
		else if (t == ScalaTokenTypes.kWHILE) return ScalaLangParser.WHILE;
		else if (t == ScalaTokenTypes.kWITH) return ScalaLangParser.WITH;
		else if (t == ScalaTokenTypes.kYIELD) return ScalaLangParser.YIELD;
		else if (t == ScalaTokenTypes.kMACRO) return ScalaLangParser.MACRO;
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////// variables and constants //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
		else if (t == ScalaTokenTypes.tIDENTIFIER) return identifierTextToTokenType(tokenText);
		else return 1;
	}

	private int identifierTextToTokenType(String tokenText) {
		if 		(tokenText.compareTo("+") == 0) return ScalaLangParser.OP_1;
		else if (tokenText.compareTo("-") == 0) return ScalaLangParser.OP_2;
		else if (tokenText.compareTo("*") == 0) return ScalaLangParser.OP_3;
		else if (tokenText.compareTo("!") == 0) return ScalaLangParser.EPT;
		else if (tokenText.compareTo("~") == 0) return ScalaLangParser.TLD;
		else if (tokenText.compareTo("=") == 0) return ScalaLangParser.ASSIGN;
		else if (tokenText.compareTo("_") == 0) return ScalaLangParser.UNDER;
		else if (tokenText.compareTo("=>") == 0) return ScalaLangParser.FUNTYPE;
		else return ScalaLangParser.ID;
	}

	@Override
	public int getLine() { return 0; }

	@Override
	public CharStream getInputStream() {
		CharSequence text = builder.getOriginalText();
		return new CharSequenceCharStream(text, text.length(), getSourceName());
	}

	@Override
	public String getSourceName() {
		return CharStream.UNKNOWN_SOURCE_NAME;
	}

	@Override
	public void setTokenFactory(TokenFactory<?> tokenFactory) {
		this.tokenFactory = tokenFactory;
	}

	@Override
	public TokenFactory<?> getTokenFactory() {
		return tokenFactory;
	}
}
