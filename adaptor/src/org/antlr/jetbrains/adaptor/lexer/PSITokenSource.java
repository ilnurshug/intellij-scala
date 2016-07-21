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
		int type = convertScalaTokenTypeToInt(builder.getTokenType());

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

	private int convertScalaTokenTypeToInt(IElementType t) {
		if (t == null) return Token.EOF;
		else if (t == ScalaTokenTypes.tSEMICOLON) return ScalaLangParser.SEMICOLON;
		else if (t == ScalaTokenTypes.tLBRACE) return ScalaLangParser.LBRACE;
		else if (t == ScalaTokenTypes.tRBRACE) return ScalaLangParser.RBRACE;
		else if (t == ScalaTokenTypes.tLSQBRACKET) return ScalaLangParser.LSQBRACKET;
		else if (t == ScalaTokenTypes.tRSQBRACKET) return ScalaLangParser.RSQBRACKET;
		else if (t == ScalaTokenTypes.tLPARENTHESIS) return ScalaLangParser.LPARENTHESIS;
		else if (t == ScalaTokenTypes.tRPARENTHESIS) return ScalaLangParser.RPARENTHESIS;
		else if (t == ScalaTokenTypes.tDOT) return ScalaLangParser.DOT;
		else if (t == ScalaTokenTypes.tCOMMA) return ScalaLangParser.COMMA;
		//else if (t == ScalaTokenTypes.tRBRACE) return 10;
		//else if (t == ScalaTokenTypes.tRBRACE) return 11;
		else if (t == ScalaTokenTypes.tCOLON) return ScalaLangParser.COLON;
		else if (t == ScalaTokenTypes.tUNDER) return ScalaLangParser.UNDER;
		else if (t == ScalaTokenTypes.tASSIGN) return ScalaLangParser.ASSIGN;
		else if (t == ScalaTokenTypes.tFUNTYPE) return ScalaLangParser.FUNTYPE;
		else if (t == ScalaTokenTypes.tCHOOSE) return ScalaLangParser.CHOOSE;
		else if (t == ScalaTokenTypes.tUPPER_BOUND) return ScalaLangParser.UPPER_BOUND;
		else if (t == ScalaTokenTypes.tLOWER_BOUND) return ScalaLangParser.LOWER_BOUND;
		else if (t == ScalaTokenTypes.tINNER_CLASS) return ScalaLangParser.INNER_CLASS;
		else if (t == ScalaTokenTypes.tAT) return ScalaLangParser.AT;
		else if (t == ScalaTokenTypes.tVIEW) return ScalaLangParser.VIEW;
		//else if (t == ScalaTokenTypes.tSLASH) return 23;
		else if (t == ScalaTokenTypes.kABSTRACT) return ScalaLangParser.ABSTRACT;
		else if (t == ScalaTokenTypes.kCASE) return ScalaLangParser.CASE;
		else if (t == ScalaTokenTypes.kCATCH) return ScalaLangParser.CATCH;
		else if (t == ScalaTokenTypes.kCLASS) return ScalaLangParser.CLASS;
		else if (t == ScalaTokenTypes.kDEF) return ScalaLangParser.DEF;
		else if (t == ScalaTokenTypes.kDO) return ScalaLangParser.DO;
		else if (t == ScalaTokenTypes.kOBJECT) return ScalaLangParser.OBJECT;
		else if (t == ScalaTokenTypes.kPACKAGE) return ScalaLangParser.PACKAGE;
		else if (t == ScalaTokenTypes.kTHROW) return ScalaLangParser.THROW;
		else if (t == ScalaTokenTypes.kNEW) return ScalaLangParser.NEW;
		else if (t == ScalaTokenTypes.tIDENTIFIER) return ScalaLangParser.ID;
		else return 1;
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
