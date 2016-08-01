package org.antlr.jetbrains.adaptor.lexer;

import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiBuilder;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.HashMap;
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

	private final HashMap<IElementType, Integer> map = new HashMap<IElementType, Integer>();

	public PSITokenSource(PsiBuilder builder) {
		this.builder = builder;

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
		map.put(ScalaTokenTypes.tCHAR, ScalaLangParser.CharacterLiteral);
		map.put(ScalaTokenTypes.tSYMBOL, ScalaLangParser.SymbolLiteral);
		map.put(ScalaTokenTypes.tINTEGER, ScalaLangParser.IntegerLiteral);
		map.put(ScalaTokenTypes.tFLOAT, ScalaLangParser.FloatingPointLiteral);
		map.put(ScalaTokenTypes.tWHITE_SPACE_IN_LINE, ScalaLangParser.WHITE_SPACE_IN_LINE);
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
		else if (t == ScalaTokenTypes.tIDENTIFIER) return identifierTextToTokenType(tokenText);
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
