package org.antlr.jetbrains.adaptor.parser;

import com.intellij.lang.*;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.diff.FlyweightCapableTreeStructure;
import org.antlr.jetbrains.adaptor.lexer.PSITokenSource;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes;

/** An adaptor that makes an ANTLR parser look like a PsiParser. */
public abstract class ANTLRParserAdaptor implements PsiParser {
	protected final Language language;
	protected final Parser parser;

	/** Create a jetbrains adaptor for an ANTLR parser object. When
	 *  the IDE requests a {@link #parse(IElementType, PsiBuilder)},
	 *  the token stream will be set on the parser.
	 */
	public ANTLRParserAdaptor(Language language, Parser parser) {
		this.language = language;
		this.parser = parser;
	}

	public Language getLanguage() {
		return language;
	}

	@NotNull
	@Override
	public ASTNode parse(IElementType root, PsiBuilder builder) {
		return parse(builder, root);
	}

	public ASTNode parse(PsiBuilder builder) {
		return parse(builder, null);
	}


	private ASTNode parse(PsiBuilder builder, IElementType root) {
		ProgressIndicatorProvider.checkCanceled();

		builder = new PsiBuilderAdaptor(builder);

		TokenSource source = new PSITokenSource(builder);
		TokenStream tokens = new CommonTokenStream(source);
		parser.setTokenStream(tokens);
		parser.setErrorHandler(new ErrorStrategyAdaptor()); // tweaks missing tokens
		parser.removeErrorListeners();
		parser.addErrorListener(new SyntaxErrorListener()); // trap errors
		ParseTree parseTree = null;
		PsiBuilder.Marker rollbackMarker = builder.mark();
		try {
			parseTree = parse(parser, root);
		}
		finally {
			rollbackMarker.rollbackTo();
		}

		// Now convert ANTLR parser tree to PSI tree by mimicking subtree
		// enter/exit with mark/done calls. I *think* this creates their parse
		// tree (AST as they call it) when you call {@link PsiBuilder#getTreeBuilt}
		ANTLRParseTreeToPSIConverter listener = createListener(parser, root, builder);
		PsiBuilder.Marker rootMarker = null;
		if (root != null) rootMarker = builder.mark();
		CustomParseTreeWalker.DEFAULT.walk(listener, parseTree);
		while (!builder.eof()) {
			ProgressIndicatorProvider.checkCanceled();
			builder.advanceLexer();
		}
		// NOTE: parse tree returned from parse will be the
		// usual ANTLR tree ANTLRParseTreeToPSIConverter will
		// convert that to the analogous jetbrains AST nodes
		// When parsing an entire file, the root IElementType
		// will be a IFileElementType.
		//
		// When trying to rename IDs and so on, you get a
		// dummy root and a type arg identifier IElementType.
		// This results in a weird tree that has for example
		// (ID (expr (primary ID))) with the ID IElementType
		// as a subtree root as well as the appropriate leaf
		// all the way at the bottom.  The dummy ID root is a
		// CompositeElement and created by
		// ParserDefinition.createElement() despite having
		// being TokenIElementType.
		if (root != null) rootMarker.done(root);
		return builder.getTreeBuilt(); // calls the ASTFactory.createComposite() etc...
	}


	protected abstract ParseTree parse(Parser parser, IElementType root);

	protected ANTLRParseTreeToPSIConverter createListener(Parser parser, IElementType root, PsiBuilder builder) {
		return new ANTLRParseTreeToPSIConverter(language, parser, builder);
	}

	private class PsiBuilderAdaptor implements PsiBuilder {
		private final PsiBuilder builder;

		public PsiBuilderAdaptor(PsiBuilder builder) {
			this.builder = builder;
		}

		@Override
		public void advanceLexer() {
			//System.out.print("before " + builder.getTokenText());

			builder.advanceLexer();
			// данный код необходим только если в ParserDefinition в качестве WS
			// используется пустой TokenSet
			while(true) {
				IElementType tokenType = builder.getTokenType();

				if (tokenType == ScalaTokenTypes.tWHITE_SPACE_IN_LINE) {
					if (builder.getTokenText().contains("\n")) {
						break;
					}
					else {
						builder.advanceLexer();
					}
				}
				else {
					break;
				}
			}

			//System.out.println("after " + builder.getTokenText());
			//return;
		}

		@Nullable
		@Override
		public IElementType getTokenType() {
			IElementType type = builder.getTokenType();
			return type;
		}

		@Override
		public Project getProject() {
			return builder.getProject();
		}

		@Override
		public CharSequence getOriginalText() {
			return builder.getOriginalText();
		}

		@Override
		public void setTokenTypeRemapper(@Nullable ITokenTypeRemapper iTokenTypeRemapper) {
			builder.setTokenTypeRemapper(iTokenTypeRemapper);
		}

		@Override
		public void remapCurrentToken(IElementType iElementType) {
			builder.remapCurrentToken(iElementType);
		}

		@Override
		public void setWhitespaceSkippedCallback(@Nullable WhitespaceSkippedCallback whitespaceSkippedCallback) {
			builder.setWhitespaceSkippedCallback(whitespaceSkippedCallback);
		}

		@Nullable
		@Override
		public IElementType lookAhead(int i) {
			return builder.lookAhead(i);
		}

		@Nullable
		@Override
		public IElementType rawLookup(int i) {
			return builder.rawLookup(i);
		}

		@Override
		public int rawTokenTypeStart(int i) {
			return builder.rawTokenTypeStart(i);
		}

		@Override
		public int rawTokenIndex() {
			return builder.rawTokenIndex();
		}

		@Nullable
		@Override
		public String getTokenText() {
			return builder.getTokenText();
		}

		@Override
		public int getCurrentOffset() {
			return builder.getCurrentOffset();
		}

		@NotNull
		@Override
		public Marker mark() {
			return builder.mark();
		}

		@Override
		public void error(String s) {
			builder.error(s);
		}

		@Override
		public boolean eof() {
			return builder.eof();
		}

		@NotNull
		@Override
		public ASTNode getTreeBuilt() {
			return builder.getTreeBuilt();
		}

		@NotNull
		@Override
		public FlyweightCapableTreeStructure<LighterASTNode> getLightTree() {
			return builder.getLightTree();
		}

		@Override
		public void setDebugMode(boolean b) {
			builder.setDebugMode(b);
		}

		@Override
		public void enforceCommentTokens(@NotNull TokenSet tokenSet) {
			builder.enforceCommentTokens(tokenSet);
		}

		@Nullable
		@Override
		public LighterASTNode getLatestDoneMarker() {
			return builder.getLatestDoneMarker();
		}

		@Nullable
		@Override
		public <T> T getUserData(@NotNull Key<T> key) {
			return builder.getUserData(key);
		}

		@Override
		public <T> void putUserData(@NotNull Key<T> key, @Nullable T t) {
			builder.putUserData(key, t);
		}

		@Nullable
		@Override
		public <T> T getUserDataUnprotected(@NotNull Key<T> key) {
			return builder.getUserDataUnprotected(key);
		}

		@Override
		public <T> void putUserDataUnprotected(@NotNull Key<T> key, @Nullable T t) {
			builder.putUserDataUnprotected(key, t);
		}


	}
}
