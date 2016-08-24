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

import java.util.ArrayList;

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
		return parse(builder, root, true);
	}

	public ASTNode parse(PsiBuilder builder) {
		return parse(builder, null, true);
	}

	public boolean tryParse(PsiBuilder builder) {
		parse(builder, null, false);

		SyntaxErrorListener listener = (SyntaxErrorListener)parser.getErrorListeners().get(0);
		return listener.getSyntaxErrors().isEmpty();
	}

	protected ASTNode parse(PsiBuilder builder, IElementType root, boolean buildTree) {
		ProgressIndicatorProvider.checkCanceled();

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

		ParseTreeWalker.DEFAULT.walk(listener, parseTree);

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
		if (buildTree) {
			return builder.getTreeBuilt(); // calls the ASTFactory.createComposite() etc...
		}
		else {
			return null;
		}
	}


	protected abstract ParseTree parse(Parser parser, IElementType root);

	protected ANTLRParseTreeToPSIConverter createListener(Parser parser, IElementType root, PsiBuilder builder) {
		return new ANTLRParseTreeToPSIConverter(language, parser, builder);
	}
}
