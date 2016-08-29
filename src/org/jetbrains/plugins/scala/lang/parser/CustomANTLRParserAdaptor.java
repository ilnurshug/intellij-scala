package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.*;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.psi.tree.IElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParserAdaptor;
import org.antlr.jetbrains.adaptor.parser.ErrorStrategyAdaptor;
import org.antlr.jetbrains.adaptor.parser.SyntaxErrorListener;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl;

public abstract class CustomANTLRParserAdaptor extends ANTLRParserAdaptor {
    public CustomANTLRParserAdaptor(Language language, Parser parser) {
        super(language, parser);
    }

    @Override
    protected ASTNode parse(PsiBuilder builder, IElementType root, boolean buildTree) {
        ProgressIndicatorProvider.checkCanceled();

        //System.out.println(builder.getOriginalText());

        TokenSource source = new CustomPSITokenSource(builder);

        TokenStream tokens = new CommonTokenStream(source);
        parser.setTokenStream(tokens);
        parser.setErrorHandler(new ErrorStrategyAdaptor()); // tweaks missing tokens
        parser.removeErrorListeners();
        parser.addErrorListener(new SyntaxErrorListener()); // trap errors
        ParseTree parseTree = null;
        PsiBuilder.Marker rollbackMarker = builder.mark();
        try {
            //long millis = System.currentTimeMillis();
            parseTree = parse(parser, root);
            //System.out.println(System.currentTimeMillis()- millis);

            //ParseTreeToDot conv = new ParseTreeToDot();
            //System.out.println(conv.convert(parseTree));
        }
        finally {
            rollbackMarker.rollbackTo();
        }

        PsiBuilder.Marker rootMarker = null;
        if (root != null) rootMarker = builder.mark();

        //ParseTreeWalker.DEFAULT.walk(listener, parseTree);
        //long millis = System.currentTimeMillis();


        ScalaLangVisitorImpl visitor = new ScalaLangVisitorImpl(language, parser, builder);
        visitor.visit(parseTree);
        //System.out.println(System.currentTimeMillis()- millis);

        while (!builder.eof()) {
            ProgressIndicatorProvider.checkCanceled();
            builder.advanceLexer();
        }

        if (root != null) rootMarker.done(root);
        if (buildTree) {
            return builder.getTreeBuilt(); // calls the ASTFactory.createComposite() etc...
        }
        else {
            return null;
        }

    }
}
