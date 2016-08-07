package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.*;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.diff.FlyweightCapableTreeStructure;
import org.antlr.jetbrains.adaptor.parser.ANTLRParserAdaptor;
import org.antlr.jetbrains.adaptor.parser.ErrorStrategyAdaptor;
import org.antlr.jetbrains.adaptor.parser.SyntaxErrorListener;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.ParseTree;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes;
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl;

/**
 * Created by ilnur on 06.08.16.
 */
public abstract class CustomANTLRParserAdaptor extends ANTLRParserAdaptor {
    public CustomANTLRParserAdaptor(Language language, Parser parser) {
        super(language, parser);
    }

    @Override
    protected ASTNode parse(PsiBuilder builder, IElementType root, boolean buildTree) {
        ProgressIndicatorProvider.checkCanceled();

        builder = new PsiBuilderAdaptor(builder);

        TokenSource source = new CustomPSITokenSource(builder);

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

        PsiBuilder.Marker rootMarker = null;
        if (root != null) rootMarker = builder.mark();

        //ParseTreeWalker.DEFAULT.walk(listener, parseTree);
        ScalaLangVisitorImpl visitor = new ScalaLangVisitorImpl(builder);
        visitor.visit(parseTree);

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
