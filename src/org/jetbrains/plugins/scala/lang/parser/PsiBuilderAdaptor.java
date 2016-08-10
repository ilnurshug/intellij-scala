package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.*;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.diff.FlyweightCapableTreeStructure;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes;

public class PsiBuilderAdaptor implements PsiBuilder {
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
        while (true) {
            IElementType tokenType = builder.getTokenType();

            if (tokenType == ScalaTokenTypes.tWHITE_SPACE_IN_LINE) {
                if (builder.getTokenText().contains("\n")) {
                    break;
                } else {
                    builder.advanceLexer();
                }
            } else {
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

    public PsiBuilder getDelegate() {
        return builder;
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
    public PsiBuilder.Marker mark() {
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