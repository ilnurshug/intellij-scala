package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonToken;
import org.antlr.v4.runtime.TokenSource;

public class CommonTokenAdaptor extends CommonToken {
    private final CommonToken token;
    private final IElementType type;
    private final int rawTokenIndex;

    public CommonTokenAdaptor(CommonToken token, IElementType type, int rawTokenIndex) {
        super(-1);
        this.token = token;
        this.type = type;
        this.rawTokenIndex = rawTokenIndex;
    }

    public IElementType getTokenType() {
        return type;
    }

    public int rawTokenIndex() {
        return rawTokenIndex;
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
