package org.antlr.jetbrains.adaptor.parser.parsing;

import com.intellij.lang.PsiBuilder;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;

import java.util.Deque;

public interface Helper {
    void visitTerminal(TerminalNode node, PsiBuilder builder);

    void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers);
}