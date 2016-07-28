package org.antlr.jetbrains.adaptor.parser.parsing;

import com.intellij.lang.PsiBuilder;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;

import java.util.Deque;

public class AccessModifierOrEmptyHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        marker.done(ScalaElementTypes.MODIFIERS());
    }
}
