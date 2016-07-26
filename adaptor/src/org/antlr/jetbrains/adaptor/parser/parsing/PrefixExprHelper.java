package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class PrefixExprHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (canDrop(ctx)) marker.drop();
        else marker.done(ScalaElementTypes.PREFIX_EXPR());
    }

    private Boolean canDrop(ParserRuleContext ctx) {
        String c = ctx.getChild(0).getText();
        return !(c.compareTo("+") == 0 || c.compareTo("-") == 0 || c.compareTo("!") == 0 || c.compareTo("~") == 0);
    }
}
