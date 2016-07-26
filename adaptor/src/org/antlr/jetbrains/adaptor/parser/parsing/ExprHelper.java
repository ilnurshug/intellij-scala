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

public class ExprHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (canDrop(ctx)) marker.drop();
        else marker.done(ScalaElementTypes.FUNCTION_EXPR());
    }

    private Boolean canDrop(ParserRuleContext ctx) {
        for (int j = 0; j < ctx.getChildCount(); j++)
            if (ctx.getChild(j).getText().compareTo("=>") == 0) return false;
        return true;
    }
}
