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

public class FunDefHelper implements Helper {
    private final Deque<PsiBuilder.Marker> braceMarkers = new ArrayDeque<PsiBuilder.Marker>();

    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {
        int type = node.getSymbol().getType();
        if (type == ScalaLangParser.LBRACE) {
            braceMarkers.push(builder.mark());
        }
        else if (type == ScalaLangParser.RBRACE) {

        }
    }
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        int childCount = ctx.getChildCount();
        if (childCount > 1 && ctx.getChild(childCount - 1).getText().compareTo("}") == 0) {
            if (!braceMarkers.isEmpty()) braceMarkers.pop().done(ScalaElementTypes.BLOCK_EXPR());
        }

        marker.drop();
    }
}
