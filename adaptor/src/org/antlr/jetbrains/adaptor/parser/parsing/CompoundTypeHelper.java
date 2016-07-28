package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class CompoundTypeHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        boolean hasRefinement = false;
        boolean isCompound = false;

        for (int i = 0; i < ctx.getChildCount(); i++) {
            ParseTree c = ctx.getChild(i);
            if (c instanceof RuleNode) {
                int r = ((RuleNode) c).getRuleContext().getRuleIndex();
                if (r == ScalaLangParser.RULE_refinement) hasRefinement = true;
            }
            else {
                if (c.getText().compareTo("with") == 0) isCompound = true;
            }
        }

        if (hasRefinement || isCompound) {
            marker.done(ScalaElementTypes.COMPOUND_TYPE());
        }
        else {
            marker.drop();
        }
    }
}