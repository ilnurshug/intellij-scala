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

public class ModifierHelper implements Helper {
    //private final Deque<PsiBuilder.Marker> hMarkers = new ArrayDeque<PsiBuilder.Marker>();

    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {

        /*int countOfModifierChild = 0;
        int pos = -1;

        RuleNode parent = ctx.getParent();
        for (int i = 0; i < parent.getChildCount(); i++) {
            ParseTree ch = parent.getChild(i);
            if (ch instanceof RuleNode
                    && ((RuleNode) ch).getRuleContext().getRuleIndex() == ScalaLangParser.RULE_modifier)
            {
                if (((RuleNode) ch).getRuleContext() == ctx) pos = countOfModifierChild;

                countOfModifierChild++;
            }
        }

        if (pos == -1) {
            assert false;
        }

        if (pos == 0) {
            hMarkers.push(marker.precede());
        }
        if (pos == countOfModifierChild - 1) {
            hMarkers.pop().done(ScalaElementTypes.MODIFIERS());
        }

        marker.drop();
        //marker.done(ScalaElementTypes.MODIFIERS());*/
        marker.drop();
    }
}
