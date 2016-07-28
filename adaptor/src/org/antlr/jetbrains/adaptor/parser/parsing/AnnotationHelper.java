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

public class AnnotationHelper implements Helper {

    private final Deque<PsiBuilder.Marker> hMarkers = new ArrayDeque<PsiBuilder.Marker>();

    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        int countOfAnnotationChild = 0;
        int pos = -1;

        RuleNode parent = ctx.getParent();
        for (int i = 0; i < parent.getChildCount(); i++) {
            ParseTree ch = parent.getChild(i);
            if (ch instanceof RuleNode
                    && ((RuleNode) ch).getRuleContext().getRuleIndex() == ScalaLangParser.RULE_annotation)
            {
                if (((RuleNode) ch).getRuleContext() == ctx) pos = countOfAnnotationChild;

                countOfAnnotationChild++;
            }
        }

        if (pos == -1) {
            assert false;
        }

        marker.done(ScalaElementTypes.ANNOTATION());

        if (pos == 0) {
            hMarkers.push(marker.precede());
        }
        if (pos == countOfAnnotationChild - 1) {
            hMarkers.pop().done(ScalaElementTypes.ANNOTATIONS());
        }
    }
}
