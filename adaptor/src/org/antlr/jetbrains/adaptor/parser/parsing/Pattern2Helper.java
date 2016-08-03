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

public class Pattern2Helper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (hasTerminalNode(ctx, ScalaLangParser.AT)) {
            marker.done(ScalaElementTypes.NAMING_PATTERN());
        }
        else if (!hasTerminalNode(ctx, ScalaLangParser.AT)
                && !hasRuleNode(ctx, ScalaLangParser.RULE_pattern3)
                && ctx.getParent().getRuleIndex() == ScalaLangParser.RULE_patternList)
        {
            marker.done(ScalaElementTypes.REFERENCE_PATTERN());
        }
        else {
            marker.drop();
        }
    }
}
