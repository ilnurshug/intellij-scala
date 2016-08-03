package org.antlr.jetbrains.adaptor.parser.parsing;

import com.intellij.lang.PsiBuilder;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;

import java.util.Deque;

/**
 * Created by user on 8/3/16.
 */
public class SimplePatternHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {

    }

    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, Deque<PsiBuilder.Marker> markers) {
        if (hasTerminalNode(ctx, ScalaLangParser.UNDER)) {   // '_'
            marker.done(ScalaElementTypes.WILDCARD_PATTERN());
        }
        else if (ctx.getChild(0).getText().compareTo("(") == 0) { // '(' patterns? ')'
            marker.done(ScalaElementTypes.TUPLE_PATTERN());
        }
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_literal)) { // literal
            marker.done(ScalaElementTypes.LITERAL_PATTERN());
        }
        else if (ctx.getChild(0) instanceof TerminalNode) {   // varid
            marker.done(ScalaElementTypes.REFERENCE_PATTERN());
        }
        else if (ctx.getChildCount() == 1 && hasRuleNode(ctx, ScalaLangParser.RULE_stableId)) { // stableId
            marker.done(ScalaElementTypes.REFERENCE_EXPRESSION());
            marker.precede().done(ScalaElementTypes.STABLE_REFERENCE_PATTERN());
        }
        else {
            marker.done(ScalaElementTypes.CONSTRUCTOR_PATTERN());
        }
    }
}
