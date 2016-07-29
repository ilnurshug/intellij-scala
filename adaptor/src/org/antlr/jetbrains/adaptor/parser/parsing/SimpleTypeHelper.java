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

public class SimpleTypeHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (hasRuleNode(ctx, ScalaLangParser.RULE_typeArgs)) {
            marker.done(ScalaElementTypes.TYPE_GENERIC_CALL());
        }
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_id)) {
            marker.done(ScalaElementTypes.TYPE_PROJECTION());
        }
        else if (hasTerminalNode(ctx, ScalaLangParser.LPARENTHESIS)
                && hasTerminalNode(ctx, ScalaLangParser.RPARENTHESIS))
        {
            if (hasTerminalNode(ctx, ScalaLangParser.COMMA)) marker.done(ScalaElementTypes.TUPLE_TYPE());
            else marker.done(ScalaElementTypes.TYPE_IN_PARENTHESIS());
        }
        else {
            marker.done(ScalaElementTypes.SIMPLE_TYPE());
        }
    }
}
