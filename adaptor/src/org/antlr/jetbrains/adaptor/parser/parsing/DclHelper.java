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

public class DclHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (hasRuleNode(ctx, ScalaLangParser.RULE_funDcl))
            marker.done(ScalaElementTypes.FUNCTION_DECLARATION());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_valDcl))
            marker.done(ScalaElementTypes.VALUE_DECLARATION());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_typeDcl))
            marker.done(ScalaElementTypes.TYPE_DECLARATION());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_varDcl))
            marker.done(ScalaElementTypes.VARIABLE_DECLARATION());
        else marker.drop();
    }
}
