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

public class DefHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (hasRuleNode(ctx, ScalaLangParser.RULE_funDef))
            marker.done(ScalaElementTypes.FUNCTION_DEFINITION());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_patDef))
            marker.done(ScalaElementTypes.PATTERN_DEFINITION());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_typeDef))
            marker.done(ScalaElementTypes.TYPE_DEFINITION());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_varDef))
            marker.done(ScalaElementTypes.VARIABLE_DEFINITION());
        else marker.drop();
    }
}
