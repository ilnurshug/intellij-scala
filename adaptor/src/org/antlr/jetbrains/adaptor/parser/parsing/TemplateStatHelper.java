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

public class TemplateStatHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (canDrop(ctx)) {
            marker.drop();
        }
        else {
            int childCount = ctx.getChildCount();
            RuleNode lastChild = (RuleNode)ctx.getChild(childCount-1);
            int ri = lastChild.getRuleContext().getRuleIndex();
            if (ri == ScalaLangParser.RULE_def) {
                ParserRuleContext lastChildContext = (ParserRuleContext)lastChild.getRuleContext();
                if (hasRuleNode(lastChildContext, ScalaLangParser.RULE_funDef))
                    marker.done(ScalaElementTypes.FUNCTION_DEFINITION());
                else if (hasRuleNode(lastChildContext, ScalaLangParser.RULE_patDef))
                    marker.done(ScalaElementTypes.PATTERN_DEFINITION());
                else if (hasRuleNode(lastChildContext, ScalaLangParser.RULE_typeDef))
                    marker.done(ScalaElementTypes.TYPE_DEFINITION());
                else if (hasRuleNode(lastChildContext, ScalaLangParser.RULE_varDef))
                    marker.done(ScalaElementTypes.VARIABLE_DEFINITION());
                else if (hasRuleNode(lastChildContext, ScalaLangParser.RULE_tmplDef))
                    marker.drop();
                else assert false;
            }
            else {
                assert false;
            }
        }
    }

    private Boolean canDrop(ParserRuleContext ctx) {
        int childCount = ctx.getChildCount();
        if (childCount == 0) return true;

        RuleNode lastChild = (RuleNode)ctx.getChild(childCount-1);
        int ri = lastChild.getRuleContext().getRuleIndex();
        return !(ri == ScalaLangParser.RULE_def);
    }
}
