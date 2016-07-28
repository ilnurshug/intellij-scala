package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class BlockHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        RuleNode parent = (RuleNode)ctx.getParent();
        int parentRule = parent.getRuleContext().getRuleIndex();

        if (parentRule == ScalaLangParser.RULE_resultExpr
                || parentRule == ScalaLangParser.RULE_caseClause)
        {
            // можно не проверять на наличае у родителя в качестве ребенка терминального узла '=>'
            // так как в правилах resultExpr и caseClause символ block всегда находится после '=>'
            //if (ctx.getChildCount() > 1) marker.done(ScalaElementTypes.BLOCK());
            //else marker.drop();
            marker.done(ScalaElementTypes.BLOCK());
        }
        else if (parentRule == ScalaLangParser.RULE_blockExpr
                || parentRule == ScalaLangParser.RULE_funDef
                || parentRule == ScalaLangParser.RULE_program)
        {
            marker.drop();
        }
        else {
            marker.done(ScalaElementTypes.BLOCK());
        }
        //marker.drop();
    }
}
