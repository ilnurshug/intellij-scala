package org.antlr.jetbrains.adaptor.parser.parsing;

import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;

/**
 * Created by user on 7/28/16.
 */
public class ParserRuleNodeContextHelper {

    boolean hasRuleNode(ParserRuleContext ctx, int ruleType) {
        boolean f = false;

        for (int i = 0; i < ctx.getChildCount(); i++)
        {
            ParseTree c = ctx.getChild(i);
            if (c instanceof RuleNode && ((RuleNode) c).getRuleContext().getRuleIndex() == ruleType) f = true;
        }
        return f;
    }

    boolean hasTerminalNode(ParserRuleContext ctx, int tokenType) {
        boolean f = false;

        for (int i = 0; i < ctx.getChildCount(); i++)
        {
            ParseTree c = ctx.getChild(i);
            if (c instanceof TerminalNode && ((TerminalNode)c).getSymbol().getType() == tokenType) f = true;
        }
        return f;
    }
}
