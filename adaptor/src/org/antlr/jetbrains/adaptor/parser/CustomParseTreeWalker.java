package org.antlr.jetbrains.adaptor.parser;

import org.antlr.v4.runtime.tree.*;

import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by user on 8/3/16.
 */
public class CustomParseTreeWalker extends ParseTreeWalker {
    public static final CustomParseTreeWalker DEFAULT = new CustomParseTreeWalker();

    private final HashMap<Integer, WalkProc> walks = new HashMap<Integer, WalkProc>();

    public void registerWalker(int rule, WalkProc walk) {
        walks.put(rule, walk);
    }

    public void walk(ANTLRParseTreeToPSIConverter listener, ParseTree t) {
        if (t instanceof RuleNode) {
            int rule = ((RuleNode) t).getRuleContext().getRuleIndex();
            if (walks.containsKey(rule)) {
                walks.get(rule).walk(listener, t);
            }
            else {
                super_walk(listener, t);
            }
        }
        else {
            super_walk(listener, t);
        }
    }

    private void super_walk(ANTLRParseTreeToPSIConverter listener, ParseTree t) {
        if(t instanceof ErrorNode) {
            listener.visitErrorNode((ErrorNode)t);
        } else if(t instanceof TerminalNode) {
            listener.visitTerminal((TerminalNode)t);
        } else {
            RuleNode r = (RuleNode)t;
            enterRule(listener, r);
            int n = r.getChildCount();

            for(int i = 0; i < n; ++i) {
                walk(listener, r.getChild(i));
            }

            exitRule(listener, r);
        }
    }

    public interface WalkProc {
        void walk(ANTLRParseTreeToPSIConverter listener, ParseTree t);
    }
}
