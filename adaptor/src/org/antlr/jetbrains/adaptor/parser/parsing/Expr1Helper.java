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

public class Expr1Helper extends ParserRuleNodeContextHelper implements Helper {
    protected final Deque<PsiBuilder.Marker> tryStmtMarkers = new ArrayDeque<PsiBuilder.Marker>();
    protected final Deque<IElementType> tryStmtMarkerTypes = new ArrayDeque<IElementType>();

    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {
        int type = node.getSymbol().getType();

        if (type == ScalaLangParser.TRY) {
            tryStmtMarkers.push(builder.mark());
            tryStmtMarkerTypes.push(ScalaElementTypes.TRY_BLOCK());
        }
        else if (type == ScalaLangParser.CATCH) {
            if (!tryStmtMarkers.isEmpty()) tryStmtMarkers.pop().done(tryStmtMarkerTypes.pop());

            tryStmtMarkers.push(builder.mark());
            tryStmtMarkerTypes.push(ScalaElementTypes.CATCH_BLOCK());
        }
        else if (type == ScalaLangParser.FINALLY) {
            if (!tryStmtMarkers.isEmpty()) tryStmtMarkers.pop().done(tryStmtMarkerTypes.pop());

            tryStmtMarkers.push(builder.mark());
            tryStmtMarkerTypes.push(ScalaElementTypes.FINALLY_BLOCK());
        }
    }

    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (canDropMarker(ctx)) {
            ifCanDrop(ctx, marker, markers);

            if (hasTerminalNode(ctx, ScalaLangParser.ASSIGN)) {
                marker.done(ScalaElementTypes.ASSIGN_STMT());
            }
            else {
                marker.drop();
            }

            //marker.drop();
        }
        else {
            marker.done(ScalaElementTypes.DUMMY_ELEMENT());
        }
    }

    private Boolean canDropMarker(ParserRuleContext ctx) {
        return !hasTerminalNode(ctx, ScalaLangParser.FUNTYPE);
    }

    private void ifCanDrop(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        PsiBuilder.Marker p = marker.precede();

        if (ctx.getChild(0).getText().compareTo("try") == 0) {
            if (!tryStmtMarkers.isEmpty()) {
                tryStmtMarkers.pop().done(tryStmtMarkerTypes.pop());
            }
            p.done(ScalaElementTypes.TRY_STMT());
        }
        else if (ctx.getChild(0).getText().compareTo("if") == 0)
            p.done(ScalaElementTypes.IF_STMT());
        else if (ctx.getChild(0).getText().compareTo("while") == 0)
            p.done(ScalaElementTypes.WHILE_STMT());
        else if (ctx.getChild(0).getText().compareTo("do") == 0)
            p.done(ScalaElementTypes.DO_STMT());
        else if (ctx.getChild(0).getText().compareTo("for") == 0)
            p.done(ScalaElementTypes.FOR_STMT());
        else if (ctx.getChild(0).getText().compareTo("throw") == 0)
            p.done(ScalaElementTypes.THROW_STMT());
        else if (ctx.getChild(0).getText().compareTo("return") == 0)
            p.done(ScalaElementTypes.RETURN_STMT());
        else if (hasTerminalNode(ctx, ScalaLangParser.MATCH))
            p.done(ScalaElementTypes.MATCH_STMT());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_ascription))
            p.done(ScalaElementTypes.TYPED_EXPR_STMT());
        else {
            p.drop();
        }
    }
}