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

public class AscriptionHelper implements Helper {
    private final Deque<PsiBuilder.Marker> hMarkers = new ArrayDeque<PsiBuilder.Marker>();

    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {
        if (node.getSymbol().getType() == ScalaLangParser.UNDER) {
            hMarkers.push(builder.mark());
        }
    }
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (!hMarkers.isEmpty()) {
            hMarkers.pop().done(ScalaElementTypes.SEQUENCE_ARG());
        }

        marker.drop();
    }
}
