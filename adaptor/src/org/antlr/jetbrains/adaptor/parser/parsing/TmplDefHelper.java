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

public class TmplDefHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        if (hasRuleNode(ctx, ScalaLangParser.RULE_classDef))
            marker.done(ScalaElementTypes.CLASS_DEF());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_objectDef))
            marker.done(ScalaElementTypes.OBJECT_DEF());
        else if (hasRuleNode(ctx, ScalaLangParser.RULE_traitDef))
            marker.done(ScalaElementTypes.TRAIT_DEF());
        else marker.drop();
    }
}
