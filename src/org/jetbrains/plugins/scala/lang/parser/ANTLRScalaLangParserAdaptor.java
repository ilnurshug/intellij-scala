package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParserAdaptor;
import org.antlr.jetbrains.adaptor.parser.CustomParseTreeWalker;
import org.antlr.jetbrains.adaptor.parser.parsing.InfixTypeHelper;
import org.antlr.jetbrains.adaptor.parser.parsing.Pattern3Helper;
import org.antlr.jetbrains.adaptor.parser.parsing.ResultExprHelper;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.ParseTree;
import org.jetbrains.plugins.scala.ScalaLanguage;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;

import java.lang.reflect.Method;
import java.util.concurrent.Callable;

/**
 * Created by user on 7/20/16.
 */
public class ANTLRScalaLangParserAdaptor extends ScalaParser {
    public static final ANTLRScalaLangParserAdaptor INSTANCE = new ANTLRScalaLangParserAdaptor();

    private final ScalaLangParser parser = new ScalaLangParser(null);
    private final Language language = ScalaLanguage.Instance;

    private ANTLRScalaLangParserAdaptor() {
        CustomParseTreeWalker.DEFAULT.registerWalker(ScalaLangParser.RULE_pattern3, new Pattern3Helper.Pattern3Walker());
        CustomParseTreeWalker.DEFAULT.registerWalker(ScalaLangParser.RULE_infixType, new InfixTypeHelper.InfixTypeWalker());
        CustomParseTreeWalker.DEFAULT.registerWalker(ScalaLangParser.RULE_resultExpr, new ResultExprHelper.ResultExprWalker());
    }

    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        return new ANTLRParserAdaptor(language, parser) {
            @Override
            protected ParseTree parse(Parser parser, IElementType root) {
                return ((ScalaLangParser) parser).program();
            }
        }.parse(root, builder);
    }

    public boolean parse(PsiBuilder builder, final String rule) {
        return new ANTLRParserAdaptor(language, parser) {
            @Override
            protected ParseTree parse(Parser parser, IElementType root) {
                try {
                    Method m = ((ScalaLangParser) parser).getClass().getMethod(rule);
                    return (ParseTree)m.invoke(parser);
                }
                catch (Exception e) {
                    System.out.print(e.getMessage());
                    return null;
                }
            }
        }.tryParse(builder);
    }
}


