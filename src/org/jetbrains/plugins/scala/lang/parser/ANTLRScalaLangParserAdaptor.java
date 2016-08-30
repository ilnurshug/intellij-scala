package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
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

    public static final ScalaLangParser PARSER = new ScalaLangParser(null);
    public static final Language LANGUAGE = ScalaLanguage.Instance;

    private ANTLRScalaLangParserAdaptor() {
    }

    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        return new CustomANTLRParserAdaptor(LANGUAGE, PARSER) {
            @Override
            protected ParseTree parse(Parser parser, IElementType root) {
                return ((ScalaLangParser) parser).program();
            }
        }.parse(root, builder);
    }

    public boolean parse(PsiBuilder builder, final Callable<ParserRuleContext> rule) {
        return new CustomANTLRParserAdaptor(LANGUAGE, PARSER) {
            @Override
            protected ParseTree parse(Parser parser, IElementType root) {
                try {
                    return rule.call();
                }
                catch (Exception e) {
                    System.out.print(e.getMessage());
                    return null;
                }
            }
        }.tryParse(builder);
    }
}


