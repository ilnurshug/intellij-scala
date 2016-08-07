package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.jetbrains.plugins.scala.ScalaLanguage;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;

import java.lang.reflect.Method;

/**
 * Created by user on 7/20/16.
 */
public class ANTLRScalaLangParserAdaptor extends ScalaParser {
    public static final ANTLRScalaLangParserAdaptor INSTANCE = new ANTLRScalaLangParserAdaptor();

    private final ScalaLangParser parser = new ScalaLangParser(null);
    private final Language language = ScalaLanguage.Instance;

    private ANTLRScalaLangParserAdaptor() {
    }

    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        return new CustomANTLRParserAdaptor(language, parser) {
            @Override
            protected ParseTree parse(Parser parser, IElementType root) {
                return ((ScalaLangParser) parser).program();
            }
        }.parse(root, builder);
    }

    public boolean parse(PsiBuilder builder, final String rule) {
        return new CustomANTLRParserAdaptor(language, parser) {
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


