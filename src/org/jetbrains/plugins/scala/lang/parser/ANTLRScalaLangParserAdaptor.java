package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParserAdaptor;
import org.antlr.v4.runtime.Parser;
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
    private final ScalaLangParserAdaptor parserAdaptor;

    public ANTLRScalaLangParserAdaptor(Parser parser) {
        parserAdaptor = new ScalaLangParserAdaptor(ScalaLanguage.Instance, parser);
    }

    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        return parserAdaptor.parse(root, builder);
    }

    public void setStartRule(String startRule) {
        parserAdaptor.startRule = startRule;
    }

    private class ScalaLangParserAdaptor extends ANTLRParserAdaptor {
        public String startRule = "program";

        public ScalaLangParserAdaptor(Language language, Parser parser) {
            super(language, parser);
        }

        @Override
        protected ParseTree parse(Parser parser, IElementType root) {
            try {
                Method m = ((ScalaLangParser) parser).getClass().getMethod(startRule);
                return (ParseTree) m.invoke((ScalaLangParser) parser);
            }
            catch (Exception e) {
                System.out.print(e.getMessage());
                return null;
            }
        }
    }
}


