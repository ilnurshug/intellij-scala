package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParserAdaptor;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.tree.ParseTree;
import org.jetbrains.plugins.scala.ScalaLanguage;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;

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

    private class ScalaLangParserAdaptor extends ANTLRParserAdaptor {
        public ScalaLangParserAdaptor(Language language, Parser parser) {
            super(language, parser);
        }

        @Override
        protected ParseTree parse(Parser parser, IElementType root) {
            return ((ScalaLangParser) parser).program();
        }
    }
}


