package org.jetbrains.plugins.scala.lang.parser;

import org.antlr.v4.runtime.CommonToken;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;

public final class NewLineToken extends CommonToken {

    public static final NewLineToken INSTANCE = new NewLineToken();

    private NewLineToken() {
        super(ScalaLangParser.Nl);
    }
}
