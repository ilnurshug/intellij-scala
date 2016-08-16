package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.util.containers.HashMap;
import org.antlr.v4.runtime.tree.ParseTree;

/**
 * Created by user on 8/16/16.
 */
public class ParseTreeToDot {

    public String convert(ParseTree t) {
        time = 0;
        map = new HashMap<ParseTree, Integer>();
        return "digraph G {\nordering=out;\n" + dfs(t) + "\n}";
    }

    private String dfs(ParseTree t) {
        if (t == null) return "";

        if (!map.containsKey(t)) {
            map.put(t, time);
            time++;
        }

        int tmp = map.get(t);

        String s = "";

        for (int i = 0; i < t.getChildCount(); i++) {
            ParseTree ch = t.getChild(i);
            s += dfs(ch);

            s += name(t.getClass().getName()) + tmp
                    + " -> "
                    + name(ch.getClass().getName()) + map.get(ch)
                    + ";\n";

        }

        return s;
    }

    private String name(String s) {
        String res = s.replace(' ', '_');
        if (res.contains("Terminal")) return "T";
        else return res.replace('.', '_').substring(res.lastIndexOf('$')+1);
    }

    private int time;
    private HashMap<ParseTree, Integer> map;
}
