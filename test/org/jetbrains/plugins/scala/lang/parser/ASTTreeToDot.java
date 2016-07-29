package org.jetbrains.plugins.scala.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.util.containers.HashMap;

/**
 * Created by user on 7/21/16.
 * converts AST tree to dot format graph
 */
public class ASTTreeToDot {

    public ASTTreeToDot() {

    }

    public String convert(ASTNode t) {
        time = 0;
        map = new HashMap<ASTNode, Integer>();
        return "digraph G {\nordering=out;\n" + dfs(t) + "\n}";
    }


    private String dfs(ASTNode t) {
        if (t == null) return "";

        if (!map.containsKey(t)) {
            map.put(t, time);
            time++;
        }

        int tmp = map.get(t);

        String s = "";

        for (ASTNode ch = t.getFirstChildNode(); ch != null; ch = ch.getTreeNext()) {
            s += dfs(ch);

            s += name(t.getElementType().toString()) + tmp
                    + " -> "
                    + name(ch.getElementType().toString()) + map.get(ch)
                    + ";\n";

        }

        return s;
    }

    private String name(String s) {
        String res = s.replace(' ', '_');
        if (!Character.isLetter(res.charAt(0))) return "T";
        else return res;
    }

    private int time;
    private HashMap<ASTNode, Integer> map;
}
