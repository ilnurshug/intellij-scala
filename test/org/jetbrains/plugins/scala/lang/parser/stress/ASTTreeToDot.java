package org.jetbrains.plugins.scala.lang.parser.stress;

import com.intellij.lang.ASTNode;
import com.intellij.util.containers.HashMap;

/**
 * Created by user on 7/21/16.
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

            s += t.getElementType().toString().replace(' ', '_') + tmp
                    + " -> "
                    + ch.getElementType().toString().replace(' ', '_') + map.get(ch)
                    + ";\n";

        }

        return s;
    }

    private int time;
    private HashMap<ASTNode, Integer> map;
}
