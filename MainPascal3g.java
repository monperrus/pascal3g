// transforms the file passed as argument as an XML-based AST
// e.g. java MainPascal3g tangle.p

import org.antlr.runtime.ANTLRFileStream;
import org.antlr.runtime.CharStream;
import org.antlr.runtime.CommonTokenStream;
import org.antlr.runtime.Token;
import org.antlr.runtime.tree.CommonTree;
import org.antlr.runtime.tree.CommonTreeAdaptor;
import org.antlr.runtime.tree.Tree;


public class MainPascal3g {

  public static void main(String[] args) {
    try {
      CharStream input = new ANTLRFileStream(args[0]);
      pascal3gLexer lexer =
        new pascal3gLexer(input);
      CommonTokenStream tokens = new CommonTokenStream(lexer); 
      pascal3gParser parser = new pascal3gParser(tokens);

      // launch the parser using the treeadaptor
      parser.setTreeAdaptor(adaptor);

      // Print the tree
      System.out.println(printTree((CommonTree) parser.program().getTree(),0));

    } catch(Exception e) {
      System.err.println("exception: "+e);
    }
  }

  /** prints the tree t as XML, with recursive calls */
  private static String printTree(Tree t, int indent) {
    StringBuffer sb = new StringBuffer(indent);
    for (int i = 0; i < indent; i++) {
      sb = sb.append("  ");
    }
    sb.append("<"+getType(t)+" value=\""+t.toString().replaceAll("<", "&lt;").replaceAll("&", "&amp;").replaceAll("\"", "&quot;")+"\" line=\""+t.getLine()+"\">\n");
    for (int i = 0; i < t.getChildCount(); i++) {
      sb.append(printTree((CommonTree) t.getChild(i), indent + 1));
    }
    for (int i = 0; i < indent; i++) {
      sb = sb.append("  ");
    }
    sb.append("</"+getType(t)+">\n");
    return sb.toString(); 
  }

  /** returns the AST node type, e.g. "FOR" */
  public static String getType(Tree t) {
    return pascal3gParser.tokenNames[t.getType()];
  }

  /** helper object */
  public static final CommonTreeAdaptor adaptor = new CommonTreeAdaptor() {
    public Object create(Token payload) {
      return new CommonTree(payload);
    }
  };

} // end class

