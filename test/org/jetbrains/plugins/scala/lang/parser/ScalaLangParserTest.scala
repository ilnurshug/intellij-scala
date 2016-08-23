package org.jetbrains.plugins.scala.lang.parser

import java.util.concurrent.Callable

import com.intellij.lang.PsiBuilderFactory
import com.intellij.psi.impl.source.DummyHolderFactory
import com.intellij.psi.impl.source.tree.{FileElement, TreeElement}
import com.intellij.psi.tree.TokenSet
import com.intellij.psi.{PsiElement, PsiFileFactory}
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTree
import org.jetbrains.plugins.scala.{ScalaFileType, ScalaLanguage}
import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.jetbrains.plugins.scala.lang.ScalaLangParser
import org.jetbrains.plugins.scala.lang.lexer.ScalaLexer
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilderImpl
import org.jetbrains.plugins.scala.lang.parser.ASTTreeToDot
import org.junit.Assert

/**
  * Created by user on 7/20/16.
  */
class ScalaLangParserTest extends SimpleTestCase
{

  def parseProgram(s: String, startRule: String) : PsiElement = {
    val parserDefinition = new ScalaParserDefinition()

    val tmp = PsiBuilderFactory.getInstance.createBuilder(parserDefinition, new ScalaLexer(), s)
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(tmp)

    val parser : ANTLRScalaLangParserAdaptor = ANTLRScalaLangParserAdaptor.INSTANCE
    val marker = builder.mark
    parser.parse(builder, startRule)
    marker.done(ScalaElementTypes.FILE)
    val node = builder.getTreeBuilt

    val converter = new ASTTreeToDot()
    println(converter.convert(node))

    node.getPsi
  }

  def doTest(s: String, startRule: String = "program"): Unit = {
    val elem = parseProgram(s, startRule)
    Assert.assertEquals(s, elem.getText)
    //Assert.assertEquals(true, true)
  }


  def testProgram(): Unit = {
    doTest("class a {\n  def b (a : A) = expr\n}")
  }

  def testProgram0(): Unit = {
    doTest("new X[Int][String][Boolean]")
  }

  def testProgram1(): Unit = {
    doTest(
      """{
         def f[A](n: Int)(body: => A): Option[A] = {
          try
            return Some(body)
          catch {
            case e: Exception if n == 0 => return None
          }
          f[A](n - 1)(body)
        }
        }"""
    )
  }

  def testProgram2(): Unit = {
    doTest("def (){}", "def")
  }

  def testProgram3(): Unit = {
    doTest("""{@a @b lazy def f(n: Int): Int = return f(n + 1)}""")
  }

  def testProgram4(): Unit = {
    doTest(
      """class C{
        val x: _ => String = ???
        }""")
  }

  private val Header = "class Seq[+A];object Seq { def apply[A](a: A) = new Seq[A] };trait L;trait A; trait B; trait C;object A extends L with A"
  private val line = "def f() {}; f(A)"
  def testProgram5(): Unit = {
    //doTest("object Seq { def apply[A](a: A) = new Seq[A] }")
    //doTest("class Seq[+A]\nobject Seq { def apply[A](a: A) = new Seq[A] }")
    doTest(line)
    //doTest(Header + ";" + line)
  }

  def testProgram6() {
    doTest("""class a {
               a.b.c
             }""")
  }

  // IMPORTANT: ANTLR's parser generates ParseTree which differ from expected (antlr5)
  def testProgram7(): Unit = {
    doTest( """{
              |def f(n: Int): Boolean =
              |  n >=0 && n match {
              |    case 1234 => f(n - 1)
              |    case _ => 1234
              |  }
            }""".stripMargin)
  }

  def testProgram8(): Unit = {
    doTest("""object main{
               def whileLoop (cond: => Boolean) (stat: => Unit): Unit
             }""")
  }

  def testProgram9(): Unit = {
    doTest("a +: b +: c +: d", "pattern3")
  }

  def testProgram10(): Unit = {
    doTest("{ \n def a() {} \n def b(){} }"/*.replace('\n',';')*/)
  }

  def testProgram11(): Unit = {
    doTest("val q\"(..$args) = $body\" = tt\nargs match { case r\"${a: Int}\" => 1 }", "block")
  }

  def testProgram12(): Unit = {
    doTest("1 + \n2", "simpleExpr")
  }

  def testProgram13(): Unit = {
    doTest("1 + /*\n*/\n2", "simpleExpr")
  }

  def testProgram14(): Unit = {
    doTest("""case a | b => g""", "caseClause")
  }

  def testProgram15(): Unit = {
    doTest("case Some(a@Some(),_*) => a", "caseClauses")
  }

  def testProgram16(): Unit = {
    doTest("r <<= a b c;d <= e f g\nh = i j k")
  }

  def testProgram17(): Unit = {
    doTest("val a = <?xml version=\"1.0\" ?>", "def")
  }

  def testProgam18(): Unit = {
    doTest("""package scala.collection
             |package mutable
             |
             |import generic._
             |
             |/**
             | * @define Coll `mutable.TreeSet`
             | * @define coll mutable tree set
             | * @factoryInfo
             | *   Companion object of TreeSet providing factory related utilities.
             | *
             | * @author Lucien Pereira
             | *
             | */
             |object TreeSet extends MutableSortedSetFactory[TreeSet] {
             |  /**
             |   *  The empty set of this type
             |   */
             |  def empty[A](implicit ordering: Ordering[A]) = new TreeSet[A]()
             |
             |}
             |
             |/**
             | * A mutable SortedSet using an immutable AVL Tree as underlying data structure.
             | *
             | * @author Lucien Pereira
             | *
             | */
             |class TreeSet[A](implicit val ordering: Ordering[A]) extends SortedSet[A] with SetLike[A, TreeSet[A]]
             |  with SortedSetLike[A, TreeSet[A]] with Set[A] with Serializable {
             |
             |  // Projection constructor
             |  private def this(base: Option[TreeSet[A]], from: Option[A], until: Option[A])(implicit ordering: Ordering[A]) {
             |    this();
             |    this.base = base
             |    this.from = from
             |    this.until = until
             |  }
             |
             |  private var base: Option[TreeSet[A]] = None
             |
             |  private var from: Option[A] = None
             |
             |  private var until: Option[A] = None
             |
             |  private var avl: AVLTree[A] = Leaf
             |
             |  private var cardinality: Int = 0
             |
             |  def resolve: TreeSet[A] = base.getOrElse(this)
             |
             |  private def isLeftAcceptable(from: Option[A], ordering: Ordering[A])(a: A): Boolean =
             |    from.map(x => ordering.gteq(a, x)).getOrElse(true)
             |
             |  private def isRightAcceptable(until: Option[A], ordering: Ordering[A])(a: A): Boolean =
             |    until.map(x => ordering.lt(a, x)).getOrElse(true)
             |
             |  /**
             |   * Cardinality store the set size, unfortunately a
             |   * set view (given by rangeImpl)
             |   * cannot take advantage of this optimisation
             |   *
             |   */
             |  override def size: Int = base.map(_ => super.size).getOrElse(cardinality)
             |
             |  override def stringPrefix = "TreeSet"
             |
             |  override def empty: TreeSet[A] = TreeSet.empty
             |
             |  override def rangeImpl(from: Option[A], until: Option[A]): TreeSet[A] = new TreeSet(Some(this), from, until)
             |
             |  override def -=(elem: A): this.type = {
             |    try {
             |      resolve.avl = resolve.avl.remove(elem, ordering)
             |      resolve.cardinality = resolve.cardinality - 1
             |    } catch {
             |      case e: NoSuchElementException => ()
             |    }
             |    this
             |  }
             |
             |  override def +=(elem: A): this.type = {
             |    try {
             |      resolve.avl = resolve.avl.insert(elem, ordering)
             |      resolve.cardinality = resolve.cardinality + 1
             |    } catch {
             |      case e: IllegalArgumentException => ()
             |    }
             |    this
             |  }
             |
             |  /**
             |   * Thanks to the immutable nature of the
             |   * underlying AVL Tree, we can share it with
             |   * the clone. So clone complexity in time is O(1).
             |   *
             |   */
             |  override def clone(): TreeSet[A] = {
             |    val clone = new TreeSet[A](base, from, until)
             |    clone.avl = resolve.avl
             |    clone.cardinality = resolve.cardinality
             |    clone
             |  }
             |
             |  override def contains(elem: A): Boolean = {
             |    isLeftAcceptable(from, ordering)(elem) &&
             |    isRightAcceptable(until, ordering)(elem) &&
             |    resolve.avl.contains(elem, ordering)
             |  }
             |
             |  override def iterator: Iterator[A] = resolve.avl.iterator
             |    .dropWhile(e => !isLeftAcceptable(from, ordering)(e))
             |      .takeWhile(e => isRightAcceptable(until, ordering)(e))
             |
             |}""".stripMargin)
  }

  def testExperiment(): Unit = {
    val lexer: ScalaLexer  = new ScalaLexer()
    lexer.start("class a \n{\n a.b.c\n}")

    var f = true
    while (f) {
      val t = lexer.getTokenType
      if (t == null) f = false
      else {
        println(t.toString)
        lexer.advance()
      }
    }
    assert(true)
  }

  def testExperiment2(): Unit = {
    //val s = "class A{def a(){}\n def b(){}\n val a = 1\n}"
    //val s = "case a | b => g"
    //val s = "| : _ = => <- <: >: # @ <% \\ + - * ! ~"
    val s = "<!-- abc d=\"a\" -->"

    val parserDefinition = new ScalaParserDefinition()

    val tmp = PsiBuilderFactory.getInstance.createBuilder(parserDefinition, new ScalaLexer(), s)
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(tmp)

    while(!builder.eof()) {
      println(builder.getTokenType.toString + " " + builder.getCurrentOffset + " " + builder.rawTokenIndex + " " + builder.rawTokenTypeStart(1))

      //if (builder.getTokenText.contains("\n")) println(1)
      //else println()

      builder.advanceLexer()
    }
    assert(true)
  }

  def testExperiment3(): Unit = {
    doTest("A \n B \n C", "testRule")
  }

  def testExperiment4(): Unit = {
    doTest("1 +[Int] /*\n\n*/\n2")
  }

  def testExperiment5(): Unit = {
    ScalaParserDefinition.omitWhitespaces = false

    val s = "a \n b \n\n c"
    doTest(s, "testNlRule")

    val parserDefinition = new ScalaParserDefinition()

    val tmp = PsiBuilderFactory.getInstance.createBuilder(parserDefinition, new ScalaLexer(), s)
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(tmp)

    while(!builder.eof()) {
      print(builder.getTokenType.toString)

      if (builder.getTokenText.contains("\n")) println(1)
      else println()

      builder.advanceLexer()
    }
    assert(true)
  }
}
