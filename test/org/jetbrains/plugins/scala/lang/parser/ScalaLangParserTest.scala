package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.PsiBuilderFactory
import com.intellij.psi.PsiElement
import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.jetbrains.plugins.scala.lang.lexer.ScalaLexer
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilderImpl
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
    doTest("object A {\n  def boo() {<caret>}\n}")
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
               a.b.`def`
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
    doTest("val x: Int forSome {} = null\n/*start*/x/*end*/\n//Int")
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
    doTest("""
             |<aa></aa> match {
             |          case <aaa><bbb>{1 + 2 + i}</ccc></aaa> =>
             |          case _ =>
             |        }
           """.stripMargin)
  }

  def testProgam18(): Unit = {
    doTest("""
             |package scala.io
             |
             |class BufferedSource {
             |
             |  override lazy val iter = (
             |    Iterator
             |    continually (codec wrap charReader.read())
             |    takeWhile (_ != -1)
             |    map (_.toChar)
             |  )
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
    val s = "new ASDGASDF()\n(x, timestamp)"
    doTest(s)
  }
}
