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

    parser.parse(builder, startRule)
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
    doTest(
      """{def f(){};f(A)}"""
    )
  }

  def testProgram0(): Unit = {
    doTest(
      """{def f(){}}"""
    )
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
    doTest(
      """package a
         package b
         package c"""
    )
  }

  def testProgram3(): Unit = {
    doTest("""{@a @b lazy def f(n: Int): Int = return f(n + 1)}""")
  }

  def testProgram4(): Unit = {
    doTest("""class C{}""")
  }

  // why parser crashes??
  private val Header = "class Seq[+A];object Seq { def apply[A](a: A) = new Seq[A] };trait L;trait A; trait B; trait C;object A extends L with A"
  private val line = "def f(a: A){}; f(A)"
  def testProgram5(): Unit = {
    //doTest("trait L;\ntrait A; trait B; trait C;\nobject A extends L with A")
    //doTest("object Seq { def apply[A](a: A) = new Seq[A] }")
    //doTest("class Seq[+A]\nobject Seq { def apply[A](a: A) = new Seq[A] }")
    doTest(Header + ";" + line)
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
    doTest("Container {type }", "type")
  }

  def testProgram12(): Unit = {
    doTest("class A{ var a,b,c: Type }")
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
    val s = "class A{def a(){}\n def b(){}\n val a = 1\n}"

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
