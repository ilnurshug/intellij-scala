package org.jetbrains.plugins.scala.lang.parser

import java.util.concurrent.Callable

import com.intellij.lang.PsiBuilderFactory
import com.intellij.psi.impl.source.DummyHolderFactory
import com.intellij.psi.impl.source.tree.{FileElement, TreeElement}
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
    val fileFactory = PsiFileFactory.getInstance(fixture.getProject)
    val context = parseText("")
    val holder: FileElement = DummyHolderFactory.createHolder(context.getManager, context).getTreeElement
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(
      PsiBuilderFactory.getInstance.createBuilder(context.getProject, holder, new ScalaLexer,
        ScalaFileType.SCALA_LANGUAGE, s)
    )

    val parser : ANTLRScalaLangParserAdaptor = ANTLRScalaLangParserAdaptor.INSTANCE

    val node = parser.parse(builder, startRule)

    val converter = new ASTTreeToDot()
    println(converter.convert(node))

    holder.rawAddChildren(node.asInstanceOf[TreeElement])

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
}
