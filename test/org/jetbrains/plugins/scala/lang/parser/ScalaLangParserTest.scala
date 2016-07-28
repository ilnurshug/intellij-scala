package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.PsiBuilderFactory
import com.intellij.psi.impl.source.DummyHolderFactory
import com.intellij.psi.impl.source.tree.{FileElement, TreeElement}
import com.intellij.psi.{PsiElement, PsiFileFactory}
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

  def parseProgram(s: String) : PsiElement = {
    val fileFactory = PsiFileFactory.getInstance(fixture.getProject)
    val context = parseText("")
    val holder: FileElement = DummyHolderFactory.createHolder(context.getManager, context).getTreeElement
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(
      PsiBuilderFactory.getInstance.createBuilder(context.getProject, holder, new ScalaLexer,
        ScalaFileType.SCALA_LANGUAGE, s)
    )

    val parser : ANTLRScalaLangParserAdaptor = new ANTLRScalaLangParserAdaptor(new ScalaLangParser(null))
    val node = parser.parse(ScalaElementTypes.BLOCK_EXPR, builder)

    val converter = new ASTTreeToDot()
    println(converter.convert(node))

    holder.rawAddChildren(node.asInstanceOf[TreeElement])

    node.getPsi
  }

  def doTest(s: String): Unit = {
    val elem = parseProgram(s)
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
}
