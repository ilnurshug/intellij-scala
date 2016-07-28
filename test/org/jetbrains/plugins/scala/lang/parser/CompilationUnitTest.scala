package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.PsiBuilderFactory
import com.intellij.psi.impl.source.DummyHolderFactory
import com.intellij.psi.impl.source.tree.{FileElement, TreeElement}
import com.intellij.psi.{PsiElement, PsiFileFactory}
import org.jetbrains.plugins.scala.ScalaFileType
import org.jetbrains.plugins.scala.base.SimpleTestCase
import org.jetbrains.plugins.scala.lang.lexer.ScalaLexer
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilderImpl
import org.jetbrains.plugins.scala.lang.parser.parsing.expressions.BlockExpr
import org.jetbrains.plugins.scala.lang.parser.ASTTreeToDot
import org.jetbrains.plugins.scala.lang.parser.parsing.Program
import org.junit.Assert

class CompilationUnitTest extends SimpleTestCase {
  def parseCompilationUnit(s: String): PsiElement = {
    ScalaParserDefinition.setUseOldParser(true)

    val fileFactory = PsiFileFactory.getInstance(fixture.getProject)
    val context = parseText("")
    val holder: FileElement = DummyHolderFactory.createHolder(context.getManager, context).getTreeElement
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(
      PsiBuilderFactory.getInstance.createBuilder(context.getProject, holder, new ScalaLexer,
        ScalaFileType.SCALA_LANGUAGE, s)
    )

    Program.parse(builder)
    val node = builder.getTreeBuilt

    val converter = new ASTTreeToDot()
    println(converter.convert(node))

    holder.rawAddChildren(node.asInstanceOf[TreeElement])
    node.getPsi
  }

  def doTest(s: String) {
    val elem = parseCompilationUnit(s)
    Assert.assertEquals(s, elem.getText)
  }

  def testCompilationUnit() {
    doTest(
      """package a
         package b
         package c"""
    )
  }

  def testCompilationUnit2(): Unit = {
    doTest("""class C{}""")
  }

  private val Header = "class Seq[+A]\nobject Seq { def apply[A](a: A) = new Seq[A] }\ntrait L;\ntrait A; trait B; trait C;\nobject A extends L with A"
  // why scala's parser crashes??
  def testCompilationUnit3(): Unit = {
    doTest(Header)
  }

  def testCompilationUnit4(): Unit = {
    doTest("""class a {
               a.b.c
             }""")
  }
}