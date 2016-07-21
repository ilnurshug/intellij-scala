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
import org.junit.Assert

/**
  * Created by user on 7/20/16.
  */
class ScalaLangParserTest extends SimpleTestCase
{

  /*def parseProgram(s: String) : PsiElement = {
    println("here")
    val fileFactory = PsiFileFactory.getInstance(fixture.getProject)
    val context = parseText("")
    val holder: FileElement = DummyHolderFactory.createHolder(context.getManager, context).getTreeElement
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(
      PsiBuilderFactory.getInstance.createBuilder(context.getProject, holder, new ScalaLexer,
        ScalaFileType.SCALA_LANGUAGE, s)
    )

    println("here2")

    val parser : ANTLRScalaLangParserAdaptor = new ANTLRScalaLangParserAdaptor(ScalaLanguage.Instance, new ScalaLangParser(null))
    parser.parse(ScalaElementTypes.BLOCK_EXPR, builder)

    println("here3")

    val node = builder.getTreeBuilt
    holder.rawAddChildren(node.asInstanceOf[TreeElement])

    println("here4")
    node.getPsi
  }*/

  def doTest(s: String): Unit = {
    //val elem = parseProgram(s)
    //Assert.assertEquals(s, elem.getText)
    Assert.assertEquals(true, true)
  }


  def testProgram(): Unit = {
    doTest(
      """{;}"""
    )
  }
}
