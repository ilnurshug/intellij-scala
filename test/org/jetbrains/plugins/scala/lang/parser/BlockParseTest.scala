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
import org.jetbrains.plugins.scala.lang.parser.parsing.patterns.Pattern3
import org.junit.Assert

/**
 * @author Alexander Podkhalyuzin
 */

class BlockParseTest extends SimpleTestCase {
  def parseBlock(s: String, p: ScalaPsiBuilderImpl => Boolean = BlockExpr.parse(_) ): PsiElement = {
    ScalaParserDefinition.setUseOldParser(true)

    val fileFactory = PsiFileFactory.getInstance(fixture.getProject)
    val context = parseText("")
    val holder: FileElement = DummyHolderFactory.createHolder(context.getManager, context).getTreeElement
    val builder: ScalaPsiBuilderImpl = new ScalaPsiBuilderImpl(
      PsiBuilderFactory.getInstance.createBuilder(context.getProject, holder, new ScalaLexer,
        ScalaFileType.SCALA_LANGUAGE, s)
    )
    p(builder)
    val node = builder.getTreeBuilt

    val converter = new ASTTreeToDot()
    println(converter.convert(node))

    holder.rawAddChildren(node.asInstanceOf[TreeElement])
    node.getPsi
  }

  def doTest(s: String, p: ScalaPsiBuilderImpl => Boolean = BlockExpr.parse(_)) {
    val elem = parseBlock(s, p)
    Assert.assertEquals(s, elem.getText)
  }

  def testBlock() {
    doTest(
"""{
  val a = new df
  val agaga =  agadg+"/"+aa
  val agd = try {
    ag.agd.adgasdg(asgadg, false, ag)
  } catch {
    d:
  }
  val adg = asdgasdg(adgasdg.asdg(asdg))
  asdg.asdg.adsg(asdgasdg,-1)
  asdg
}"""
    )
  }

  def testBlock2() {
    doTest(
"""{
    PsiDocumentManager.getInstance(project).commitAllDocuments()
    if (!CodeInsightUtilBase.prepareFileForWrite(file)) return
    IdeDocumentHistory.getInstance(project).includeCurrentPlaceAsChangePlace()
    val entityType = ref.getParent match {
      case call: ScMethodCall => call.expectedType.map(_.presentableText)
      case _ => ref.expectedType.map(_.presentableText)
    }
    val parameters:
    inWriteAction {
      val place = if (ref.qualifier.isDefined) anchorForQualified(ref) else anchorForUnqualified(ref)
      for (anchor <- place; holder <- anchor.parent) {
        val placeholder = if (entityType.isDefined) "%s %s%s: Int" else "%s %s%s"
        val text = placeholder.format(keyword, ref.nameId.getText, parameters.mkString)
        val entity = holder.addAfter(parseElement(text, ref.getManager), anchor)
        if (ref.qualifier.isEmpty)
          holder.addBefore(createNewLine(ref.getManager, "\n\n"), entity)
        val builder = new TemplateBuilderImpl(entity)
        for (aType <- entityType;
             typeElement <- entity.children.findByType(classOf[ScSimpleTypeElement])) {
          builder.replaceElement(typeElement, aType)
        }
        entity.depthFirst.filterByType(classOf[ScParameter]).foreach { parameter =>
          val id = parameter.getNameIdentifier
          builder.replaceElement(id, id.getText)
          parameter.paramType.foreach { it =>
            builder.replaceElement(it, it.getText)
          }
        }
        CodeInsightUtilBase.forcePsiPostprocessAndRestoreElement(entity)
        val template = builder.buildTemplate()
        val targetFile = entity.getContainingFile
        val newEditor = positionCursor(project, targetFile, entity.getLastChild)
        val range = entity.getTextRange
        newEditor.getDocument.deleteString(range.getStartOffset, range.getEndOffset)
        TemplateManager.getInstance(project).startTemplate(newEditor, template)
      }
    }
}"""
    )


  }

  def testBlock3() {
      doTest(
"""{
      var asdga = adf
      val adf = """"" + """"
        gads P { fasdf A, B; }
      """"" + """"
      val a = adga(adfad, 'A)
      val b = adga(fadsfa, 'B)

      case class ->(asdfad: Symbol, adfasd: Any)
      implicit def adsfasdf(adfad: ->):
      adsgadsf {
        actor { adsgasdfa(a,b) }
        actor {
          adfada.adsfad { sadsfasdf =>
            sdasfasd ! 'B -> ('label, 42, "foo")
          }
        }
        bdfasdf.asdfas { sasdfasdf =>
          sasdfasdfa.receive('A,'B) {
            case ('A, ('label, i, str)) => asdfasdf = true
            case 'B -> (i:Int) =>
          }
        }
      }

      assert(adsfadsf, "")
    }"""
      )
    }

    def testBlock4() {
      doTest(
"""{
    val asdfadf = fadfad.:
    fadfa {
      dafsdfa {
        case 'take => asdfasd(asdfadf)
      }
    }
  }"""
      )
    }

    def testBlock5(): Unit = {
      doTest("{def f(){};f(A)(B)}")
    }

    def testBlock6():Unit = {
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

    def testBlock7(): Unit = {
      doTest("""{@a @b lazy def f(n: Int): Int = return f(n + 1)}""")
    }

    def testBlock8(): Unit = {
      doTest( """{
                |def f(n: Int): Boolean =
                |  n >=0 && n match {
                |    case 1234 => f(n - 1)
                |    case _ => 1234
                |  }
              }""".stripMargin)
    }

    def testBlock9(): Unit = {
      doTest("{def f(a:Int*){}}")
    }

    def testBlock10(): Unit = {
      doTest("a +: b +: c +: d", Pattern3.parse(_))
    }

    def testBlock11(): Unit = {
      doTest("{def f(){}}")
    }
}