package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType
import org.antlr.v4.runtime.tree.{ErrorNode, RuleNode, TerminalNode}
import org.jetbrains.plugins.scala.lang.ScalaLangBaseVisitor
import org.jetbrains.plugins.scala.lang.ScalaLangParser._
import org.jetbrains.plugins.scala.lang.parser.visitors._

import scala.collection.mutable

/**
  * Created by ilnur on 06.08.16.
  */
class ScalaLangVisitorImpl(builder: PsiBuilder) extends ScalaLangBaseVisitor[Unit] {

  var args: mutable.Stack[Boolean] = new mutable.Stack[Boolean]  // stack for bool-arguments, might be helpful for some parsing methods

  var typeArgs: mutable.Stack[IElementType] = new mutable.Stack[IElementType]

  override def visitProgram(ctx: ProgramContext): Unit = ProgramVisitor.visit(this, builder, ctx, args)

  override def visitLiteral(ctx: LiteralContext): Unit = LiteralVisitor.visit(this, builder, ctx, args)

  override def visitQualId(ctx: QualIdContext): Unit = QualIdVisitor.visit(this, builder, ctx, args)

  override def visitIds(ctx: IdsContext): Unit = IdsVisitor.visit(this, builder, ctx, args)

  override def visitPath(ctx: PathContext): Unit = PathVisitor.visit(this, builder, ctx, args)

  override def visitStableId(ctx: StableIdContext): Unit = StableIdVisitor.visit(this, builder, ctx, args)

  override def visitClassQualifier(ctx: ClassQualifierContext): Unit = ClassQualifierVisitor.visit(this, builder, ctx, args)

  override def visitType(ctx: TypeContext): Unit = TypeVisitor.visit(this, builder, ctx, args)

  override def visitFunctionArgTypes(ctx: FunctionArgTypesContext): Unit = FunctionArgTypesVisitor.visit(this, builder, ctx, args)

  override def visitExistentialClause(ctx: ExistentialClauseContext): Unit = ExistentialClauseVisitor.visit(this, builder, ctx, args)

  override def visitExistentialDcl(ctx: ExistentialDclContext): Unit = ExistentialDclVisitor.visit(this, builder, ctx, args)

  override def visitInfixType(ctx: InfixTypeContext): Unit = InfixTypeVisitor.visit(this, builder, ctx, args)

  override def visitCompoundType(ctx: CompoundTypeContext): Unit = CompoundTypeVisitor.visit(this, builder, ctx, args)

  override def visitAnnotType(ctx: AnnotTypeContext): Unit = AnnotTypeVisitor.visit(this, builder, ctx, args)

  override def visitSimpleType(ctx: SimpleTypeContext): Unit = SimpleTypeVisitor.visit(this, builder, ctx, args)

  override def visitTypeArgs(ctx: TypeArgsContext): Unit = TypeArgsVisitor.visit(this, builder, ctx, args)

  override def visitTypes(ctx: TypesContext): Unit = TypesVisitor.visit(this, builder, ctx, args)

  override def visitRefinement(ctx: RefinementContext): Unit = RefinementVisitor.visit(this, builder, ctx, args)

  override def visitRefineStat(ctx: RefineStatContext): Unit = RefineStatVisitor.visit(this, builder, ctx, args)

  override def visitTypePat(ctx: TypePatContext): Unit = TypePatVisitor.visit(this, builder, ctx, args)

  override def visitAscription(ctx: AscriptionContext): Unit = AscriptionVisitor.visit(this, builder, ctx, args)

  override def visitExpr(ctx: ExprContext): Unit = ExprVisitor.visit(this, builder, ctx, args)

  override def visitExpr1(ctx: Expr1Context): Unit = Expr1Visitor.visit(this, builder, ctx, args)

  override def visitPostfixExpr(ctx: PostfixExprContext): Unit = PostfixExprVisitor.visit(this, builder, ctx, args)

  override def visitInfixExpr(ctx: InfixExprContext): Unit = InfixExprVisitor.visit(this, builder, ctx, args)

  override def visitPrefixExpr(ctx: PrefixExprContext): Unit = PrefixExprVisitor.visit(this, builder, ctx, args)

  override def visitSimpleExpr(ctx: SimpleExprContext): Unit = SimpleExprVisitor.visit(this, builder, ctx, args)

  override def visitSimpleExpr1(ctx: SimpleExpr1Context): Unit = SimpleExpr1Visitor.visit(this, builder, ctx, args)

  override def visitExprs(ctx: ExprsContext): Unit = ExprsVisitor.visit(this, builder, ctx, args)

  override def visitArgumentExprs(ctx: ArgumentExprsContext): Unit = ArgumentExprsVisitor.visit(this, builder, ctx, args)

  override def visitBlockExpr(ctx: BlockExprContext): Unit = BlockExprVisitor.visit(this, builder, ctx, args)

  override def visitBlock(ctx: BlockContext): Unit = BlockVisitor.visit(this, builder, ctx, args)

  override def visitBlockStat(ctx: BlockStatContext): Unit = BlockStatVisitor.visit(this, builder, ctx, args)

  override def visitResultExpr(ctx: ResultExprContext): Unit = ResultExprVisitor.visit(this, builder, ctx, args)

  override def visitEnumerators(ctx: EnumeratorsContext): Unit = EnumeratorsVisitor.visit(this, builder, ctx, args)

  override def visitGenerator(ctx: GeneratorContext): Unit = GeneratorVisitor.visit(this, builder, ctx, args)

  override def visitCaseClauses(ctx: CaseClausesContext): Unit = CaseClausesVisitor.visit(this, builder, ctx, args)

  override def visitCaseClause(ctx: CaseClauseContext): Unit = CaseClauseVisitor.visit(this, builder, ctx, args)

  override def visitGuard(ctx: GuardContext): Unit = GuardVisitor.visit(this, builder, ctx, args)

  override def visitPattern(ctx: PatternContext): Unit = PatternVisitor.visit(this, builder, ctx, args)

  override def visitPattern1(ctx: Pattern1Context): Unit = Pattern1Visitor.visit(this, builder, ctx, args)

  override def visitPattern2(ctx: Pattern2Context): Unit = Pattern2Visitor.visit(this, builder, ctx, args)

  override def visitPattern3(ctx: Pattern3Context): Unit = Pattern3Visitor.visit(this, builder, ctx, args)

  override def visitSimplePattern(ctx: SimplePatternContext): Unit = SimplePatternVisitor.visit(this, builder, ctx, args)

  override def visitPatterns(ctx: PatternsContext): Unit = PatternsVisitor.visit(this, builder, ctx, args)

  override def visitTypeParamClause(ctx: TypeParamClauseContext): Unit = TypeParamClauseVisitor.visit(this, builder, ctx, args)

  override def visitVariantTypeParam(ctx: VariantTypeParamContext): Unit = VariantTypeParamVisitor.visit(this, builder, ctx, args)

  override def visitFunTypeParamClause(ctx: FunTypeParamClauseContext): Unit = FunTypeParamClauseVisitor.visit(this, builder, ctx, args)

  override def visitTypeParam(ctx: TypeParamContext): Unit = TypeParamVisitor.visit(this, builder, ctx, args)

  override def visitParamClauses(ctx: ParamClausesContext): Unit = ParamClausesVisitor.visit(this, builder, ctx, args)

  override def visitParamClause(ctx: ParamClauseContext): Unit = ParamClauseVisitor.visit(this, builder, ctx, args)

  override def visitParams(ctx: ParamsContext): Unit = ParamsVisitor.visit(this, builder, ctx, args)

  override def visitParam(ctx: ParamContext): Unit = ParamVisitor.visit(this, builder, ctx, args)

  override def visitParamType(ctx: ParamTypeContext): Unit = ParamTypeVisitor.visit(this, builder, ctx, args)

  override def visitClassParamClauses(ctx: ClassParamClausesContext): Unit = ClassParamClausesVisitor.visit(this, builder, ctx, args)

  override def visitClassParamClause(ctx: ClassParamClauseContext): Unit = ClassParamClauseVisitor.visit(this, builder, ctx, args)

  override def visitClassParams(ctx: ClassParamsContext): Unit = ClassParamsVisitor.visit(this, builder, ctx, args)

  override def visitClassParam(ctx: ClassParamContext): Unit = ClassParamVisitor.visit(this, builder, ctx, args)

  override def visitBindings(ctx: BindingsContext): Unit = BindingsVisitor.visit(this, builder, ctx, args)

  override def visitBinding(ctx: BindingContext): Unit = BindingVisitor.visit(this, builder, ctx, args)

  override def visitModifier(ctx: ModifierContext): Unit = ModifierVisitor.visit(this, builder, ctx, args)

  override def visitModifiersOrEmpty(ctx: ModifiersOrEmptyContext): Unit = ModifiersOrEmptyVisitor.visit(this, builder, ctx, args)

  override def visitLocalModifier(ctx: LocalModifierContext): Unit = LocalModifierVisitor.visit(this, builder, ctx, args)

  override def visitAccessModifier(ctx: AccessModifierContext): Unit = AccessModifierVisitor.visit(this, builder, ctx, args)

  override def visitAccessQualifier(ctx: AccessQualifierContext): Unit = AccessQualifierVisitor.visit(this, builder, ctx, args)

  override def visitAnnotation(ctx: AnnotationContext): Unit = AnnotationVisitor.visit(this, builder, ctx, args)

  override def visitAnnotationExpr(ctx: AnnotationExprContext): Unit = AnnotationExprVisitor.visit(this, builder, ctx, args)

  override def visitAnnotations(ctx: AnnotationsContext): Unit = AnnotationsVisitor.visit(this, builder, ctx, args)

  override def visitAnnotationsNonEmpty(ctx: AnnotationsNonEmptyContext): Unit = AnnotationsNonEmptyVisitor.visit(this, builder, ctx, args)

  override def visitTemplateBody(ctx: TemplateBodyContext): Unit = TemplateBodyVisitor.visit(this, builder, ctx, args)

  override def visitTemplateStat(ctx: TemplateStatContext): Unit = TemplateStatVisitor.visit(this, builder, ctx, args)

  override def visitSelfType(ctx: SelfTypeContext): Unit = SelfTypeVisitor.visit(this, builder, ctx, args)

  override def visitImport_(ctx: Import_Context): Unit = Import_Visitor.visit(this, builder, ctx, args)

  override def visitImportExpr(ctx: ImportExprContext): Unit = ImportExprVisitor.visit(this, builder, ctx, args)

  override def visitImportSelectors(ctx: ImportSelectorsContext): Unit = ImportSelectorsVisitor.visit(this, builder, ctx, args)

  override def visitImportSelector(ctx: ImportSelectorContext): Unit = ImportSelectorVisitor.visit(this, builder, ctx, args)

  override def visitDcl(ctx: DclContext): Unit = DclVisitor.visit(this, builder, ctx, args)

  override def visitValDcl(ctx: ValDclContext): Unit = ValDclVisitor.visit(this, builder, ctx, args)

  override def visitVarDcl(ctx: VarDclContext): Unit = VarDclVisitor.visit(this, builder, ctx, args)

  override def visitFunDcl(ctx: FunDclContext): Unit = FunDclVisitor.visit(this, builder, ctx, args)

  override def visitFunSig(ctx: FunSigContext): Unit = FunSigVisitor.visit(this, builder, ctx, args)

  override def visitTypeDcl(ctx: TypeDclContext): Unit = TypeDclVisitor.visit(this, builder, ctx, args)

  override def visitPatVarDef(ctx: PatVarDefContext): Unit = PatVarDefVisitor.visit(this, builder, ctx, args)

  override def visitDef(ctx: DefContext): Unit = DefVisitor.visit(this, builder, ctx, args)

  override def visitPatDef(ctx: PatDefContext): Unit = PatDefVisitor.visit(this, builder, ctx, args)

  override def visitPatternList(ctx: PatternListContext): Unit = PatternListVisitor.visit(this, builder, ctx, args)

  override def visitVarDef(ctx: VarDefContext): Unit = VarDefVisitor.visit(this, builder, ctx, args)

  override def visitFunDef(ctx: FunDefContext): Unit = FunDefVisitor.visit(this, builder, ctx, args)

  override def visitTypeDef(ctx: TypeDefContext): Unit = TypeDefVisitor.visit(this, builder, ctx, args)

  override def visitTmplDef(ctx: TmplDefContext): Unit = TmplDefVisitor.visit(this, builder, ctx, args)

  override def visitModifiersOrCase(ctx: ModifiersOrCaseContext): Unit = ModifiersOrCaseVisitor.visit(this, builder, ctx, args)

  override def visitClassDef(ctx: ClassDefContext): Unit = ClassDefVisitor.visit(this, builder, ctx, args)

  override def visitPrimaryConstructor(ctx: PrimaryConstructorContext): Unit = PrimaryConstructorVisitor.visit(this, builder, ctx, args)

  override def visitAccessModifierOrEmpty(ctx: AccessModifierOrEmptyContext): Unit = AccessModifierOrEmptyVisitor.visit(this, builder, ctx, args)

  override def visitTraitDef(ctx: TraitDefContext): Unit = TraitDefVisitor.visit(this, builder, ctx, args)

  override def visitObjectDef(ctx: ObjectDefContext): Unit = ObjectDefVisitor.visit(this, builder, ctx, args)

  override def visitClassTemplateOpt(ctx: ClassTemplateOptContext): Unit = ClassTemplateOptVisitor.visit(this, builder, ctx, args)

  override def visitTraitTemplateOpt(ctx: TraitTemplateOptContext): Unit = TraitTemplateOptVisitor.visit(this, builder, ctx, args)

  override def visitClassTemplate(ctx: ClassTemplateContext): Unit = ClassTemplateVisitor.visit(this, builder, ctx, args)

  override def visitTraitTemplate(ctx: TraitTemplateContext): Unit = TraitTemplateVisitor.visit(this, builder, ctx, args)

  override def visitClassParents(ctx: ClassParentsContext): Unit = ClassParentsVisitor.visit(this, builder, ctx, args)

  override def visitTraitParents(ctx: TraitParentsContext): Unit = TraitParentsVisitor.visit(this, builder, ctx, args)

  override def visitConstr(ctx: ConstrContext): Unit = ConstrVisitor.visit(this, builder, ctx, args)

  override def visitEarlyDefs(ctx: EarlyDefsContext): Unit = EarlyDefsVisitor.visit(this, builder, ctx, args)

  override def visitEarlyDef(ctx: EarlyDefContext): Unit = EarlyDefVisitor.visit(this, builder, ctx, args)

  override def visitConstrExpr(ctx: ConstrExprContext): Unit = ConstrExprVisitor.visit(this, builder, ctx, args)

  override def visitConstrBlock(ctx: ConstrBlockContext): Unit = ConstrBlockVisitor.visit(this, builder, ctx, args)

  override def visitSelfInvocation(ctx: SelfInvocationContext): Unit = SelfInvocationVisitor.visit(this, builder, ctx, args)

  override def visitTopStatSeq(ctx: TopStatSeqContext): Unit = TopStatSeqVisitor.visit(this, builder, ctx, args)

  override def visitTopStat(ctx: TopStatContext): Unit = TopStatVisitor.visit(this, builder, ctx, args)

  override def visitPackaging(ctx: PackagingContext): Unit = PackagingVisitor.visit(this, builder, ctx, args)

  override def visitPackageObject(ctx: PackageObjectContext): Unit = PackageObjectVisitor.visit(this, builder, ctx, args)

  override def visitCompilationUnit(ctx: CompilationUnitContext): Unit = CompilationUnitVisitor.visit(this, builder, ctx, args)

  override def visitPackageDcl(ctx: PackageDclContext): Unit = PackageDclVisitor.visit(this, builder, ctx, args)

  override def visitId(ctx: IdContext): Unit = IdVisitor.visit(this, builder, ctx, args)

  override def visitSemi(ctx: SemiContext): Unit = SemiVisitor.visit(this, builder, ctx, args)

  override def visitTerminal(node: TerminalNode): Unit = builder.advanceLexer()
  
  //override def visitErrorNode(node: ErrorNode): Unit = super.visitErrorNode(node)
}

object ScalaLangVisitorImpl {
  def visitErrorNode(builder: PsiBuilder, node: ErrorNode, msg: String): Unit = {
    val errMarker = builder.mark()
    builder.error(msg)
    errMarker.done(ScalaElementTypes.ERROR_STMT)
  }
}
