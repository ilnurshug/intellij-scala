package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.PsiBuilder
import org.antlr.v4.runtime.tree.TerminalNode
import org.jetbrains.plugins.scala.lang.ScalaLangBaseVisitor
import org.jetbrains.plugins.scala.lang.ScalaLangParser._

import org.jetbrains.plugins.scala.lang.parser.visitors._

import scala.collection.mutable

/**
  * Created by ilnur on 06.08.16.
  */
class ScalaLangVisitorImpl(builder: PsiBuilder) extends ScalaLangBaseVisitor {

  var args: mutable.Stack[Object] = new mutable.Stack[Object]  // stack for arguments, might be helpful for some parsing methods

  override def visitProgram(ctx: ProgramContext): Unit = ProgramVisitor.visit(builder, ctx, args)

  override def visitLiteral(ctx: LiteralContext): Unit = LiteralVisitor.visit(builder, ctx, args)

  override def visitQualId(ctx: QualIdContext): Unit = QualIdVisitor.visit(builder, ctx, args)

  override def visitIds(ctx: IdsContext): Unit = IdsVisitor.visit(builder, ctx, args)

  override def visitPath(ctx: PathContext): Unit = PathVisitor.visit(builder, ctx, args)

  override def visitStableId(ctx: StableIdContext): Unit = StableIdVisitor.visit(builder, ctx, args)

  override def visitClassQualifier(ctx: ClassQualifierContext): Unit = ClassQualifierVisitor.visit(builder, ctx, args)

  override def visitType(ctx: TypeContext): Unit = TypeVisitor.visit(builder, ctx, args)

  override def visitFunctionArgTypes(ctx: FunctionArgTypesContext): Unit = FunctionArgTypesVisitor.visit(builder, ctx, args)

  override def visitExistentialClause(ctx: ExistentialClauseContext): Unit = ExistentialClauseVisitor.visit(builder, ctx, args)

  override def visitExistentialDcl(ctx: ExistentialDclContext): Unit = ExistentialDclVisitor.visit(builder, ctx, args)

  override def visitInfixType(ctx: InfixTypeContext): Unit = InfixTypeVisitor.visit(builder, ctx, args)

  override def visitCompoundType(ctx: CompoundTypeContext): Unit = CompoundTypeVisitor.visit(builder, ctx, args)

  override def visitAnnotType(ctx: AnnotTypeContext): Unit = AnnotTypeVisitor.visit(builder, ctx, args)

  override def visitSimpleType(ctx: SimpleTypeContext): Unit = SimpleTypeVisitor.visit(builder, ctx, args)

  override def visitTypeArgs(ctx: TypeArgsContext): Unit = TypeArgsVisitor.visit(builder, ctx, args)

  override def visitTypes(ctx: TypesContext): Unit = TypesVisitor.visit(builder, ctx, args)

  override def visitRefinement(ctx: RefinementContext): Unit = RefinementVisitor.visit(builder, ctx, args)

  override def visitRefineStat(ctx: RefineStatContext): Unit = RefineStatVisitor.visit(builder, ctx, args)

  override def visitTypePat(ctx: TypePatContext): Unit = TypePatVisitor.visit(builder, ctx, args)

  override def visitAscription(ctx: AscriptionContext): Unit = AscriptionVisitor.visit(builder, ctx, args)

  override def visitExpr(ctx: ExprContext): Unit = ExprVisitor.visit(builder, ctx, args)

  override def visitExpr1(ctx: Expr1Context): Unit = Expr1Visitor.visit(builder, ctx, args)

  override def visitPostfixExpr(ctx: PostfixExprContext): Unit = PostfixExprVisitor.visit(builder, ctx, args)

  override def visitInfixExpr(ctx: InfixExprContext): Unit = InfixExprVisitor.visit(builder, ctx, args)

  override def visitPrefixExpr(ctx: PrefixExprContext): Unit = PrefixExprVisitor.visit(builder, ctx, args)

  override def visitSimpleExpr(ctx: SimpleExprContext): Unit = SimpleExprVisitor.visit(builder, ctx, args)

  override def visitSimpleExpr1(ctx: SimpleExpr1Context): Unit = SimpleExpr1Visitor.visit(builder, ctx, args)

  override def visitExprs(ctx: ExprsContext): Unit = ExprsVisitor.visit(builder, ctx, args)

  override def visitArgumentExprs(ctx: ArgumentExprsContext): Unit = ArgumentExprsVisitor.visit(builder, ctx, args)

  override def visitBlockExpr(ctx: BlockExprContext): Unit = BlockExprVisitor.visit(builder, ctx, args)

  override def visitBlock(ctx: BlockContext): Unit = BlockVisitor.visit(builder, ctx, args)

  override def visitBlockStat(ctx: BlockStatContext): Unit = BlockStatVisitor.visit(builder, ctx, args)

  override def visitResultExpr(ctx: ResultExprContext): Unit = ResultExprVisitor.visit(builder, ctx, args)

  override def visitEnumerators(ctx: EnumeratorsContext): Unit = EnumeratorsVisitor.visit(builder, ctx, args)

  override def visitGenerator(ctx: GeneratorContext): Unit = GeneratorVisitor.visit(builder, ctx, args)

  override def visitCaseClauses(ctx: CaseClausesContext): Unit = CaseClausesVisitor.visit(builder, ctx, args)

  override def visitCaseClause(ctx: CaseClauseContext): Unit = CaseClauseVisitor.visit(builder, ctx, args)

  override def visitGuard(ctx: GuardContext): Unit = GuardVisitor.visit(builder, ctx, args)

  override def visitPattern(ctx: PatternContext): Unit = PatternVisitor.visit(builder, ctx, args)

  override def visitPattern1(ctx: Pattern1Context): Unit = Pattern1Visitor.visit(builder, ctx, args)

  override def visitPattern2(ctx: Pattern2Context): Unit = Pattern2Visitor.visit(builder, ctx, args)

  override def visitPattern3(ctx: Pattern3Context): Unit = Pattern3Visitor.visit(builder, ctx, args)

  override def visitSimplePattern(ctx: SimplePatternContext): Unit = SimplePatternVisitor.visit(builder, ctx, args)

  override def visitPatterns(ctx: PatternsContext): Unit = PatternsVisitor.visit(builder, ctx, args)

  override def visitTypeParamClause(ctx: TypeParamClauseContext): Unit = TypeParamClauseVisitor.visit(builder, ctx, args)

  override def visitVariantTypeParam(ctx: VariantTypeParamContext): Unit = VariantTypeParamVisitor.visit(builder, ctx, args)

  override def visitFunTypeParamClause(ctx: FunTypeParamClauseContext): Unit = FunTypeParamClauseVisitor.visit(builder, ctx, args)

  override def visitTypeParam(ctx: TypeParamContext): Unit = TypeParamVisitor.visit(builder, ctx, args)

  override def visitParamClauses(ctx: ParamClausesContext): Unit = ParamClausesVisitor.visit(builder, ctx, args)

  override def visitParamClause(ctx: ParamClauseContext): Unit = ParamClauseVisitor.visit(builder, ctx, args)

  override def visitParams(ctx: ParamsContext): Unit = ParamsVisitor.visit(builder, ctx, args)

  override def visitParam(ctx: ParamContext): Unit = ParamVisitor.visit(builder, ctx, args)

  override def visitParamType(ctx: ParamTypeContext): Unit = ParamTypeVisitor.visit(builder, ctx, args)

  override def visitClassParamClauses(ctx: ClassParamClausesContext): Unit = ClassParamClausesVisitor.visit(builder, ctx, args)

  override def visitClassParamClause(ctx: ClassParamClauseContext): Unit = ClassParamClauseVisitor.visit(builder, ctx, args)

  override def visitClassParams(ctx: ClassParamsContext): Unit = ClassParamsVisitor.visit(builder, ctx, args)

  override def visitClassParam(ctx: ClassParamContext): Unit = ClassParamVisitor.visit(builder, ctx, args)

  override def visitBindings(ctx: BindingsContext): Unit = BindingsVisitor.visit(builder, ctx, args)

  override def visitBinding(ctx: BindingContext): Unit = BindingVisitor.visit(builder, ctx, args)

  override def visitModifier(ctx: ModifierContext): Unit = ModifierVisitor.visit(builder, ctx, args)

  override def visitModifiersOrEmpty(ctx: ModifiersOrEmptyContext): Unit = ModifiersOrEmptyVisitor.visit(builder, ctx, args)

  override def visitLocalModifier(ctx: LocalModifierContext): Unit = LocalModifierVisitor.visit(builder, ctx, args)

  override def visitAccessModifier(ctx: AccessModifierContext): Unit = AccessModifierVisitor.visit(builder, ctx, args)

  override def visitAccessQualifier(ctx: AccessQualifierContext): Unit = AccessQualifierVisitor.visit(builder, ctx, args)

  override def visitAnnotation(ctx: AnnotationContext): Unit = AnnotationVisitor.visit(builder, ctx, args)

  override def visitAnnotationExpr(ctx: AnnotationExprContext): Unit = AnnotationExprVisitor.visit(builder, ctx, args)

  override def visitAnnotations(ctx: AnnotationsContext): Unit = AnnotationsVisitor.visit(builder, ctx, args)

  override def visitAnnotationsNonEmpty(ctx: AnnotationsNonEmptyContext): Unit = AnnotationsNonEmptyVisitor.visit(builder, ctx, args)

  override def visitTemplateBody(ctx: TemplateBodyContext): Unit = TemplateBodyVisitor.visit(builder, ctx, args)

  override def visitTemplateStat(ctx: TemplateStatContext): Unit = TemplateStatVisitor.visit(builder, ctx, args)

  override def visitSelfType(ctx: SelfTypeContext): Unit = SelfTypeVisitor.visit(builder, ctx, args)

  override def visitImport_(ctx: Import_Context): Unit = Import_Visitor.visit(builder, ctx, args)

  override def visitImportExpr(ctx: ImportExprContext): Unit = ImportExprVisitor.visit(builder, ctx, args)

  override def visitImportSelectors(ctx: ImportSelectorsContext): Unit = ImportSelectorsVisitor.visit(builder, ctx, args)

  override def visitImportSelector(ctx: ImportSelectorContext): Unit = ImportSelectorVisitor.visit(builder, ctx, args)

  override def visitDcl(ctx: DclContext): Unit = DclVisitor.visit(builder, ctx, args)

  override def visitValDcl(ctx: ValDclContext): Unit = ValDclVisitor.visit(builder, ctx, args)

  override def visitVarDcl(ctx: VarDclContext): Unit = VarDclVisitor.visit(builder, ctx, args)

  override def visitFunDcl(ctx: FunDclContext): Unit = FunDclVisitor.visit(builder, ctx, args)

  override def visitFunSig(ctx: FunSigContext): Unit = FunSigVisitor.visit(builder, ctx, args)

  override def visitTypeDcl(ctx: TypeDclContext): Unit = TypeDclVisitor.visit(builder, ctx, args)

  override def visitPatVarDef(ctx: PatVarDefContext): Unit = PatVarDefVisitor.visit(builder, ctx, args)

  override def visitDef(ctx: DefContext): Unit = DefVisitor.visit(builder, ctx, args)

  override def visitPatDef(ctx: PatDefContext): Unit = PatDefVisitor.visit(builder, ctx, args)

  override def visitPatternList(ctx: PatternListContext): Unit = PatternListVisitor.visit(builder, ctx, args)

  override def visitVarDef(ctx: VarDefContext): Unit = VarDefVisitor.visit(builder, ctx, args)

  override def visitFunDef(ctx: FunDefContext): Unit = FunDefVisitor.visit(builder, ctx, args)

  override def visitTypeDef(ctx: TypeDefContext): Unit = TypeDefVisitor.visit(builder, ctx, args)

  override def visitTmplDef(ctx: TmplDefContext): Unit = TmplDefVisitor.visit(builder, ctx, args)

  override def visitModifiersOrCase(ctx: ModifiersOrCaseContext): Unit = ModifiersOrCaseVisitor.visit(builder, ctx, args)

  override def visitClassDef(ctx: ClassDefContext): Unit = ClassDefVisitor.visit(builder, ctx, args)

  override def visitPrimaryConstructor(ctx: PrimaryConstructorContext): Unit = PrimaryConstructorVisitor.visit(builder, ctx, args)

  override def visitAccessModifierOrEmpty(ctx: AccessModifierOrEmptyContext): Unit = AccessModifierOrEmptyVisitor.visit(builder, ctx, args)

  override def visitTraitDef(ctx: TraitDefContext): Unit = TraitDefVisitor.visit(builder, ctx, args)

  override def visitObjectDef(ctx: ObjectDefContext): Unit = ObjectDefVisitor.visit(builder, ctx, args)

  override def visitClassTemplateOpt(ctx: ClassTemplateOptContext): Unit = ClassTemplateOptVisitor.visit(builder, ctx, args)

  override def visitTraitTemplateOpt(ctx: TraitTemplateOptContext): Unit = TraitTemplateOptVisitor.visit(builder, ctx, args)

  override def visitClassTemplate(ctx: ClassTemplateContext): Unit = ClassTemplateVisitor.visit(builder, ctx, args)

  override def visitTraitTemplate(ctx: TraitTemplateContext): Unit = TraitTemplateVisitor.visit(builder, ctx, args)

  override def visitClassParents(ctx: ClassParentsContext): Unit = ClassParentsVisitor.visit(builder, ctx, args)

  override def visitTraitParents(ctx: TraitParentsContext): Unit = TraitParentsVisitor.visit(builder, ctx, args)

  override def visitConstr(ctx: ConstrContext): Unit = ConstrVisitor.visit(builder, ctx, args)

  override def visitEarlyDefs(ctx: EarlyDefsContext): Unit = EarlyDefsVisitor.visit(builder, ctx, args)

  override def visitEarlyDef(ctx: EarlyDefContext): Unit = EarlyDefVisitor.visit(builder, ctx, args)

  override def visitConstrExpr(ctx: ConstrExprContext): Unit = ConstrExprVisitor.visit(builder, ctx, args)

  override def visitConstrBlock(ctx: ConstrBlockContext): Unit = ConstrBlockVisitor.visit(builder, ctx, args)

  override def visitSelfInvocation(ctx: SelfInvocationContext): Unit = SelfInvocationVisitor.visit(builder, ctx, args)

  override def visitTopStatSeq(ctx: TopStatSeqContext): Unit = TopStatSeqVisitor.visit(builder, ctx, args)

  override def visitTopStat(ctx: TopStatContext): Unit = TopStatVisitor.visit(builder, ctx, args)

  override def visitPackaging(ctx: PackagingContext): Unit = PackagingVisitor.visit(builder, ctx, args)

  override def visitPackageObject(ctx: PackageObjectContext): Unit = PackageObjectVisitor.visit(builder, ctx, args)

  override def visitCompilationUnit(ctx: CompilationUnitContext): Unit = CompilationUnitVisitor.visit(builder, ctx, args)

  override def visitPackageDcl(ctx: PackageDclContext): Unit = PackageDclVisitor.visit(builder, ctx, args)

  override def visitId(ctx: IdContext): Unit = IdVisitor.visit(builder, ctx, args)

  override def visitSemi(ctx: SemiContext): Unit = SemiVisitor.visit(builder, ctx, args)

  /*
    не содержит вызова метода builder.advanceLexer() для того чтобы
    в visitor'ах можно было писать builder.advanceLexer()
    в данном случае придется вносить в код меньше изменений
   */
  override def visitTerminal(node: TerminalNode): Unit = ()//builder.advanceLexer()

}
