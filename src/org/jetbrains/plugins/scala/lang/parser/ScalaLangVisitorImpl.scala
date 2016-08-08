package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ErrorNode, RuleNode, TerminalNode}
import org.jetbrains.plugins.scala.lang.ScalaLangBaseVisitor
import org.jetbrains.plugins.scala.lang.ScalaLangParser._
import org.jetbrains.plugins.scala.lang.parser.visitors._

import scala.collection.mutable

/**
  * Created by ilnur on 06.08.16.
  */
class ScalaLangVisitorImpl(builder: PsiBuilder) extends ScalaLangBaseVisitor[Unit] {

  def getBuilder = builder

  var args: mutable.Stack[Boolean] = new mutable.Stack[Boolean]  // stack for bool-arguments, might be helpful for some parsing methods

  var typeArgs: mutable.Stack[IElementType] = new mutable.Stack[IElementType]

  override def visitLiteral(ctx: LiteralContext): Unit = visit(ctx, ScalaElementTypes.LITERAL)

  override def visitQualId(ctx: QualIdContext): Unit = QualIdVisitor.visit(this, ctx)

  override def visitIds(ctx: IdsContext): Unit = visit(ctx, ScalaElementTypes.IDENTIFIER_LIST)

  override def visitPath(ctx: PathContext): Unit = PathVisitor.visit(this, ctx)

  override def visitStableId(ctx: StableIdContext): Unit = StableIdVisitor.visit(this, ctx)

  override def visitTypeType(ctx: TypeTypeContext): Unit = visit(ctx, ScalaElementTypes.TYPE)

  override def visitWildcardType(ctx: WildcardTypeContext): Unit = visit(ctx, ScalaElementTypes.WILDCARD_TYPE)

  override def visitExistentialType(ctx: ExistentialTypeContext): Unit = visit(ctx, ScalaElementTypes.EXISTENTIAL_TYPE)

  override def visitExistentialClause(ctx: ExistentialClauseContext): Unit = visit(ctx, ScalaElementTypes.EXISTENTIAL_CLAUSE)

  override def visitInfixType(ctx: InfixTypeContext): Unit = InfixTypeVisitor.visit(this, ctx)

  override def visitCompoundType(ctx: CompoundTypeContext): Unit = CompoundTypeVisitor.visit(this, ctx)

  override def visitAnnotType(ctx: AnnotTypeContext): Unit = AnnotTypeVisitor.visit(this, ctx)

  override def visitSimpleType(ctx: SimpleTypeContext): Unit = SimpleTypeVisitor.visit(this, ctx)

  override def visitTypeArgs(ctx: TypeArgsContext): Unit = TypeArgsVisitor.visit(this, ctx)

  override def visitTypes(ctx: TypesContext): Unit = TypesVisitor.visit(this, ctx)

  override def visitRefinement(ctx: RefinementContext): Unit = visit(ctx, ScalaElementTypes.REFINEMENT)

  override def visitTypePat(ctx: TypePatContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PATTERN)

  override def visitSequenceArg(ctx: SequenceArgContext): Unit = visit(ctx, ScalaElementTypes.SEQUENCE_ARG)

  override def visitExpr(ctx: ExprContext): Unit = ExprVisitor.visit(this, ctx)

  override def visitIfStmt(ctx: IfStmtContext): Unit = visit(ctx, ScalaElementTypes.IF_STMT)

  override def visitWhileStmt(ctx: WhileStmtContext): Unit = visit(ctx, ScalaElementTypes.WHILE_STMT)

  override def visitTryStmt(ctx: TryStmtContext): Unit = visit(ctx, ScalaElementTypes.TRY_STMT)

  override def visitTryBlock(ctx: TryBlockContext): Unit = visit(ctx, ScalaElementTypes.TRY_BLOCK)

  override def visitCatchBlock(ctx: CatchBlockContext): Unit = visit(ctx, ScalaElementTypes.CATCH_BLOCK)

  override def visitFinallyBlock(ctx: FinallyBlockContext): Unit = visit(ctx, ScalaElementTypes.FINALLY_BLOCK)

  override def visitDoStmt(ctx: DoStmtContext): Unit = visit(ctx, ScalaElementTypes.DO_STMT)

  override def visitForStmt(ctx: ForStmtContext): Unit = visit(ctx, ScalaElementTypes.FOR_STMT)

  override def visitThrowStmt(ctx: ThrowStmtContext): Unit = visit(ctx, ScalaElementTypes.THROW_STMT)

  override def visitReturnStmt(ctx: ReturnStmtContext): Unit = visit(ctx, ScalaElementTypes.RETURN_STMT)

  override def visitAssignStmt(ctx: AssignStmtContext): Unit = visit(ctx, ScalaElementTypes.ASSIGN_STMT)

  override def visitTypedExprStmt(ctx: TypedExprStmtContext): Unit = visit(ctx, ScalaElementTypes.TYPED_EXPR_STMT)

  override def visitMatchStmt(ctx: MatchStmtContext): Unit = visit(ctx, ScalaElementTypes.MATCH_STMT)

  override def visitBindings(ctx: BindingsContext): Unit = {
    val marker = builder.mark()
    visit(ctx, ScalaElementTypes.PARAM_CLAUSE)
    marker.done(ScalaElementTypes.PARAM_CLAUSES)
  }

  override def visitBinding(ctx: BindingContext): Unit = BindingVisitor.visit(this, ctx)

  override def visitNewTemplate(ctx: NewTemplateContext): Unit = visit(ctx, ScalaElementTypes.NEW_TEMPLATE)

  override def visitSimpleExpr1(ctx: SimpleExpr1Context): Unit = SimpleExpr1Visitor.visit(this, ctx)

  override def visitPostfixExpr(ctx: PostfixExprContext): Unit = PostfixExprVisitor.visit(this, ctx)

  override def visitInfixExpr(ctx: InfixExprContext): Unit = InfixExprVisitor.visit(this, ctx)

  override def visitPrefixExpr(ctx: PrefixExprContext): Unit = PrefixExprVisitor.visit(this, ctx)

  override def visitArgumentExprs(ctx: ArgumentExprsContext): Unit = visit(ctx, ScalaElementTypes.ARG_EXPRS)

  override def visitBlockExpr(ctx: BlockExprContext): Unit = visit(ctx, ScalaElementTypes.BLOCK_EXPR)

  override def visitBlock(ctx: BlockContext): Unit = BlockVisitor.visit(this, ctx)

  override def visitResultExpr(ctx: ResultExprContext): Unit = ResultExprVisitor.visit(this, ctx)

  override def visitParam(ctx: ParamContext): Unit = visit(ctx, ScalaElementTypes.PARAM)

  override def visitFunTypeParamClause(ctx: FunTypeParamClauseContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PARAM_CLAUSE)

  override def visitTypeParam(ctx: TypeParamContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PARAM)

  override def visitParamClauses(ctx: ParamClausesContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSES)

  override def visitParamClause(ctx: ParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitParamType(ctx: ParamTypeContext): Unit = visit(ctx, ScalaElementTypes.PARAM_TYPE)

  override def visitPatternDefinition(ctx: PatternDefinitionContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_DEFINITION)

  override def visitVariableDefinition(ctx: VariableDefinitionContext): Unit = visit(ctx, ScalaElementTypes.VARIABLE_DEFINITION)

  override def visitFunctionDefinition(ctx: FunctionDefinitionContext): Unit = visit(ctx, ScalaElementTypes.FUNCTION_DEFINITION)

  override def visitTypeDefinition(ctx: TypeDefinitionContext): Unit = visit(ctx, ScalaElementTypes.TYPE_DEFINITION)

  override def visitTemplateDefinition(ctx: TemplateDefinitionContext): Unit = ???

  override def visitBlockWithBraces(ctx: BlockWithBracesContext): Unit = visit(ctx, ScalaElementTypes.BLOCK_EXPR)

  override def visitAnnotations(ctx: AnnotationsContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATIONS)

  override def visitAnnotationsNonEmpty(ctx: AnnotationsNonEmptyContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATIONS)

  override def visitEnumerators(ctx: EnumeratorsContext): Unit = visit(ctx, ScalaElementTypes.ENUMERATORS)

  override def visitCaseClauses(ctx: CaseClausesContext): Unit = visit(ctx, ScalaElementTypes.CASE_CLAUSES)

  override def visitCaseClause(ctx: CaseClauseContext): Unit = visit(ctx, ScalaElementTypes.CASE_CLAUSE)

  override def visitGuard(ctx: GuardContext): Unit = visit(ctx, ScalaElementTypes.GUARD)

  override def visitPattern(ctx: PatternContext): Unit = PatternVisitor.visit(this, ctx)

  override def visitTypedPattern(ctx: TypedPatternContext): Unit = visit(ctx, ScalaElementTypes.TYPED_PATTERN)

  override def visitReferencePattern(ctx: ReferencePatternContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE_PATTERN)

  override def visitNamingPattern(ctx: NamingPatternContext): Unit = visit(ctx, ScalaElementTypes.NAMING_PATTERN)

  override def visitPattern3(ctx: Pattern3Context): Unit = Pattern3Visitor.visit(this, ctx)

  override def visitPatternSeq(ctx: PatternSeqContext): Unit = visit(ctx, ScalaElementTypes.PATTERNS)

  override def visitSeqWildcard(ctx: SeqWildcardContext): Unit = visit(ctx, ScalaElementTypes.SEQ_WILDCARD)

  override def visitTypeParamClause(ctx: TypeParamClauseContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PARAM_CLAUSE)

  override def visitVariantTypeParam(ctx: VariantTypeParamContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PARAM)

  override def visitClassParamClauses(ctx: ClassParamClausesContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSES)

  override def visitClassParamClause(ctx: ClassParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitClassParam(ctx: ClassParamContext): Unit = visit(ctx, ScalaElementTypes.CLASS_PARAM)

  override def visitModifiersOrEmpty(ctx: ModifiersOrEmptyContext): Unit = visit(ctx, ScalaElementTypes.MODIFIERS)

  override def visitAccessModifier(ctx: AccessModifierContext): Unit = visit(ctx, ScalaElementTypes.ACCESS_MODIFIER)

  override def visitAnnotation(ctx: AnnotationContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATION)

  override def visitAccessModifierOrEmpty(ctx: AccessModifierOrEmptyContext): Unit = visit(ctx, ScalaElementTypes.MODIFIERS)

  override def visitAnnotationExpr(ctx: AnnotationExprContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATION_EXPR)

  override def visitTemplateBody(ctx: TemplateBodyContext): Unit = visit(ctx, ScalaElementTypes.TEMPLATE_BODY)

  override def visitSelfType(ctx: SelfTypeContext): Unit = visit(ctx, ScalaElementTypes.SELF_TYPE)

  override def visitWildcardPattern(ctx: WildcardPatternContext): Unit = visit(ctx, ScalaElementTypes.WILDCARD_PATTERN)

  override def visitPatternInParenthesis(ctx: PatternInParenthesisContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_IN_PARENTHESIS)

  override def visitTuplePattern(ctx: TuplePatternContext): Unit = visit(ctx, ScalaElementTypes.TUPLE_PATTERN)

  override def visitLiteralPattern(ctx: LiteralPatternContext): Unit = visit(ctx, ScalaElementTypes.LITERAL_PATTERN)

  override def visitStableReferencePattern(ctx: StableReferencePatternContext): Unit = visit(ctx, ScalaElementTypes.STABLE_REFERENCE_PATTERN)

  override def visitConstructorPattern(ctx: ConstructorPatternContext): Unit = visit(ctx, ScalaElementTypes.CONSTRUCTOR_PATTERN)

  override def visitPatternArgs(ctx: PatternArgsContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_ARGS)

  override def visitNamingPattern2(ctx: NamingPattern2Context): Unit = visit(ctx, ScalaElementTypes.NAMING_PATTERN)

  override def visitGeneratorNoGuard(ctx: GeneratorNoGuardContext): Unit = visit(ctx, ScalaElementTypes.GENERATOR)







  override def visitImport_(ctx: Import_Context): Unit = Import_Visitor.visit(this, ctx)

  override def visitImportExpr(ctx: ImportExprContext): Unit = ImportExprVisitor.visit(this, ctx)

  override def visitImportSelectors(ctx: ImportSelectorsContext): Unit = ImportSelectorsVisitor.visit(this, ctx)

  override def visitImportSelector(ctx: ImportSelectorContext): Unit = ImportSelectorVisitor.visit(this, ctx)

  override def visitDcl(ctx: DclContext): Unit = DclVisitor.visit(this, ctx)

  override def visitValDcl(ctx: ValDclContext): Unit = ValDclVisitor.visit(this, ctx)

  override def visitVarDcl(ctx: VarDclContext): Unit = VarDclVisitor.visit(this, ctx)

  override def visitFunDcl(ctx: FunDclContext): Unit = FunDclVisitor.visit(this, ctx)

  override def visitTypeDcl(ctx: TypeDclContext): Unit = TypeDclVisitor.visit(this, ctx)

  override def visitPatVarDef(ctx: PatVarDefContext): Unit = PatVarDefVisitor.visit(this, ctx)

  override def visitPatDef(ctx: PatDefContext): Unit = PatDefVisitor.visit(this, ctx)

  override def visitPatternList(ctx: PatternListContext): Unit = PatternListVisitor.visit(this, ctx)

  override def visitVarDef(ctx: VarDefContext): Unit = VarDefVisitor.visit(this, ctx)

  override def visitTypeDef(ctx: TypeDefContext): Unit = TypeDefVisitor.visit(this, ctx)

  override def visitTmplDef(ctx: TmplDefContext): Unit = TmplDefVisitor.visit(this, ctx)

  override def visitModifiersOrCase(ctx: ModifiersOrCaseContext): Unit = ModifiersOrCaseVisitor.visit(this, ctx)

  override def visitClassDef(ctx: ClassDefContext): Unit = ClassDefVisitor.visit(this, ctx)

  override def visitPrimaryConstructor(ctx: PrimaryConstructorContext): Unit = PrimaryConstructorVisitor.visit(this, ctx)

  override def visitTraitDef(ctx: TraitDefContext): Unit = TraitDefVisitor.visit(this, ctx)

  override def visitObjectDef(ctx: ObjectDefContext): Unit = ObjectDefVisitor.visit(this, ctx)

  override def visitClassTemplateOpt(ctx: ClassTemplateOptContext): Unit = ClassTemplateOptVisitor.visit(this, ctx)

  override def visitTraitTemplateOpt(ctx: TraitTemplateOptContext): Unit = TraitTemplateOptVisitor.visit(this, ctx)

  override def visitClassTemplate(ctx: ClassTemplateContext): Unit = ClassTemplateVisitor.visit(this, ctx)

  override def visitTraitTemplate(ctx: TraitTemplateContext): Unit = TraitTemplateVisitor.visit(this, ctx)

  override def visitClassParents(ctx: ClassParentsContext): Unit = ClassParentsVisitor.visit(this, ctx)

  override def visitTraitParents(ctx: TraitParentsContext): Unit = TraitParentsVisitor.visit(this, ctx)

  override def visitConstr(ctx: ConstrContext): Unit = ConstrVisitor.visit(this, ctx)

  override def visitEarlyDefs(ctx: EarlyDefsContext): Unit = EarlyDefsVisitor.visit(this, ctx)

  override def visitEarlyDef(ctx: EarlyDefContext): Unit = EarlyDefVisitor.visit(this, ctx)

  override def visitConstrExpr(ctx: ConstrExprContext): Unit = ConstrExprVisitor.visit(this, ctx)

  override def visitConstrBlock(ctx: ConstrBlockContext): Unit = ConstrBlockVisitor.visit(this, ctx)

  override def visitSelfInvocation(ctx: SelfInvocationContext): Unit = SelfInvocationVisitor.visit(this, ctx)

  override def visitTopStatSeq(ctx: TopStatSeqContext): Unit = TopStatSeqVisitor.visit(this, ctx)

  override def visitTopStat(ctx: TopStatContext): Unit = TopStatVisitor.visit(this, ctx)

  override def visitPackaging(ctx: PackagingContext): Unit = PackagingVisitor.visit(this, ctx)

  override def visitPackageObject(ctx: PackageObjectContext): Unit = PackageObjectVisitor.visit(this, ctx)

  override def visitPackageDcl(ctx: PackageDclContext): Unit = PackageDclVisitor.visit(this, ctx)

  override def visitTerminal(node: TerminalNode): Unit = builder.advanceLexer()

  //override def visitErrorNode(node: ErrorNode): Unit = super.visitErrorNode(node)

  def visit(ctx: ParserRuleContext, element: IElementType) = {
    val marker = builder.mark()
    visitChildren(ctx)
    marker.done(element)
  }
}

object ScalaLangVisitorImpl {
  def visitErrorNode(builder: PsiBuilder, node: ErrorNode, msg: String): Unit = {
    val errMarker = builder.mark()
    builder.error(msg)
    errMarker.done(ScalaElementTypes.ERROR_STMT)
  }
}
