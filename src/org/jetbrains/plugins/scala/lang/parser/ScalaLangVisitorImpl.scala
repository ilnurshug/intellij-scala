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


  override def visitBlockNode(ctx: BlockNodeContext): Unit = visit(ctx, ScalaElementTypes.BLOCK)

  override def visitImplicitParamClause(ctx: ImplicitParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitImplicitClassParamClause(ctx: ImplicitClassParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitLiteral(ctx: LiteralContext): Unit = visit(ctx, ScalaElementTypes.LITERAL)

  override def visitQualId(ctx: QualIdContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE)

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

  override def visitEarlyDefs(ctx: EarlyDefsContext): Unit = visit(ctx, ScalaElementTypes.EARLY_DEFINITIONS)

  override def visitPatternList(ctx: PatternListContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_LIST)

  override def visitClassDefinition(ctx: ClassDefinitionContext): Unit = visit(ctx, ScalaElementTypes.CLASS_DEF)

  override def visitObjectDefinition(ctx: ObjectDefinitionContext): Unit = visit(ctx, ScalaElementTypes.OBJECT_DEF)

  override def visitTraitDefinition(ctx: TraitDefinitionContext): Unit = visit(ctx, ScalaElementTypes.TRAIT_DEF)

  override def visitModifiersOrCase(ctx: ModifiersOrCaseContext): Unit = visit(ctx, ScalaElementTypes.MODIFIERS)

  override def visitPrimaryConstructor(ctx: PrimaryConstructorContext): Unit = visit(ctx, ScalaElementTypes.PRIMARY_CONSTRUCTOR)

  override def visitPackageObject(ctx: PackageObjectContext): Unit = visit(ctx, ScalaElementTypes.OBJECT_DEF)

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Unit = visit(ctx, ScalaElementTypes.VALUE_DECLARATION)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Unit = visit(ctx, ScalaElementTypes.VARIABLE_DECLARATION)

  override def visitFunctionDeclaration(ctx: FunctionDeclarationContext): Unit = visit(ctx, ScalaElementTypes.FUNCTION_DECLARATION)

  override def visitTypeDeclaration(ctx: TypeDeclarationContext): Unit = visit(ctx, ScalaElementTypes.TYPE_DECLARATION)

  override def visitImport_(ctx: Import_Context): Unit = visit(ctx, ScalaElementTypes.IMPORT_STMT)

  override def visitImportExpr(ctx: ImportExprContext): Unit = visit(ctx, ScalaElementTypes.IMPORT_EXPR)

  override def visitImportSelectors(ctx: ImportSelectorsContext): Unit = visit(ctx, ScalaElementTypes.IMPORT_SELECTORS)

  override def visitClassTemplateOpt(ctx: ClassTemplateOptContext): Unit = visit(ctx, ScalaElementTypes.EXTENDS_BLOCK)

  override def visitTraitTemplateOpt(ctx: TraitTemplateOptContext): Unit = visit(ctx, ScalaElementTypes.EXTENDS_BLOCK)

  override def visitClassParents(ctx: ClassParentsContext): Unit = visit(ctx, ScalaElementTypes.CLASS_PARENTS)

  override def visitTraitParents(ctx: TraitParentsContext): Unit = visit(ctx, ScalaElementTypes.TRAIT_PARENTS)

  override def visitConstr(ctx: ConstrContext): Unit = visit(ctx, ScalaElementTypes.CONSTRUCTOR)

  override def visitConstrExpr(ctx: ConstrExprContext): Unit = ConstrExprVisitor.visit(this, ctx)

  override def visitConstrBlock(ctx: ConstrBlockContext): Unit = visit(ctx, ScalaElementTypes.CONSTR_BLOCK)

  override def visitSelfInvocation(ctx: SelfInvocationContext): Unit = visit(ctx, ScalaElementTypes.SELF_INVOCATION)

  override def visitPackaging(ctx: PackagingContext): Unit = visit(ctx, ScalaElementTypes.PACKAGING)

  // ???
  override def visitPackageDcl(ctx: PackageDclContext): Unit = visit(ctx, ScalaElementTypes.PACKAGING)



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
