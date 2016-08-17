package org.jetbrains.plugins.scala.lang.parser

import com.intellij.lang.{PsiBuilder, WhitespacesAndCommentsBinder}
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

  //var typeArgs: mutable.Stack[IElementType] = new mutable.Stack[IElementType]

  var isPattern: mutable.Stack[Boolean] = new mutable.Stack[Boolean]


  override def visitTestRule(ctx: TestRuleContext): Unit = visit(ctx, ScalaElementTypes.BLOCK_EXPR)


  override def visitEnumerator(ctx: EnumeratorContext): Unit = {
    val marker = builder.mark()

    visitChildren(ctx)

    if (ctx.pattern1() != null) marker.done(ScalaElementTypes.ENUMERATOR)
    else marker.drop()
  }

  override def visitConstrAnnotation(ctx: ConstrAnnotationContext): Unit = visit(ctx, ScalaElementTypes.CONSTRUCTOR)

  override def visitBlockNode(ctx: BlockNodeContext): Unit = visit(ctx, ScalaElementTypes.BLOCK)

  override def visitImplicitParamClause(ctx: ImplicitParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitImplicitClassParamClause(ctx: ImplicitClassParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitReferenceExp(ctx: ReferenceExpContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE_EXPRESSION)

  override def visitLiteral(ctx: LiteralContext): Unit = visit(ctx, ScalaElementTypes.LITERAL)

  override def visitInterpolatedStringPartReference(ctx: InterpolatedStringPartReferenceContext): Unit = visit(ctx, ScalaElementTypes.INTERPOLATED_PREFIX_LITERAL_REFERENCE)

  override def visitQualId(ctx: QualIdContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE)

  override def visitIds(ctx: IdsContext): Unit = visit(ctx, ScalaElementTypes.IDENTIFIER_LIST)

  override def visitFieldId(ctx: FieldIdContext): Unit = visit(ctx, ScalaElementTypes.FIELD_ID)

  override def visitReference(ctx: ReferenceContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE)

  override def visitThisReference(ctx: ThisReferenceContext): Unit = visit(ctx, ScalaElementTypes.THIS_REFERENCE)

  override def visitSuperReference(ctx: SuperReferenceContext): Unit = visit(ctx, ScalaElementTypes.SUPER_REFERENCE)

  override def visitPathRef(ctx: PathRefContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE)

  override def visitPathRefExpr(ctx: PathRefExprContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE_EXPRESSION)

  override def visitStableIdRef(ctx: StableIdRefContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE)

  override def visitStableIdRefExpr(ctx: StableIdRefExprContext): Unit = visit(ctx, ScalaElementTypes.REFERENCE_EXPRESSION)

  override def visitTypeType(ctx: TypeTypeContext): Unit = visit(ctx, ScalaElementTypes.TYPE)

  override def visitWildcardType(ctx: WildcardTypeContext): Unit = visit(ctx, ScalaElementTypes.WILDCARD_TYPE)

  override def visitExistentialType(ctx: ExistentialTypeContext): Unit = visit(ctx, ScalaElementTypes.EXISTENTIAL_TYPE)

  override def visitExistentialClause(ctx: ExistentialClauseContext): Unit = visit(ctx, ScalaElementTypes.EXISTENTIAL_CLAUSE)

  override def visitInfixType(ctx: InfixTypeContext): Unit = InfixTypeVisitor.visit(this, ctx)

  override def visitCompoundType(ctx: CompoundTypeContext): Unit = CompoundTypeVisitor.visit(this, ctx)

  override def visitAnnotType(ctx: AnnotTypeContext): Unit = AnnotTypeVisitor.visit(this, ctx)

  override def visitAnnotTypeNoMultipleSQBrackets(ctx: AnnotTypeNoMultipleSQBracketsContext): Unit = AnnotTypeNoMultipleSQBracketsVisitor.visit(this, ctx)

  override def visitSimpleTypeNoMultipleSQBrackets(ctx: SimpleTypeNoMultipleSQBracketsContext): Unit = SimpleTypeNoMultipleSQBracketsVisitor.visit(this, ctx)

  override def visitSimpleTypeSub(ctx: SimpleTypeSubContext): Unit = SimpleTypeSubVisitor.visit(this, ctx)

  override def visitSimpleType(ctx: SimpleTypeContext): Unit = SimpleTypeVisitor.visit(this, ctx)


  override def visitTypeArgs(ctx: TypeArgsContext): Unit = TypeArgsVisitor.visit(this, ctx)


  override def visitTypes(ctx: TypesContext): Unit = TypesVisitor.visit(this, ctx)

  override def visitRefinement(ctx: RefinementContext): Unit = visit(ctx, ScalaElementTypes.REFINEMENT)

  override def visitTypePat(ctx: TypePatContext): Unit = {
    isPattern.push(true)
    visit(ctx, ScalaElementTypes.TYPE_PATTERN)
    isPattern.pop()
  }

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

  override def visitImplicitClosure(ctx: ImplicitClosureContext): Unit = {
    val exprMarker = builder.mark()

    val ipmarker = builder.mark
    builder.advanceLexer() //Ate implicit

    val pmarker = builder.mark
    builder.advanceLexer() //Ate id

    pmarker.done(ScalaElementTypes.PARAM)
    ipmarker.done(ScalaElementTypes.PARAM_CLAUSE)
    ipmarker.precede.done(ScalaElementTypes.PARAM_CLAUSES)

    builder.advanceLexer() //Ate =>

    visitExpr(ctx.expr())

    exprMarker.done(ScalaElementTypes.FUNCTION_EXPR)
  }

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

  override def visitArgumentExprsParen(ctx: ArgumentExprsParenContext): Unit = visit(ctx, ScalaElementTypes.ARG_EXPRS)

  override def visitArgumentExprs(ctx: ArgumentExprsContext): Unit = visit(ctx, ScalaElementTypes.ARG_EXPRS)

  override def visitBlockExpr(ctx: BlockExprContext): Unit = visit(ctx, ScalaElementTypes.BLOCK_EXPR)

  override def visitResultExpr(ctx: ResultExprContext): Unit = ResultExprVisitor.visit(this, ctx)

  override def visitParam(ctx: ParamContext): Unit = visit(ctx, ScalaElementTypes.PARAM)

  override def visitFunTypeParamClause(ctx: FunTypeParamClauseContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PARAM_CLAUSE)

  override def visitTypeParam(ctx: TypeParamContext): Unit = visit(ctx, ScalaElementTypes.TYPE_PARAM)

  override def visitParamClauses(ctx: ParamClausesContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSES)

  override def visitParamClause(ctx: ParamClauseContext): Unit = visit(ctx, ScalaElementTypes.PARAM_CLAUSE)

  override def visitParamType(ctx: ParamTypeContext): Unit = visit(ctx, ScalaElementTypes.PARAM_TYPE)

  override def visitEmptyModifiers(ctx: EmptyModifiersContext): Unit = visit(ctx, ScalaElementTypes.MODIFIERS)

  override def visitWildcardType2(ctx: WildcardType2Context): Unit = visit(ctx, ScalaElementTypes.WILDCARD_TYPE)

  override def visitPatternDefinition(ctx: PatternDefinitionContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_DEFINITION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitVariableDefinition(ctx: VariableDefinitionContext): Unit = visit(ctx, ScalaElementTypes.VARIABLE_DEFINITION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitFunctionDefinition(ctx: FunctionDefinitionContext): Unit = visit(ctx, ScalaElementTypes.FUNCTION_DEFINITION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitMacroDefinition(ctx: MacroDefinitionContext): Unit = visit(ctx, ScalaElementTypes.MACRO_DEFINITION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitTypeDefinition(ctx: TypeDefinitionContext): Unit = visit(ctx, ScalaElementTypes.TYPE_DEFINITION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitBlockWithBraces(ctx: BlockWithBracesContext): Unit = visit(ctx, ScalaElementTypes.BLOCK_EXPR)

  override def visitAnnotations(ctx: AnnotationsContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATIONS, ScalaTokenBinders.DEFAULT_LEFT_EDGE_BINDER, afterDone = true)

  override def visitAnnotationsNoNl(ctx: AnnotationsNoNlContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATIONS)

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

  override def visitExtendsBlock(ctx: ExtendsBlockContext): Unit = visit(ctx, ScalaElementTypes.EXTENDS_BLOCK)

  override def visitSelfType(ctx: SelfTypeContext): Unit = visit(ctx, ScalaElementTypes.SELF_TYPE)

  override def visitWildcardPattern(ctx: WildcardPatternContext): Unit = visit(ctx, ScalaElementTypes.WILDCARD_PATTERN)

  override def visitPatternInParenthesis(ctx: PatternInParenthesisContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_IN_PARENTHESIS)

  override def visitTuplePattern(ctx: TuplePatternContext): Unit = visit(ctx, ScalaElementTypes.TUPLE_PATTERN)

  override def visitStringPatternArgs(ctx: StringPatternArgsContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_ARGS)

  override def visitInterpolatedPrefixPatternReference(ctx: InterpolatedPrefixPatternReferenceContext): Unit = visit(ctx, ScalaElementTypes.INTERPOLATED_PREFIX_PATTERN_REFERENCE)

  override def visitInterpolationPattern(ctx: InterpolationPatternContext): Unit = visit(ctx, ScalaElementTypes.INTERPOLATION_PATTERN)

  override def visitLiteralPattern(ctx: LiteralPatternContext): Unit = visit(ctx, ScalaElementTypes.LITERAL_PATTERN)

  override def visitStableReferencePattern(ctx: StableReferencePatternContext): Unit = visit(ctx, ScalaElementTypes.STABLE_REFERENCE_PATTERN)

  override def visitConstructorPattern(ctx: ConstructorPatternContext): Unit = visit(ctx, ScalaElementTypes.CONSTRUCTOR_PATTERN)

  override def visitPatternArgs(ctx: PatternArgsContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_ARGS)

  override def visitNamingPattern2(ctx: NamingPattern2Context): Unit = visit(ctx, ScalaElementTypes.NAMING_PATTERN)

  override def visitGeneratorNoGuard(ctx: GeneratorNoGuardContext): Unit = visit(ctx, ScalaElementTypes.GENERATOR)

  override def visitEarlyDefs(ctx: EarlyDefsContext): Unit = visit(ctx, ScalaElementTypes.EARLY_DEFINITIONS)

  override def visitPatternList(ctx: PatternListContext): Unit = visit(ctx, ScalaElementTypes.PATTERN_LIST)

  override def visitClassDefinition(ctx: ClassDefinitionContext): Unit = visit(ctx, ScalaElementTypes.CLASS_DEF, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitObjectDefinition(ctx: ObjectDefinitionContext): Unit = visit(ctx, ScalaElementTypes.OBJECT_DEF, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitTraitDefinition(ctx: TraitDefinitionContext): Unit = visit(ctx, ScalaElementTypes.TRAIT_DEF, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitModifiersOrCase(ctx: ModifiersOrCaseContext): Unit = visit(ctx, ScalaElementTypes.MODIFIERS)

  override def visitPrimaryConstructor(ctx: PrimaryConstructorContext): Unit = visit(ctx, ScalaElementTypes.PRIMARY_CONSTRUCTOR)

  override def visitPackageObject(ctx: PackageObjectContext): Unit = visit(ctx, ScalaElementTypes.OBJECT_DEF)

  override def visitValueDeclaration(ctx: ValueDeclarationContext): Unit = visit(ctx, ScalaElementTypes.VALUE_DECLARATION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitVariableDeclaration(ctx: VariableDeclarationContext): Unit = visit(ctx, ScalaElementTypes.VARIABLE_DECLARATION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitFunctionDeclaration(ctx: FunctionDeclarationContext): Unit = visit(ctx, ScalaElementTypes.FUNCTION_DECLARATION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitTypeDeclaration(ctx: TypeDeclarationContext): Unit = visit(ctx, ScalaElementTypes.TYPE_DECLARATION, ScalaTokenBinders.PRECEEDING_COMMENTS_TOKEN)

  override def visitImport_(ctx: Import_Context): Unit = visit(ctx, ScalaElementTypes.IMPORT_STMT)

  override def visitImportExpr(ctx: ImportExprContext): Unit = visit(ctx, ScalaElementTypes.IMPORT_EXPR)

  override def visitImportSelectors(ctx: ImportSelectorsContext): Unit = visit(ctx, ScalaElementTypes.IMPORT_SELECTORS)

  override def visitPlaceholderExpr(ctx: PlaceholderExprContext): Unit = visit(ctx, ScalaElementTypes.PLACEHOLDER_EXPR)

  override def visitEmptyAnnotations(ctx: EmptyAnnotationsContext): Unit = visit(ctx, ScalaElementTypes.ANNOTATIONS)

  override def visitImportSelector(ctx: ImportSelectorContext): Unit = visit(ctx, ScalaElementTypes.IMPORT_SELECTOR)

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
  override def visitPackageDcl(ctx: PackageDclContext): Unit = {
    val marker = builder.mark()

    visitChildren(ctx)

    if (ctx.topStatSeq() != null) marker.drop()
    else marker.done(ScalaElementTypes.PACKAGING)
  }


  override def visitXmlExpr(ctx: XmlExprContext): Unit = visit(ctx, ScalaElementTypes.XML_EXPR)

  override def visitElement(ctx: ElementContext): Unit = {
    val marker = builder.mark()

    visitChildren(ctx)

    if (ctx.emptyElemTag() != null) marker.drop()
    else marker.done(ScalaElementTypes.XML_ELEMENT)
  }

  override def visitEmptyElemTag(ctx: EmptyElemTagContext): Unit = visit(ctx, ScalaElementTypes.XML_EMPTY_TAG)

  override def visitSTag(ctx: STagContext): Unit = visit(ctx, ScalaElementTypes.XML_START_TAG)

  override def visitETag(ctx: ETagContext): Unit = visit(ctx, ScalaElementTypes.XML_END_TAG)

  //override def visitContent(ctx: ContentContext): Unit = super.visitContent(ctx)

  //override def visitContent1(ctx: Content1Context): Unit = super.visitContent1(ctx)

  //override def visitXmlContent(ctx: XmlContentContext): Unit = super.visitXmlContent(ctx)

  override def visitComment(ctx: CommentContext): Unit = visit(ctx, ScalaElementTypes.XML_COMMENT)

  override def visitCDSect(ctx: CDSectContext): Unit = visit(ctx, ScalaElementTypes.XML_CD_SECT)

  override def visitPI(ctx: PIContext): Unit = visit(ctx, ScalaElementTypes.XML_PI)

  override def visitXmlAttribute(ctx: XmlAttributeContext): Unit = visit(ctx, ScalaElementTypes.XML_ATTRIBUTE)

  //override def visitAttValue(ctx: AttValueContext): Unit = super.visitAttValue(ctx)

  //override def visitScalaExpr(ctx: ScalaExprContext): Unit = super.visitScalaExpr(ctx)

  //override def visitCharData(ctx: CharDataContext): Unit = super.visitCharData(ctx)

  override def visitXmlPattern(ctx: XmlPatternContext): Unit = visit(ctx, ScalaElementTypes.XML_PATTERN)

  override def visitEmptyElemTagP(ctx: EmptyElemTagPContext): Unit = visit(ctx, ScalaElementTypes.XML_EMPTY_TAG)

  override def visitSTagP(ctx: STagPContext): Unit = visit(ctx, ScalaElementTypes.XML_START_TAG)

  override def visitETagP(ctx: ETagPContext): Unit = visit(ctx, ScalaElementTypes.XML_END_TAG)

  //override def visitContentP(ctx: ContentPContext): Unit = super.visitContentP(ctx)

  //override def visitContent1P(ctx: Content1PContext): Unit = super.visitContent1P(ctx)

  //override def visitScalaPatterns(ctx: ScalaPatternsContext): Unit = super.visitScalaPatterns(ctx)

  //override def visitXmlPatterns(ctx: XmlPatternsContext): Unit = super.visitXmlPatterns(ctx)



  override def visitTerminal(node: TerminalNode): Unit = builder.advanceLexer()

  //override def visitErrorNode(node: ErrorNode): Unit = super.visitErrorNode(node)

  def visit(ctx: ParserRuleContext, element: IElementType) = {
    val marker = builder.mark()
    visitChildren(ctx)
    marker.done(element)
  }

  def visit(ctx: ParserRuleContext, element: IElementType, left: WhitespacesAndCommentsBinder, afterDone: Boolean = false) = {
    val marker = builder.mark()
    if (!afterDone) marker.setCustomEdgeTokenBinders(left, null)
    visitChildren(ctx)
    marker.done(element)
    if (afterDone) marker.setCustomEdgeTokenBinders(left, null)
  }
}

object ScalaLangVisitorImpl {
  def visitErrorNode(builder: PsiBuilder, node: ErrorNode, msg: String): Unit = {
    val errMarker = builder.mark()
    builder.error(msg)
    errMarker.done(ScalaElementTypes.ERROR_STMT)
  }
}
