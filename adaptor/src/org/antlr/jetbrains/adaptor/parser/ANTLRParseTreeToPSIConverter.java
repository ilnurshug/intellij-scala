package org.antlr.jetbrains.adaptor.parser;

import com.intellij.lang.Language;
import com.intellij.lang.PsiBuilder;
import com.intellij.openapi.progress.ProgressIndicatorProvider;
import com.intellij.psi.tree.IElementType;
import org.antlr.jetbrains.adaptor.lexer.PSIElementTypeFactory;
import org.antlr.jetbrains.adaptor.lexer.RuleIElementType;
import org.antlr.jetbrains.adaptor.lexer.TokenIElementType;
import org.antlr.v4.runtime.ANTLRErrorListener;
import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.lexer.ScalaElementType;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;

import java.util.*;

/** This is how we build an intellij PSI tree from an ANTLR parse tree.
 *  We let the ANTLR parser build its kind of ParseTree and then
 *  we convert to a PSI tree in one go using a standard ANTLR ParseTreeListener.
 *
 *  The list of SyntaxError objects are pulled from the parser and used
 *  for error message highlighting (error nodes don't have the info).
 */
public class ANTLRParseTreeToPSIConverter implements ParseTreeListener {
	protected final Language language;
	protected final PsiBuilder builder;
	protected List<SyntaxError> syntaxErrors;
	protected final Deque<PsiBuilder.Marker> markers = new ArrayDeque<PsiBuilder.Marker>();

	protected final List<TokenIElementType> tokenElementTypes;
	protected final List<RuleIElementType> ruleElementTypes;

	/** Map an error's start char index (usually start of a token) to the error object. */
	protected Map<Integer, SyntaxError> tokenToErrorMap = new HashMap<Integer, SyntaxError>();

	//private HashMap<Integer, ArrayList<Integer>> tokenPositions = new HashMap<Integer, ArrayList<Integer>>();

	public ANTLRParseTreeToPSIConverter(Language language, Parser parser, PsiBuilder builder) {
		this.language = language;
		this.builder = builder;

		this.tokenElementTypes = PSIElementTypeFactory.getTokenIElementTypes(language);
		this.ruleElementTypes = PSIElementTypeFactory.getRuleIElementTypes(language);

		for (ANTLRErrorListener listener : parser.getErrorListeners()) {
			if (listener instanceof SyntaxErrorListener) {
				syntaxErrors = ((SyntaxErrorListener)listener).getSyntaxErrors();
				for (SyntaxError error : syntaxErrors) {
					// record first error per token
					int StartIndex = error.getOffendingSymbol().getStartIndex();
					if ( !tokenToErrorMap.containsKey(StartIndex) ) {
						tokenToErrorMap.put(StartIndex, error);
					}
				}
			}
		}
	}

	protected final Language getLanguage() {
		return language;
	}

	protected final PsiBuilder getBuilder() {
		return builder;
	}

	protected final Deque<PsiBuilder.Marker> getMarkers() {
		return markers;
	}

	protected final List<TokenIElementType> getTokenElementTypes() {
		return tokenElementTypes;
	}

	protected final List<RuleIElementType> getRuleElementTypes() {
		return ruleElementTypes;
	}

	@Override
	public void visitTerminal(TerminalNode node) {
		//int type = node.getSymbol().getType();
		//int pos = node.getSymbol().getStartIndex();

		//System.out.println(node.getSymbol().getText() + " " + type + " " + pos);

//		if (!tokenPositions.containsKey(type)) tokenPositions.put(type, new ArrayList<Integer>());
//		ArrayList<Integer> arr = tokenPositions.get(type);
//		arr.add(pos);

		builder.advanceLexer();
	}

	/** Summary. For any syntax error thrown by the parser, there will be an
	 *  ErrorNode in the parse tree and this method will process it.
	 *  All errors correspond to actual tokens in the input except for
	 *  missing token errors.
	 *
	 *  There are there are multiple error situations to consider:
	 *
	 *  1. Extraneous token. The parse tree will have an ErrorNode for token.
	 *
	 *  2. Token mismatch. The parse tree will have an ErrorNode for token.
	 *
	 *  3. Missing token. The parse tree will have an ErrorNode but
	 *     it does not correspond to any bit of the input. We underline
	 *     the current token.
	 *
	 *  4. NoViableAlt (input inconsistent with any rule alt).
	 *     The parse tree will have an ErrorNode for token.
	 *
	 *  5. Tokens consumed to resync the parser during recovery.
	 *     The parse tree will have an ErrorNode for each token.
	 *
	 *  This is complicated by errors that occur at EOF but I have
	 *  modified error strategy to add error nodes for EOF if needed.
	 *
	 *  Another complication. During prediction, we might match n
	 *  tokens and then fail on the n+1 token, leading to NoViableAltException.
	 *  But, it's offending token is at n+1 not current token where
	 *  prediction started (which we use to find syntax errors). So,
	 *  SyntaxError objects return start not offending token in this case.
	 */
	public void visitErrorNode(ErrorNode node) {
		ProgressIndicatorProvider.checkCanceled();

		Token badToken = node.getSymbol();
		boolean isConjuredToken = badToken.getTokenIndex()<0;
		int nodeStartIndex = badToken.getStartIndex();
		SyntaxError error = tokenToErrorMap.get(nodeStartIndex);

		if ( error!=null ) {
			PsiBuilder.Marker errorMarker = builder.mark();
			if ( badToken.getStartIndex()>=0 &&
				 badToken.getType()!=Token.EOF &&
				 !isConjuredToken )
			{
				// we advance lexer if error occurred at a real token
				// Missing tokens should highlight the token at the missing position
				// but can't consume a token that does not exist.
				builder.advanceLexer();
			}
			String message = String.format("%s%n", error.getMessage());
			errorMarker.error(message);
		}
		else {
			if ( isConjuredToken ) {
				PsiBuilder.Marker errorMarker = builder.mark();
				errorMarker.error(badToken.getText()); // says "<missing X>" or similar
			}
			else {
				// must be a real token consumed during recovery; just consume w/o highlighting it as an error
				builder.advanceLexer();
			}
		}
	}

	@Override
	public void enterEveryRule(ParserRuleContext ctx) {
		ProgressIndicatorProvider.checkCanceled();
		markers.push(getBuilder().mark());
	}

	@Override
	public void exitEveryRule(ParserRuleContext ctx) {
		ProgressIndicatorProvider.checkCanceled();
		PsiBuilder.Marker marker = markers.pop();
		//marker.done(getRuleElementTypes().get(ctx.getRuleIndex()));
		int i = ctx.getRuleIndex();
		if (canDropMarker(ctx, i)) {
			marker.drop();
		}
		else {
			marker.done(convertRuleIndexToScalaElementType(i));
		}
	}

	private Boolean canDropMarker(ParserRuleContext ctx, int i) {
		switch (i) {
			case ScalaLangParser.RULE_program:
			case ScalaLangParser.RULE_blockStat:
			case ScalaLangParser.RULE_semi:
			case ScalaLangParser.RULE_topStat:
			case ScalaLangParser.RULE_topStatSeq:
			case ScalaLangParser.RULE_compilationUnit:
			case ScalaLangParser.RULE_def:
			case ScalaLangParser.RULE_id:				// ???
			case ScalaLangParser.RULE_funSig:
			case ScalaLangParser.RULE_expr1:
			case ScalaLangParser.RULE_simpleExpr1:
			case ScalaLangParser.RULE_simpleExpr2:
			case ScalaLangParser.RULE_stableId1:
			case ScalaLangParser.RULE_classQualifier:
			case ScalaLangParser.RULE_functionArgTypes:
			case ScalaLangParser.RULE_existentialDcl:
			case ScalaLangParser.RULE_refineStat:
			case ScalaLangParser.RULE_ascription:
			case ScalaLangParser.RULE_exprs:
			case ScalaLangParser.RULE_resultExpr:
			case ScalaLangParser.RULE_params:
			case ScalaLangParser.RULE_classParams:
				return true;
			case ScalaLangParser.RULE_expr:
				//for (int j = 0; j < ctx.getChildCount(); j++)
				//	System.out.println(ctx.getChild(j).getText());
				return false;
			case ScalaLangParser.RULE_prefixExpr:
				String c = ctx.getChild(0).getText();
				return !(c.compareTo("+") == 0 || c.compareTo("-") == 0 || c.compareTo("!") == 0 || c.compareTo("~") == 0);
			default:
				return false;
		}
	}

	private IElementType convertRuleIndexToScalaElementType(int i) {
		switch (i) {
			case ScalaLangParser.RULE_blockExpr:
				return ScalaElementTypes.BLOCK_EXPR();
			case ScalaLangParser.RULE_block:
				return ScalaElementTypes.BLOCK();
			case ScalaLangParser.RULE_funDef:
				return ScalaElementTypes.FUNCTION_DEFINITION();
			case ScalaLangParser.RULE_paramClauses:
				return ScalaElementTypes.PARAM_CLAUSES();
			case ScalaLangParser.RULE_paramClause:
				return ScalaElementTypes.PARAM_CLAUSE();
			case ScalaLangParser.RULE_prefixExpr:
				return ScalaElementTypes.PREFIX_EXPR();
			case ScalaLangParser.RULE_infixExpr:
				return ScalaElementTypes.INFIX_EXPR();
			case ScalaLangParser.RULE_postfixExpr:
				return ScalaElementTypes.POSTFIX_EXPR();
			case ScalaLangParser.RULE_expr:
				return ScalaElementTypes.FUNCTION_EXPR();
			case ScalaLangParser.RULE_literal:
				return ScalaElementTypes.LITERAL();
			case ScalaLangParser.RULE_stableId:
				return ScalaElementTypes.STABLE_ID();
			case ScalaLangParser.RULE_type:
				return ScalaElementTypes.TYPE();
			case ScalaLangParser.RULE_existentialClause:
				return ScalaElementTypes.EXISTENTIAL_CLAUSE();
			case ScalaLangParser.RULE_compoundType:
				return ScalaElementTypes.COMPOUND_TYPE();
			case ScalaLangParser.RULE_annotType:
				return ScalaElementTypes.ANNOT_TYPE();
			case ScalaLangParser.RULE_simpleType:
				return ScalaElementTypes.SIMPLE_TYPE();
			case ScalaLangParser.RULE_typeArgs:
				return ScalaElementTypes.TYPE_ARGS();
			case ScalaLangParser.RULE_types:
				return ScalaElementTypes.TYPES();
			case ScalaLangParser.RULE_refinement:
				return ScalaElementTypes.REFINEMENT();
			case ScalaLangParser.RULE_typePat:
				return ScalaElementTypes.TYPE_PATTERN();
			case ScalaLangParser.RULE_argumentExprs:
				return ScalaElementTypes.ARG_EXPRS();
			case ScalaLangParser.RULE_enumerators:
				return ScalaElementTypes.ENUMERATORS();
			case ScalaLangParser.RULE_generator:
				return ScalaElementTypes.GENERATOR();
			case ScalaLangParser.RULE_caseClauses:
				return ScalaElementTypes.CASE_CLAUSES();
			case ScalaLangParser.RULE_caseClause:
				return ScalaElementTypes.CASE_CLAUSE();
			case ScalaLangParser.RULE_guard:
				return ScalaElementTypes.GUARD();
			case ScalaLangParser.RULE_pattern:
				return ScalaElementTypes.PATTERN();
			case ScalaLangParser.RULE_patterns:
				return ScalaElementTypes.PATTERNS();
			case ScalaLangParser.RULE_typeParamClause:
				return ScalaElementTypes.TYPE_PARAM_CLAUSE();
			case ScalaLangParser.RULE_funTypeParamClause:
				return ScalaElementTypes.TYPE_PARAM_CLAUSE();
			case ScalaLangParser.RULE_variantTypeParam:
				return ScalaElementTypes.VARIANT_TYPE_PARAM();
			case ScalaLangParser.RULE_typeParam:
				return ScalaElementTypes.TYPE_PARAM();
			case ScalaLangParser.RULE_param:
				return ScalaElementTypes.PARAM();
			case ScalaLangParser.RULE_paramType:
				return ScalaElementTypes.PARAM_TYPE();
			case ScalaLangParser.RULE_classParamClauses:
				return ScalaElementTypes.PARAM_CLAUSES();
			case ScalaLangParser.RULE_classParamClause:
				return ScalaElementTypes.PARAM_CLAUSE();
			case ScalaLangParser.RULE_classParam:
				return ScalaElementTypes.CLASS_PARAM();
			/*case ScalaLangParser.RULE_pattern1:
			return ...
			case ScalaLangParser.RULE_pattern2:
			return ...
			case ScalaLangParser.RULE_pattern3:
			return ...
			case ScalaLangParser.RULE_simplePattern:
			return ...*/



			/*case ScalaLangParser.RULE_bindings:
				return ScalaElementTypes.BINDINGS();
			case ScalaLangParser.RULE_binding:
				return ScalaElementTypes.BINDING();
			case ScalaLangParser.RULE_modifier:
				return ScalaElementTypes.MODIFIER();
			case ScalaLangParser.RULE_localModifier:
				return ScalaElementTypes.LOCAL_MODIFIER();
			case ScalaLangParser.RULE_accessModifier:
				return ScalaElementTypes.ACCESS_MODIFIER();
			case ScalaLangParser.RULE_accessQualifier:
				return ScalaElementTypes.ACCESS_QUALIFIER();
			case ScalaLangParser.RULE_annotation:
				return ScalaElementTypes.ANNOTATION();
			case ScalaLangParser.RULE_constrAnnotation:
				return ScalaElementTypes.CONSTR_ANNOTATION();
			case ScalaLangParser.RULE_templateBody:
				return ScalaElementTypes.TEMPLATE_BODY();
			case ScalaLangParser.RULE_templateStat:
				return ScalaElementTypes.TEMPLATE_STAT();
			case ScalaLangParser.RULE_selfType:
				return ScalaElementTypes.SELF_TYPE();
			case ScalaLangParser.RULE_import_:
				return ScalaElementTypes.IMPORT_();
			case ScalaLangParser.RULE_importExpr:
				return ScalaElementTypes.IMPORT_EXPR();
			case ScalaLangParser.RULE_importSelectors:
				return ScalaElementTypes.IMPORT_SELECTORS();
			case ScalaLangParser.RULE_importSelector:
				return ScalaElementTypes.IMPORT_SELECTOR();
			case ScalaLangParser.RULE_dcl:
				return ScalaElementTypes.DCL();
			case ScalaLangParser.RULE_valDcl:
				return ScalaElementTypes.VAL_DCL();
			case ScalaLangParser.RULE_varDcl:
				return ScalaElementTypes.VAR_DCL();
			case ScalaLangParser.RULE_funDcl:
				return ScalaElementTypes.FUN_DCL();
			case ScalaLangParser.RULE_funSig:
				return ScalaElementTypes.FUN_SIG();
			case ScalaLangParser.RULE_typeDcl:
				return ScalaElementTypes.TYPE_DCL();
			case ScalaLangParser.RULE_patVarDef:
				return ScalaElementTypes.PAT_VAR_DEF();
			case ScalaLangParser.RULE_def:
				return ScalaElementTypes.DEF();
			case ScalaLangParser.RULE_patDef:
				return ScalaElementTypes.PAT_DEF();
			case ScalaLangParser.RULE_varDef:
				return ScalaElementTypes.VAR_DEF();
			case ScalaLangParser.RULE_typeDef:
				return ScalaElementTypes.TYPE_DEF();
			case ScalaLangParser.RULE_tmplDef:
				return ScalaElementTypes.TMPL_DEF();
			case ScalaLangParser.RULE_classDef:
				return ScalaElementTypes.CLASS_DEF();
			case ScalaLangParser.RULE_traitDef:
				return ScalaElementTypes.TRAIT_DEF();
			case ScalaLangParser.RULE_objectDef:
				return ScalaElementTypes.OBJECT_DEF();
			case ScalaLangParser.RULE_classTemplateOpt:
				return ScalaElementTypes.CLASS_TEMPLATE_OPT();
			case ScalaLangParser.RULE_traitTemplateOpt:
				return ScalaElementTypes.TRAIT_TEMPLATE_OPT();
			case ScalaLangParser.RULE_classTemplate:
				return ScalaElementTypes.CLASS_TEMPLATE();
			case ScalaLangParser.RULE_traitTemplate:
				return ScalaElementTypes.TRAIT_TEMPLATE();
			case ScalaLangParser.RULE_classParents:
				return ScalaElementTypes.CLASS_PARENTS();
			case ScalaLangParser.RULE_traitParents:
				return ScalaElementTypes.TRAIT_PARENTS();
			case ScalaLangParser.RULE_constr:
				return ScalaElementTypes.CONSTR();
			case ScalaLangParser.RULE_earlyDefs:
				return ScalaElementTypes.EARLY_DEFS();
			case ScalaLangParser.RULE_earlyDef:
				return ScalaElementTypes.EARLY_DEF();
			case ScalaLangParser.RULE_constrExpr:
				return ScalaElementTypes.CONSTR_EXPR();
			case ScalaLangParser.RULE_constrBlock:
				return ScalaElementTypes.CONSTR_BLOCK();
			case ScalaLangParser.RULE_selfInvocation:
				return ScalaElementTypes.SELF_INVOCATION();
			case ScalaLangParser.RULE_topStatSeq:
				return ScalaElementTypes.TOP_STAT_SEQ();
			case ScalaLangParser.RULE_topStat:
				return ScalaElementTypes.TOP_STAT();
			case ScalaLangParser.RULE_packaging:
				return ScalaElementTypes.PACKAGING();
			case ScalaLangParser.RULE_packageObject:
				return ScalaElementTypes.PACKAGE_OBJECT();
			case ScalaLangParser.RULE_compilationUnit:
				return ScalaElementTypes.COMPILATION_UNIT();
			case ScalaLangParser.RULE_id:
				return ScalaElementTypes.ID();
			case ScalaLangParser.RULE_semi:
				return ScalaElementTypes.SEMI();*/


			default:
				return ScalaElementTypes.DUMMY_ELEMENT();
		}
	}
}
