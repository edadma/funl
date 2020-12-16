package xyz.hyperreal.bvm

import util.parsing.input.Position

import xyz.hyperreal.dal.Operator

trait AST

case class SourceAST(statements: List[StatementAST]) extends AST

trait StatementAST extends AST

trait DeclarationStatementAST extends StatementAST
//case class ImportAST( qual: String, names: List[(String, Option[String])] ) extends DeclarationStatementAST
//case class NativeAST( pkg: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
//case class FunctionAST( cls: String, name: List[(String, Option[String])] ) extends DeclarationStatementAST
case class ValAST(struc: StructureAST, pos: Position, exp: ExpressionAST) extends DeclarationStatementAST
case class VarAST(pos: Position, var name: String, oname: String, init: Option[ExpressionAST])
    extends DeclarationStatementAST
case class DataAST(pos: Position, name: String, constructors: List[(String, List[Symbol])])
    extends DeclarationStatementAST
case class DefAST(oname: String, func: FunctionExpressionAST) extends DeclarationStatementAST

case class DeclarationBlockAST(decls: List[DeclarationStatementAST]) extends DeclarationStatementAST

trait ExpressionAST extends StatementAST
case class SetValueExpressionAST(pos: Position, var name: String, oname: String, expr: ExpressionAST)
    extends ExpressionAST
case class NativeFunctionExpressionAST(function: VM => Any) extends ExpressionAST
case class RegexLiteralAST(pos: Position, regex: String) extends ExpressionAST
case class RangeExpressionAST(fpos: Position,
                              f: ExpressionAST,
                              tpos: Position,
                              t: ExpressionAST,
                              bpos: Position,
                              b: ExpressionAST,
                              inclusive: Boolean)
    extends ExpressionAST
case class UnboundedLazyListExpressionAST(fpos: Position, f: ExpressionAST, bpos: Position, b: ExpressionAST)
    extends ExpressionAST
case class SequenceExpressionAST(fpos: Position,
                                 f: ExpressionAST,
                                 tpos: Position,
                                 t: ExpressionAST,
                                 bpos: Position,
                                 b: ExpressionAST,
                                 inclusive: Boolean)
    extends ExpressionAST
case class GenerateExpressionAST(pos: Position, collection: ExpressionAST) extends ExpressionAST
case class GeneratorExpressionAST(structure: StructureAST,
                                  pos: Position,
                                  traversable: ExpressionAST,
                                  filter: Option[ExpressionAST])
    extends ExpressionAST
case class PartialFunctionExpressionAST(cases: List[FunctionExpressionAST]) extends ExpressionAST
case class FunctionExpressionAST(var name: String,
                                 pos: Position,
                                 parms: List[StructureAST],
                                 arb: Boolean,
                                 parts: List[FunctionPartExpressionAST],
                                 where: WhereClauseAST)
    extends ExpressionAST
case class ApplyExpressionAST(epos: Position,
                              f: ExpressionAST,
                              apos: Position,
                              args: List[(Position, ExpressionAST)],
                              var tailrecursive: Boolean)
    extends ExpressionAST
case class DotExpressionAST(epos: Position, expr: ExpressionAST, apos: Position, field: Symbol) extends ExpressionAST
case class BracketExpressionAST(epos: Position, f: ExpressionAST, apos: Position, arg: ExpressionAST)
    extends ExpressionAST
case class LiteralExpressionAST(v: Any) extends ExpressionAST
case class VariableExpressionAST(pos: Position, var name: String, oname: String) extends ExpressionAST
case class BinaryExpressionAST(lpos: Position,
                               left: ExpressionAST,
                               op: Symbol,
                               func: Operator,
                               rpos: Position,
                               right: ExpressionAST)
    extends ExpressionAST
case class UnaryExpressionAST(op: Symbol, func: Operator, pos: Position, expr: ExpressionAST) extends ExpressionAST
case class AssignmentExpressionAST(lhs: List[(Position, ExpressionAST)],
                                   op: Symbol,
                                   func: Operator,
                                   rhs: List[(Position, ExpressionAST)])
    extends ExpressionAST
case class ScanAssignmentExpressionAST(lpos: Position, lhs: ExpressionAST, rhs: ExpressionAST) extends ExpressionAST
case class ReversableAssignmentExpressionAST(lpos: Position, lhs: ExpressionAST, rhs: ExpressionAST)
    extends ExpressionAST
case class BlockExpressionAST(l: List[StatementAST]) extends ExpressionAST
case class InterpolationExpressionAST(l: List[ExpressionAST]) extends ExpressionAST
case class TupleExpressionAST(l: List[ExpressionAST]) extends ExpressionAST
case class ListExpressionAST(l: List[ExpressionAST]) extends ExpressionAST
case class SetExpressionAST(l: List[ExpressionAST]) extends ExpressionAST
case class MapExpressionAST(l: List[(ExpressionAST, ExpressionAST)]) extends ExpressionAST
case class ErrorExpressionAST(pos: Position, error: String) extends ExpressionAST
case class OrExpressionAST(left: ExpressionAST, right: ExpressionAST) extends ExpressionAST
case class AndExpressionAST(left: ExpressionAST, right: ExpressionAST) extends ExpressionAST
case class NotExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class TypeExpressionAST(expr: ExpressionAST, typ: String) extends ExpressionAST
case class ConditionalExpressionAST(cond: Seq[(ExpressionAST, ExpressionAST)], els: Option[ExpressionAST])
    extends ExpressionAST
case class EveryExpressionAST(label: Option[String],
                              gen: ExpressionAST,
                              body: Option[ExpressionAST],
                              els: Option[ExpressionAST])
    extends ExpressionAST
case class RepeatExpressionAST(label: Option[String], body: ExpressionAST) extends ExpressionAST
case class WhileExpressionAST(label: Option[String],
                              cond: ExpressionAST,
                              body: Option[ExpressionAST],
                              els: Option[ExpressionAST])
    extends ExpressionAST
case class ForLoopExpressionAST(label: Option[String],
                                gen: GeneratorExpressionAST,
                                body: Option[ExpressionAST],
                                els: Option[ExpressionAST])
    extends ExpressionAST
case class ForExpressionAST(label: Option[String],
                            gen: List[GeneratorExpressionAST],
                            body: ExpressionAST,
                            els: Option[ExpressionAST])
    extends ExpressionAST
//case class IteratorExpressionAST( structure: StructureAST, pos: Position, traversable: ExpressionAST, filter: Option[ExpressionAST] ) extends ExpressionAST
case object FailExpressionAST extends ExpressionAST
case class BreakExpressionAST(pos: Position, label: Option[String], expr: Option[ExpressionAST]) extends ExpressionAST
case class ContinueExpressionAST(pos: Position, label: Option[String]) extends ExpressionAST
case class ReturnExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class YieldExpressionAST(expr: ExpressionAST, result: Option[ExpressionAST]) extends ExpressionAST
case class SectionExpressionAST(op: Symbol, func: Operator) extends ExpressionAST
case class LeftSectionExpressionAST(pos: Position,
                                    expr: ExpressionAST,
                                    lambda: FunctionExpressionAST,
                                    op: Symbol,
                                    func: Operator,
                                    var closure: Option[Boolean] = None)
    extends ExpressionAST
case class RightSectionExpressionAST(op: Symbol,
                                     func: Operator,
                                     pos: Position,
                                     expr: ExpressionAST,
                                     lambda: FunctionExpressionAST,
                                     var closure: Option[Boolean] = None)
    extends ExpressionAST
case class ListComprehensionExpressionAST(comprehension: ComprehensionAST) extends ExpressionAST
case class SetComprehensionExpressionAST(comprehension: ComprehensionAST) extends ExpressionAST
case class ScanExpressionAST(spos: Position, subject: ExpressionAST, expr: ExpressionAST) extends ExpressionAST
case class EqExpressionAST(expr: ExpressionAST, target: ExpressionAST, result: ExpressionAST) extends ExpressionAST
case class RepeatedAlternationExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class DereferenceExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class MatchExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class DefinedExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class UndefinedExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class DefinedOptionExpressionAST(expr: ExpressionAST) extends ExpressionAST
case class SysvarExpressionAST(pos: Position, name: String) extends ExpressionAST
case class PatternExpressionAST(pat: PatternAST) extends ExpressionAST
case class PatternExpressionStringsAST(pat: PatternAST) extends ExpressionAST
case object CaptureGroupsExpressionAST extends ExpressionAST

case class ComprehensionBodyAST(expr: ExpressionAST) extends ExpressionAST

case class FunctionPartExpressionAST(guard: Option[ExpressionAST], body: ExpressionAST)
case class ComprehensionAST(expr: ExpressionAST, gen: List[GeneratorExpressionAST])

case class WhereClauseAST(where: List[DeclarationStatementAST]) extends AST

trait StructureAST extends AST
case class NamedStructureAST(pos: Position, var alias: String, s: StructureAST) extends StructureAST
case class TypeStructureAST(s: StructureAST, typename: String) extends StructureAST
case class LiteralStructureAST(v: Any) extends StructureAST
case class VariableStructureAST(pos: Position, var name: String, oname: String) extends StructureAST
case class TupleStructureAST(pos: Position, args: List[StructureAST]) extends StructureAST
case class AlternationStructureAST(l: List[StructureAST]) extends StructureAST
case class RecordStructureAST(pos: Position, name: String, args: Vector[StructureAST]) extends StructureAST
case class ListStructureAST(pos: Position, l: List[StructureAST]) extends StructureAST
case class ConsStructureAST(pos: Position, head: StructureAST, tail: StructureAST) extends StructureAST
case object NilStructureAST extends StructureAST

trait PatternAST extends AST

trait OperatorPattern {
  val subpat: PatternAST
}

case class ConcatenationPattern(seq: Seq[PatternAST]) extends PatternAST
case class AlternationPattern(alt: Seq[PatternAST]) extends PatternAST with StructureAST
case class StringPattern(s: ExpressionAST) extends PatternAST
case class LiteralPattern(s: String) extends PatternAST
case class OptionalPattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class ReluctantOptionalPattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class ZeroOrMorePattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class ReluctantZeroOrMorePattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class OneOrMorePattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class ReluctantOneOrMorePattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class RepeatPattern(subpat: PatternAST, lower: Int, pos: Position, upper: Option[Int])
    extends PatternAST
    with OperatorPattern
case class ReluctantRepeatPattern(subpat: PatternAST, lower: Int, pos: Position, upper: Option[Int])
    extends PatternAST
    with OperatorPattern
case object DotPattern extends PatternAST
case object AnyPattern extends PatternAST
case class ClassPattern(clas: Char => Boolean) extends PatternAST
case class CapturePattern(key: String, subpat: PatternAST, conversion: CharSequence => Any)
    extends PatternAST
    with OperatorPattern
case class NumberedCapturePattern(subpat: PatternAST, var n: Int = 0) extends PatternAST with OperatorPattern
case class CompiledSubPattern(forward: Compilation, reverse: Compilation) extends PatternAST
case class AtomicPattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class ReferencePattern(key: String) extends PatternAST
case object BeginningOfInputPattern extends PatternAST
case object EndOfInputPattern extends PatternAST
case class LookaheadPattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class FlagConditionalPattern(mask: Int, set: PatternAST, unset: PatternAST) extends PatternAST
case class LookaheadClassPattern(clas: Char => Boolean) extends PatternAST
case class LookbehindClassPattern(clas: Char => Boolean) extends PatternAST
case class NegationPattern(subpat: PatternAST) extends PatternAST
case class SetFlagsPattern(setmask: Int, clearmask: Int) extends PatternAST
case class SetFlagsGroupPattern(setmask: Int, clearmask: Int, subpat: PatternAST)
    extends PatternAST
    with OperatorPattern
case class LookbehindPattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class GroupPattern(subpat: PatternAST) extends PatternAST with OperatorPattern
case class NativePattern(matcher: VM => Unit = _ => ()) extends PatternAST
case object EmptyPattern extends PatternAST
