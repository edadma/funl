package xyz.hyperreal

import xyz.hyperreal.bvm.{VMClass, VMNonAppendableNonResizableNonMapSequence, VMNonSet, VMNonUpdatable, VMObject, VMUnordered}

import java.io.PrintStream
import xyz.hyperreal.char_reader.CharReader
import xyz.hyperreal.recursive_descent_parser.{Assoc, FX, FY, XF, XFX, XFY, YF, YFX}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

package object prolog {

  implicit val symbolOrdering: Ordering[Symbol] = Ordering by Symbol.unapply
  implicit val functorOrdering: Ordering[Indicator] = Ordering by { f: Indicator =>
    (f.arity, f.name)
  }
  implicit val procedureOrdering: Ordering[Procedure] = Ordering by [Procedure, Indicator] (_.ind)

  val NIL: Symbol = Symbol("[]")
  val CONS: Indicator = Indicator(Symbol("."), 2)
  val INDICATOR: Indicator = Indicator(Symbol("/"), 2)
  val TRUE: AtomAST = AtomAST(null, "true")

  val NATIVE_PREDICATE = 0
  val NATIVE_MATH = 1
  val NATIVE_RUNTIME = 2

  case class Operator(priority: Int, specifier: Assoc, operator: Symbol)

  case object EMPTY { override def toString = "[empty]" }

  case class Indicator(name: Symbol, arity: Int) { override def toString = s"${name.name}/$arity" }

  case class Procedure(ind: Indicator, var block: Block, pub: Boolean, clauses: ArrayBuffer[Clause] = new ArrayBuffer) {
    override def toString = s"[procedure $ind]"
  }

  case class Clause(ast: TermAST, head: TermAST, body: TermAST, block: Block)

  case class Structure(ind: Indicator, args: Array[VMObject])
      extends VMObject
      with VMNonAppendableNonResizableNonMapSequence
      with VMUnordered
      with VMNonUpdatable
      with VMNonSet {
    val clas: VMClass = null //todo

    def name: String = ind.name.name

    def arity: Int = args.length

    def element(n: Int): Any = args(n)

    def update(n: Int, v: VMObject): Unit = args(n) = v

    def apply(idx: VMObject): VMObject = args(idx.toInt)

    def size: Int = args.length

    def iterator: Iterator[VMObject] = args.iterator

    def head: VMObject = sys.error("no head method")

    def tail: VMObject = sys.error("no tail method")
  }

  class Vars {
    val varMap = new mutable.HashMap[String, Int]
    val evals = new mutable.HashSet[String]

    def count: Int = varMap.size

    def anon: Int = num(s"$$$count")

    def num(name: String): Int = {
      varMap get name match {
        case None =>
          val n = count

          varMap(name) = n
          n
        case Some(n) => n
      }
    }

    def get(name: String): Option[Int] = varMap get name

    def eval(name: String): Boolean =
      if (evals(name))
        false
      else {
        evals += name
        true
      }

    def evalSet: Set[String] = evals map (_ + '\'') toSet
  }

  class PrologException(msg: String, val term: Any) extends Exception(msg)

  def indicator(name: Symbol, arity: Int): Structure = Structure(INDICATOR, Array(name, arity))

  def indicator(f: Indicator): Structure = indicator(f.name, f.arity)

  def exception(r: CharReader, msg: String, term: Any) =
    throw new PrologException(if (r eq null) msg else r.longErrorText(msg), term)

  def exceptionTerm(error: Any, other: Any): Structure = Structure(Indicator(Symbol("error"), 2), Array(error, other))

  def instantiationError(r: CharReader, msg: String, name: String, arity: Int): Nothing =
    exception(r, msg, exceptionTerm(Symbol("instantiation_error"), indicator(name, arity)))

  def typeError(r: CharReader, msg: String, typ: String, culprit: Any, name: String, arity: Int): Nothing =
    exception(r, msg, exceptionTerm(Structure(Indicator(Symbol("type_error"), 2), Array(Symbol(typ), culprit)), indicator(name, arity)))

  def existenceError(r: CharReader, msg: String, obj: Symbol, culprit: Any, name: Symbol, arity: Int): Nothing =
    exception(r, msg, exceptionTerm(Structure(Indicator(Symbol("existence_error"), 2), Array(obj, culprit)), indicator(name, arity)))

  def domainError(r: CharReader, msg: String, domain: String, culprit: Any, name: String, arity: Int): Nothing =
    exception(r, msg, exceptionTerm(Structure(Indicator(Symbol("domain_error"), 2), Array(domain, culprit)), indicator(name, arity)))

  def permissionError(r: CharReader, msg: String, operation: String, typ: String, culprit: Any, name: String, arity: Int): Nothing =
    exception(r, msg, exceptionTerm(Structure(Indicator(Symbol("permission_error"), 3), Array(operation, typ, culprit)), indicator(name, arity)))

  def problem(r: CharReader, msg: String): Nothing =
    if (r eq null)
      sys.error(msg)
    else
      r.error(msg)

  def groundTerm(term: Any): Boolean =
    term match {
      case Structure(_, args)    => args forall groundTerm
      case _: Symbol | _: Number => true
      case _: VM#Variable        => false
    }

  def vareval(a: Any): Any =
    a match {
      case v: VM#Variable => v.eval
      case _              => a
    }

  @tailrec
  def list2array(s: Any, buf: ArrayBuffer[Any] = new ArrayBuffer): Option[Array[Any]] =
    s match {
      case NIL => Some(buf.toArray)
      case Structure(CONS, Array(head, tail)) =>
        buf += head
        list2array(tail, buf)
      case _ => None
    }

  def array2list(a: collection.IndexedSeq[Any]): Any = {
    var list: Any = NIL
    var idx = a.length - 1

    while (idx >= 0) {
      list = cons(a(idx), list)
      idx -= 1
    }

    list
  }

  def cons(head: Any, tail: Any): Structure = Structure(CONS, Array(head, tail))

  def indicator(name: String, arity: Int): Indicator = Indicator(Symbol(name), arity)

  val atomRegex: Regex = "([a-zA-Z][a-zA-Z0-9_]*)" r

  def display(a: Any, flags: Set[Symbol] = Set()): String = {
    def string(s: String) = {
      s.replace("\\", "\\\\").replace("\t", "\\t").replace("\n", "\\n").replace("\r", "\\r")
    }

    def operand(a: Any, priority: Int) =
      a match {
        case Structure(Indicator(name, arity), _) =>
          Operators get (name, arity) match {
            case Some(Operator(priority1, _, _)) if priority1 > priority => s"(${display(a, flags)})"
            case _                                                       => display(a, flags)
          }
        case _ => display(a, flags)
      }

    vareval(a) match {
      case Symbol(atomRegex(s)) => s
      case Symbol(s)            => s"'${string(s)}'"
      case Structure(CONS, Array(_, _)) =>
        @tailrec
        def elems(term: Any, buf: StringBuilder = new StringBuilder): String =
          term match {
            case NIL => buf.toString
            case Structure(CONS, Array(hd, tl)) =>
              buf ++= display(hd, flags)

              val tl1 = vareval(tl)

              tl1 match {
                case NIL                           =>
                case s: Structure if s.ind == CONS => buf ++= ", "
                case _                             => buf ++= " | "
              }

              elems(tl1, buf)
            case e =>
              buf ++= display(e, flags)
              buf.toString
          }

        s"[${elems(a)}]"
      case Structure(Indicator(name: Symbol, arity), args) if !flags.contains(Symbol("ignore_ops")) && Operators.defined(name, arity) =>
        Operators(name, arity) match {
          case Operator(priority, FX, operator) =>
            operator.name + operand(args(0), priority)
          case Operator(priority, FY, operator) =>
            operator.name + operand(args(0), priority + 1)
          case Operator(priority, XF, operator) =>
            operand(args(0), priority) + operator.name
          case Operator(priority, YF, operator) =>
            operand(args(0), priority + 1) + operator.name
          case Operator(priority, XFX, operator) =>
            operand(args(0), priority) + operator.name + operand(args(1), priority)
          case Operator(priority, XFY, operator) =>
            operand(args(0), priority) + operator.name + operand(args(1), priority + 1)
          case Operator(priority, YFX, operator) =>
            operand(args(0), priority + 1) + operator.name + operand(args(1), priority)
          case Operator(_, assoc, _) => sys.error(s"unknown associativity: $assoc")
        }
      case Structure(Indicator(Symbol(name), _), args) => s"$name(${args.map(display(_, flags)).mkString(",")})"
      case s: String                                   => s""""${string(s)}""""
      case v                                           => v.toString
    }
  }

  def instruction(inst: Instruction): String =
    inst match {
      case null                                          => "*** null ***"
      case JumpInst(b)                                   => s"jump $b"
      case NopInst                                       => "nop"
      case NilUnifyInst                                  => "nil unify"
      case DebugInst(msg, null)                          => s"-----  $msg"
      case DebugInst(msg, pos)                           => s"-----  $msg -- ${pos.lineText}"
      case PushInst(d)                                   => s"push $d"
      case PushVarInst(n)                                => s"pushv $n"
      case VarUnifyInst(n)                               => s"unifyv $n"
      case StructureInst(Indicator(Symbol(name), arity)) => s"pushf $name/$arity"
      case ElementUnifyInst(n)                           => s"unifye $n"
      case ReturnInst                                    => s"return"
      case FunctorInst(Indicator(Symbol(name), arity))   => s"functor $name/$arity"
      case DupInst                                       => "dup"
      case EqInst                                        => "eq"
      case NeInst                                        => "ne"
      case LtInst                                        => "lt"
      case LeInst                                        => "le"
      case GtInst                                        => "gt"
      case GeInst                                        => "ge"
      case BranchIfInst(disp)                            => s"branch if $disp"
      case BranchInst(disp)                              => s"branch $disp"
      case FailInst                                      => "fail"
      case ChoiceInst(disp)                              => s"choice $disp"
      case CutChoiceInst(disp)                           => s"cut_choice $disp"
      case CutInst                                       => "cut"
      case MarkInst(disp)                                => s"mark $disp"
      case UnmarkInst                                    => "unmark"
      case CallBlockInst                                 => "call block"
      case CallProcedureInst(p)                          => s"call $p"
      case CallIndirectInst(_, f)                        => s"call $f"
      case DropInst                                      => "drop"
      case PushFrameInst                                 => "pushfr"
      case FrameInst(vars)                               => s"frame $vars"
      case NativeInst(pred, _, _, _)                     => s"native $pred"
      case UnifyInst                                     => "unify"
      case EvalInst(_, _, v)                             => s"eval $v"
      case AddInst                                       => "add"
      case SubInst                                       => "sub"
      case MulInst                                       => "mul"
      case DivInst                                       => "div"
    }

  def dump(array: Array[Byte], start: Int, lines: Int, out: PrintStream = Console.out): Unit = {
    val addr = start - start % 16

    def printByte(b: Option[Int]): Unit =
      if (b isEmpty)
        out.print("-- ")
      else
        out.print("%02x ".format(b.get & 0xFF).toUpperCase)

    def printChar(c: Option[Int]): Unit = out.print(if (c.nonEmpty && ' ' <= c.get && c.get <= '~') c.get.asInstanceOf[Char] else '.')

    def read(addr: Int) =
      if (addr < array.length)
        Some(array(addr) & 0xFF)
      else
        None

    for (line <- addr until (addr + 16 * lines) by 16) {
      out.print("%6x  ".format(line).toUpperCase)

      for (i <- line until (line + 16)) {
        if (i % 16 == 8)
          out.print(' ')

        printByte(read(i))
      }

      val bytes = ((line + 16) min 0x10000) - line

      out.print(" " * ((16 - bytes) * 3 + 1 + (if (bytes < 9) 1 else 0)))

      for (i <- line until (line + 16))
        printChar(read(i))

      out.println()
    }
  }

}
