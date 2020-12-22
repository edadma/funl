package xyz.hyperreal.bvm

import scala.math.ScalaNumber
import scala.annotation.tailrec
import scala.collection.immutable.{ArraySeq, TreeMap}
import scala.collection.mutable.{ArrayBuffer, Map => MutableMap, Seq => MutableSeq}
import util.parsing.input.Position
import xyz.hyperreal.dal.{BasicDAL, IntType, Type, numberType, toBigInt}

import java.{lang => boxed}
import scala.collection.mutable

object VM {
  private val HALT = -1
  private val MATCH_FAIL = -1
  private val VM_STATE = -2
  private val ONE = new VMNumberObject(IntType, 1)
}

class VM(code: Compilation,
         captureTrees: ArraySeq[Node],
         scan: Boolean,
         anchored: Boolean,
         val args: Any,
         interface: VMInterface) {
  import VM._

  var seq: CharSequence = _
  protected[bvm] val stack = new ArrayBufferStack[ChoicePoint]
  protected[bvm] var flags: Int = _
  protected[bvm] var data: List[Any] = _
  protected[bvm] var ptr: Int = _
  protected[bvm] var ip: Int = _
  protected[bvm] var starts: Map[String, Int] = _
  protected[bvm] var captures: TreeMap[String, (Int, Int, Any)] = _
  protected[bvm] var frame: Frame = _
  protected[bvm] var mark: Int = _
  protected[bvm] var pos: Position = _
  var scanpos: Int = _
  protected[bvm] var globals: Array[Any] = _
  protected[bvm] var context: List[Array[Any]] = _
  protected[bvm] var ret: Int = _
  protected[bvm] var bindings = new ArrayBuffer[Any]
  var trace = false
  var limit: Int = Int.MaxValue

  protected def set(bits: Int): Boolean = (flags & bits) > 0

  protected def clear(bits: Int): Boolean = (flags & bits) == 0

  protected def prevpos: Int = ptr - 1

  protected def nextpos: Int = ptr + 1

  protected def previous: Char = seq charAt prevpos

  protected def pushMatchFailChoice(): Unit =
    stack.push(ChoicePoint(flags, data, nextpos, MATCH_FAIL, starts, captures, frame, mark, pos, ret, null))

  protected def reset(subject: CharSequence, start: Int, init: List[Any]): Unit = {
    stack.clear()
    flags = 0
    data = init.reverse
    seq = subject
    ptr = 0
    ip = start
    starts = Map[String, Int]()
    captures = TreeMap[String, (Int, Int, Any)]()
    frame = null
    mark = -1
    pos = null
    context = Nil
    ret = HALT
  }

  def matchString(s: String): Any = {
    def matches: Boolean = {
      for (k <- 0 until s.length)
        if (seq.charAt(scanpos + k) != s.charAt(k))
          return false

      true
    }

    val end = scanpos + s.length

    if (end <= seq.length)
      if (matches)
        end + 1
      else
        Fail
    else
      Fail
  }

  def tabToPosition(n: Int): Any = {
    if (n < -seq.length || n > seq.length + 1)
      Fail
    else {
      val pos =
        if (n > 0)
          n - 1
        else
          seq.length - n

      val res =
        if (scanpos > pos)
          seq.subSequence(pos, scanpos)
        else
          seq.subSequence(scanpos, pos)
      val (oldseq, oldscanpos) = (seq, scanpos)

      pushChoice(vm => {
        vm.seq = oldseq
        vm.scanpos = oldscanpos
      })
      scanpos = pos
      res
    }
  }

  def getdata: List[Any] = data

  def isEmpty: Boolean = data.isEmpty

  def top: Any =
    if (isEmpty)
      sys.error("stack underflow")
    else
      data.head

  def pop: Any = {
    val res = top

    data = data.tail
    res
  }

  def derefp: Any = deref(pop)

  def derefpi: Int = derefp.asInstanceOf[Int]

  def derefps: String = derefp.asInstanceOf[String]

  def dereft: Any = deref(top)

//	def long2bigint( v: Any ) =
//		v match {
//			case l: Long => BigInt(l)
//			case _ => v
//		}

  def push(item: Any): Unit =
    if (item == Fail) fail()
    else data = item :: data

  def pushChoice(disp: Int): Unit = pushChoice(disp, null)

  def pushChoice(disp: Int, action: VM => Unit): Unit =
    stack push ChoicePoint(flags, data, ptr, ip + disp, starts, captures, frame, mark, pos, ret, action)

  def pushChoice(action: VM => Unit): Unit =
    stack push ChoicePoint(flags, data, ptr, VM_STATE, starts, captures, frame, mark, pos, ret, action)

  protected def pushState(): Unit =
    stack push ChoicePoint(flags, data, ptr, VM_STATE, starts, captures, frame, mark, pos, ret, null)

  protected def choice(): Unit = {
    val ChoicePoint(fl, dat, idx, loc, st, caps, frm, mrk, ps, rt, action) =
      stack.pop

    flags = fl
    data = dat
    ptr = idx
    ip = loc
    starts = st
    captures = caps
    frame = frm
    mark = mrk
    pos = ps
    ret = rt

    if (action ne null)
      action(this)
  }

  protected def state(): Unit = {
    val curip = ip

    choice()
    ip = curip
  }

  protected def stateSameData(): Unit = {
    val curdata = data
    val curframe = frame

    state()
    data = curdata
    frame = curframe
  }

  protected[bvm] def fail(): Unit = {
    if (trace)
      println("--- fail ---")

    do choice() while (ip == VM_STATE)

    if (scan && !anchored && ip == MATCH_FAIL && !peoi) {
      pushMatchFailChoice()
      ip = 0
    }
  }

  protected def current: Char = seq charAt ptr

  protected def advance(): Unit = ptr = nextpos

  protected def reverse(): Unit = ptr = prevpos

  protected def index: Int = ptr

  protected def equal(ch: Char): Boolean =
    (flags & Pattern.CASE_INSENSITIVE) == 0 && current == ch || (flags & Pattern.CASE_INSENSITIVE) > 0 && Character
      .toLowerCase(current) == Character.toLowerCase(ch)

  protected def forwardStringMatch(s: String): Unit = {
    var idx = 0

    @tailrec def chars(): Unit =
      if (idx < s.length) {
        if (!eoi && equal(s.charAt(idx))) {
          advance()
          idx += 1
          chars()
        } else
          fail()
      }

    chars()
  }

  protected def reverseStringMatch(s: String): Unit = {
    var idx = s.length - 1

    @tailrec def chars(): Unit =
      if (idx >= 0) {
        if (boi)
          fail()
        else {
          reverse()

          if (equal(s.charAt(idx))) {
            idx -= 1
            chars()
          } else
            fail()
        }
      }

    chars()
  }

  protected def eoi: Boolean =
    if (ptr > seq.length)
      sys.error("ptr is past end of input")
    else
      ptr == seq.length

  protected def peoi: Boolean = ptr > seq.length

  protected def boi: Boolean =
    if (ptr < 0)
      sys.error("ptr is before beginning of input")
    else
      ptr == 0

  protected def dot: Boolean =
    (clear(Pattern.DOTALL) && (set(Pattern.UNIX_LINES) && current != '\n' || clear(Pattern.UNIX_LINES) && !TERMINATOR_CLASS(
      current))) || set(Pattern.DOTALL)

  protected def binaryOperation(lpos: Position, op: Symbol, rpos: Position): Unit = {
    val r = derefp
    val l = derefp

    op match {
      case Symbol("+") =>
        if (l.isInstanceOf[String] || r.isInstanceOf[String])
          push(display(l) + display(r))
        else
          l match {
            case value: collection.Map[_, _] if r.isInstanceOf[collection.Map[_, _]] =>
              push(value ++ r.asInstanceOf[collection.Map[_, _]])
            case value: collection.Map[_, _] if r.isInstanceOf[Iterable[_]] =>
              push(
                value ++ r
                  .asInstanceOf[Iterable[Vector[Any]]]
                  .map(v => (v(0), v(1))))
            case value: Iterable[_] if r.isInstanceOf[Iterable[_]] =>
              push(value ++ r.asInstanceOf[Iterable[_]])
            case _ =>
              if (!l.isInstanceOf[VMNumber])
                problem(lpos, s"not a number: ${display(l)}")
              else if (!r.isInstanceOf[VMNumber])
                problem(rpos, s"not a number: ${display(r)}")
              else
                push(BasicDAL.compute(op, l.asInstanceOf[VMNumber], r.asInstanceOf[VMNumber], interface.number))
          }
      case Symbol("=") | Symbol("!=") =>
        if ((l, r) match {
              case (a: VMNumber, b: VMNumber) => BasicDAL.relate(op, a, b)
              case _                          => if (op == Symbol("=")) l == r else l != r
            })
          push(r)
        else
          fail()
      case Symbol("<") | Symbol(">") | Symbol("<=") | Symbol(">=") | Symbol("div") =>
        if (BasicDAL.relate(op, l.asInstanceOf[VMNumber], r.asInstanceOf[VMNumber]))
          push(r)
        else
          fail()
      case Symbol(":") =>
        r match {
          case s: VMList => push(interface.cons(l.asInstanceOf[VMObject], s))
          case _         => problem(rpos, s"not a list: ${display(r)}")
        }
      case Symbol("in") | Symbol("notin") => // todo: implement 'in'
      case _ =>
        if (!l.isInstanceOf[VMNumber])
          problem(lpos, s"not a number: ${display(l)}")
        else if (!r.isInstanceOf[VMNumber])
          problem(rpos, s"not a number: ${display(r)}")
        else
          push(BasicDAL.perform(op, l.asInstanceOf[VMNumber], r.asInstanceOf[VMNumber], interface.number))
    }
  }

  protected def callIndirect(callable: Any, fpos: Position, apos: Position, ps: List[Position], argc: Int): Unit = {
    callable match {
      case SectionOperation(op) =>
        if (argc != 2)
          problem(apos, s"wrong number of arguments for a section: found $argc, expected 2")

        binaryOperation(ps.head, op, ps.tail.head)
      case LeftSectionOperation(lpos, l, op) =>
        if (argc != 1)
          problem(apos, s"wrong number of arguments for a left section: found $argc, expected 1")

        val r = derefp

        push(l)
        push(r)
        binaryOperation(lpos, op, ps.head)
      case RightSectionOperation(op, rpos, r) =>
        if (argc != 1)
          problem(apos, s"wrong number of arguments for a right section: found $argc, expected 1")

        push(r)
        binaryOperation(ps.head, op, rpos)
      case FunctionReference(entry, name, arity, locals) =>
        if (argc != arity)
          problem(apos, s"wrong number of arguments for '$name': found $argc, expected $arity")

        pos = apos
        context = locals
        ret = ip
        ip = entry
      case c: RecordConstructor =>
        if (argc != c.arity)
          problem(apos, s"expecting ${c.arity} arguments")

        val args = new Array[Any](c.arity)

        for (i <- c.arity - 1 to 0 by -1)
          args(i) = derefp

        push(new Record(c.name, ArraySeq.from(args), c.symbolMap, c.stringMap))
      case r: Record =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        derefp match {
          case s: String =>
            r.stringMap get s match {
              case None    => problem(apos, s"not a field of record '${r.name}'")
              case Some(n) => push(r.element(n))
            }
          case n: VMNumber if n.typ == IntType =>
            if (0 <= n.value.intValue && n.value.intValue < r.arity)
              push(r.element(n.value.intValue))
            else
              problem(apos, s"index out of range: $n")
          case _ => problem(apos, "expected a string or integer")
        }
      case s: VMObject if s.isSequence =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        derefp match {
          case idx: VMNumber if idx.typ == IntType && (idx.value.intValue < 0 || idx.value.intValue >= s.length) =>
            problem(ps.head, s"sequence (of length ${s.length}) index out of range: $idx")
          case idx: VMNumber if idx.typ == IntType =>
            push(s.apply(idx.value.intValue))
          case idx => problem(ps.head, s"expected integer sequence index: $idx")
        }
      case a: Array[Any] =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        derefp match {
          case idx: Int if idx < 0 || idx >= a.length =>
            problem(ps.head, s"array (of length ${a.length}) index out of range: $idx")
          case idx: Int => push(new MutableSeqAssignable(a, idx))
          case idx      => problem(ps.head, s"expected integer array index: $idx")
        }
      case ms: MutableSeq[_] =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        derefp match {
          case idx: Int if idx < 0 || idx >= ms.length =>
            problem(ps.head, s"sequence (of length ${ms.length}) index out of range: $idx")
          case idx: Int =>
            push(new MutableSeqAssignable(ms.asInstanceOf[MutableSeq[Any]], idx))
          case idx => problem(ps.head, s"expected integer sequence index: $idx")
        }
      case m: MutableMap[_, _] =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        val arg = derefp

//        if (m.asInstanceOf[MutableMap[Any, Any]].contains(arg))
        push(new MutableMapAssignable(m.asInstanceOf[MutableMap[Any, Any]], arg))
//        else
//          push(undefined)
      case m: collection.Map[_, _] =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        val arg = derefp

        push(
          m.asInstanceOf[collection.Map[Any, Any]]
            .getOrElse(arg, undefined /*problem(apos, s"key not found: ${display(arg)}")*/ ))
      case s: collection.Seq[Any] =>
        if (argc != 1)
          problem(apos, "a function application with one argument was expected")

        derefp match {
          case n: Integer =>
            if (0 <= n && n < s.length)
              push(s(n))
            else
              problem(apos, s"index out of range: $n")
          case _ => problem(apos, "expected a string or (small) integer")
        }
      case s: collection.Set[_] =>
        push(s.asInstanceOf[collection.Set[Any]](derefp))
      case p: Product => push(p.productElement(derefp.asInstanceOf[Int]))
      case s: String  => push(s.charAt(derefp.asInstanceOf[Int]).toString)
      case f: NativeFunction =>
        push(
          if (argc == 1)
            f.asInstanceOf[(VM, Position, List[Position], Any) => Any](this, apos, ps, pop)
          else if (argc == 0)
            f.asInstanceOf[(VM, Position, List[Position], Any) => Any](this, apos, ps, ArgList())
          else {
            val args = new Array[Any](argc)

            for (i <- argc - 1 to 0 by -1)
              args(i) = pop

            f.asInstanceOf[(VM, Position, List[Position], Any) => Any](this, apos, ps, ArgList(args.toIndexedSeq: _*))
          })
      case o => problem(fpos, s"not applicable: ${display(o)}")
    }
  }

  protected def rewrap(objs: List[Any], types: Array[Class[_]]): Seq[Object] = {
    val varargs = types.length > 0 && types(types.length - 1).getName == "scala.collection.Seq"

    def wrap(l: List[Any], idx: Int): List[Any] =
      if (varargs && idx == types.length - 1)
        List(l)
      else if (!varargs && idx == types.length)
        Nil
      else
        ((l.head, types(idx).getName) match {
          case (x: Number, "java.lang.Number") => x
          case (x: BigInt, _) if x.isValidLong =>
            x.longValue.asInstanceOf[AnyRef]
          case (x: ScalaNumber, _) => x.underlying
//						case (x: Closure, "funl.interp.Evaluator$Closure") => x
//						case (x: Closure, "scala.Function0") => x.function0
//						case (x: Closure, "scala.Function1") => x.function1
//						case (x: Closure, "scala.Function2") => x.function2
          case (x, _) => x.asInstanceOf[AnyRef]
        }) :: wrap(l.tail, idx + 1)

    wrap(objs, 0).asInstanceOf[Seq[Object]]
  }

  protected def assignable(objs: List[Any], types: Array[Class[_]]): Boolean = {
    val len = objs.length
    val varargs = types.length > 0 && types(types.length - 1).getName == "scala.collection.Seq"

    if (varargs && len < types.length - 1 || !varargs && len != types.length)
      false
    else {
      (objs zip types zipWithIndex).forall({
        case ((a, t), idx) =>
          if (a == null)
            true //todo: should be false for primitive type parameters
          else {
            val cls = a.getClass

            t.getName == "int" && cls.getName == "java.lang.Integer" ||
            t.getName == "double" && cls.getName == "java.lang.Double" ||
            t.getName == "boolean" && cls.getName == "java.lang.Boolean" ||
            t.getName == "long" && (cls.getName == "java.lang.Long" ||
            cls.getName == "scala.math.BigInt") ||
            /*(t.getName == "scala.Function0" || t.getName == "scala.Function1" || t.getName == "scala.Function2") && a.isInstanceOf[Closure] ||*/
            t.getName == "scala.collection.Seq" && idx == types.length - 1 ||
            t.isAssignableFrom(cls)
          }
      })
    }
  }

  private def subsequence(start: Int) =
    if (start > ptr) new SubSequence(seq, ptr, start)
    else new SubSequence(seq, start, ptr)

  protected def run(): Unit = {
    var count = 0

    while (ip >= 0 && (!trace || count < limit)) {
      count += 1

      if (trace) println(this)

      if (ip < code.length) {
        val loc = ip

        ip += 1

        code(loc) match {
          case NativeInst(function) => function(this)
          case CallInst(disp) =>
            push(Return(ip))
            ip += disp
          case CallIndirectInst(fpos, apos, ps, argc) =>
            callIndirect(derefp, fpos, apos, ps, argc)
          case DotOperatorInst(epos, apos, field) =>
            derefp match {
              case r: Record =>
                r.symbolMap get field match {
                  case None =>
                    problem(apos, s"'$field' not a field of record '${r.name}'")
                  case Some(n) => push(r.element(n))
                }
              case m: MutableMap[_, _] =>
                push(new MutableMapAssignable(m.asInstanceOf[MutableMap[Any, Any]], field.name))
              case m: collection.Map[_, _] =>
                push(
                  m.asInstanceOf[collection.Map[Any, Any]]
                    .getOrElse(field.name, undefined))
              case null        => problem(epos, "null value")
              case `undefined` => problem(epos, "undefined value")
            }
          case ReturnInst =>
            ip = pop.asInstanceOf[Return].ret
          case FunctionReturnInst =>
            val res = pop

            ip = frame.ret
            frame = pop.asInstanceOf[Frame]

            if (frame eq null)
              ret = HALT
            else
              ret = frame.ret

            push(res)
          case RepeatBeginInst(lower, upper) => push(Repetition(lower, upper))
          case RepeatLoopInst(disp) =>
            val Repetition(lower, upper, current) = pop

            def iterate(): Unit = push(Repetition(lower, upper, current + 1))

            if (current < lower)
              iterate()
            else if (upper.isEmpty || current < upper.get) {
              pushChoice(disp)
              iterate()
            } else
              ip += disp
          case LowerRepeatLoopInst(disp) =>
            val Repetition(count, upper, current) = pop

            if (current >= count)
              ip += disp

            push(Repetition(count, upper, current + 1))
          case UpperRepeatLoopInst(disp) =>
            val Repetition(lower, upper, current) = pop

            if (upper.isEmpty || current <= upper.get) {
              push(Repetition(lower, upper, current + 1))
              pushChoice(0)
              pop
            }

            ip += disp
          case AtomicInst => push(Cut(stack.size))
          case DropInst   => pop
          case CutInst =>
            pop match {
              case Cut(size) => stack.discard(stack.size - size)
            }
          case MarkInst(disp) =>
            pushChoice(disp)
            mark = stack.size
          case MarkStateInst =>
            pushState()
            mark = stack.size
          case UnmarkInst =>
            stack.discard(stack.size - mark)
            state()
          case UnmarkSameDataInst =>
            stack.discard(stack.size - mark)
            stateSameData()
          case ChangeMarkInst(disp) =>
            val ChoicePoint(flags, dat, ptr, _, starts, captures, frm, mrk, ps, rt, action) = stack(
              stack.size - mark + 1)

            stack(stack.size - mark + 1) =
              ChoicePoint(flags, dat, ptr, ip + disp, starts, captures, frm, mrk, ps, rt, action)
          case ClassInst(clas) =>
            if (!eoi && !clas(current))
              fail()
          case ClassReverseInst(clas) =>
            if (!boi && !clas(previous))
              fail()
          case CaptureBeginInst(parm, n) =>
            starts += (parm -> index)

            if (n > -1) {
              def clear(branches: List[Node]): Unit =
                for (Node(n, s) <- branches) {
                  captures += (n -> (0, 0, ""))
                  clear(s)
                }

              clear(captureTrees(n).branches)
            }
          case CaptureSaveInst(parm, conversion) =>
            val start = starts(parm)
            val sub = subsequence(start)
            val obj =
              if (conversion eq null)
                sub
              else
                conversion(sub)

            captures += (parm -> (start, index, obj))
          case AdvanceInst =>
            if (eoi)
              fail()
            else
              advance()
          case ReverseInst =>
            if (boi)
              fail()
            else
              reverse()
          case DotInst =>
            if (!eoi && dot)
              advance()
            else
              fail()
          case DotReverseInst =>
            if (boi)
              fail()
            else {
              reverse()

              if (!dot)
                fail()
            }
          case BranchInst(disp) => ip += disp
          case ZeroLengthInst   => push(pop.asInstanceOf[Pointer].p == ptr)
          case BranchIfInst(disp) =>
            if (pop.asInstanceOf[Boolean])
              ip += disp
          case BranchIfNotInst(disp) =>
            if (!pop.asInstanceOf[Boolean])
              ip += disp
          case FlagsClearInst(mask)       => push((flags & mask) == 0)
          case ChoiceInst(disp)           => pushChoice(disp)
          case StringMatchInst            => forwardStringMatch(derefps)
          case StringMatchReverseInst     => reverseStringMatch(derefps)
          case LiteralMatchInst(s)        => forwardStringMatch(s)
          case LiteralMatchReverseInst(s) => reverseStringMatch(s)
          case ReferenceMatchInst(key) =>
            forwardStringMatch(captures(key).asInstanceOf[String])
          case ReferenceMatchReverseInst(key) =>
            reverseStringMatch(captures(key).asInstanceOf[String])
          case SavePointerInst     => push(Pointer(ptr))
          case RestorePositionInst => ptr = pop.asInstanceOf[Pointer].p
          case PushMatchedInst =>
            push(subsequence(pop.asInstanceOf[Pointer].p))
          case PushCaptureGroupsInst => push(captures)
          case PushCaptureGroupsStringsInst =>
            push(captures map {
              case (k, (_, _, c)) =>
                (k, if (c.isInstanceOf[CharSequence]) c.toString else c)
            })
          case BeginningPositionMatchInst =>
            if (!boi)
              fail()
          case EndPositionMatchInst =>
            if (!eoi)
              fail()
          case FailInst         => fail()
          case FailIfFalseInst  => if (dereft == false) fail()
          case SaveFlagsInst    => push(Flags(flags))
          case RestoreFlagsInst => flags = pop.asInstanceOf[Flags].f
          case SetFlagsInst(setmask, clearmask) =>
            flags |= setmask
            flags &= ~clearmask
          case PushInst(a) =>
            a match {
              case n: Number => push(interface.number(numberType(n), n))
              case _         => push(a)
            }
          case PushFunctionInst(compute) => push(compute(this))
          case PushFunctionReferenceInst(entry, name, arity, fidx) =>
            push(
              FunctionReference(entry,
                                name,
                                arity,
                                if (frame eq null) Nil
                                else frame.locals.tails.drop(fidx).next()))
          case LocalInst(fidx, idx, _) => push(frame.locals(fidx)(idx))
          case SetLocalInst(idx, variable) =>
            frame.locals.head(idx) = if (variable) new VariableAssignable(derefp) else derefp
          case GlobalInst(idx, _) => push(globals(idx))
          case SetGlobalInst(idx, variable) =>
            globals(idx) = if (variable) new VariableAssignable(derefp) else derefp
          case SetGlobalsInst(idxs) =>
            for ((gi, i) <- idxs zipWithIndex)
              globals(gi) = bindings(i)
          case SetLocalsInst(idxs) =>
            for ((li, i) <- idxs zipWithIndex)
              frame.locals.head(li) = bindings(i)
          case GlobalsInst(globalc) => globals = new Array(globalc)
          case FrameInst(localc) =>
            push(frame)
            frame = Frame(new Array[Any](localc) :: context, ret)
          case ConcatenateInst(parts) =>
            val buf = new StringBuilder

            for (_ <- 1 to parts)
              buf ++= derefp.toString

            push(buf.toString)
          case TupleInst(arity) =>
            val a = new Array[Any](arity)

            for (i <- arity - 1 to 0 by -1)
              a(i) = derefp

            push(new Tuple(ArraySeq.from(a)))
          case TupleElementInst(n) =>
            push(derefp.asInstanceOf[TupleLike].element(n))
          case DupInst => push(top)
          case DupUnderInst =>
            val t = pop

            push(top)
            push(t)
          case OverInst =>
            val b = pop
            val a = pop

            push(a)
            push(b)
            push(a)
          case SwapInst =>
            val b = pop
            val a = pop

            push(b)
            push(a)
          case BindingsInst => bindings.clear()
          case BindingInst  => bindings += derefp
          case MatchBindingInst(idx) =>
            if (bindings(idx) != derefp)
              fail()
          case TypeCheckInst(struc, _) =>
            struc match {
              case TupleStructureAST(_, l) =>
                val arity = l.length

                dereft match {
                  case t: Tuple if t.arity == arity =>
                  case _: Tuple =>
                    fail() //problem( tpos, s"arity mismatch: expected arity of $arity, but actual arity was ${t.arity}" )
                  case _ =>
                    fail() //problem( tpos, s"type mismatch: expected tuple of arity $arity: $o" )
                }
              case NilStructureAST =>
                dereft match {
                  case s: Seq[_] if s.isEmpty =>
                  case _ =>
                    fail() //problem( tpos, "expected a non-empty sequence" )
                }
              case ConsStructureAST(_, _, _) | ListStructureAST(_, _) =>
                dereft match {
                  case s: Seq[_] if s.nonEmpty =>
                  case _ =>
                    fail() //problem( tpos, "expected a non-empty sequence" )
                }
              case RecordStructureAST(_, name, args) =>
                dereft match {
                  case r: Record if r.name == name && r.arity == args.length =>
                  case _ =>
                    fail() //problem( tpos, s"expected a record: '$name'" )//todo: fail with reason???
                }
              case _ =>
            }
          case ListInst(len) =>
            var l: VMList = interface.nil

            for (_ <- 1 to len)
              l = interface.cons(derefp.asInstanceOf[VMObject], l)

            push(l)
          case SetInst =>
            val elem = derefp

            push(derefp.asInstanceOf[Set[Any]] + elem)
          case ListHeadInst            => push(derefp.asInstanceOf[VMCons].head)
          case ListTailInst            => push(derefp.asInstanceOf[VMCons].tail)
          case EqInst                  => push(derefp == derefp)
          case ErrorInst(p, error)     => problem(p, error)
          case CallingErrorInst(error) => problem(pos, error)
          case MapInst =>
            val v = derefp
            val k = derefp

            push(derefp.asInstanceOf[Map[Any, Any]] + (k -> v))
          case DerefInst => push(derefp)
          case AssignmentInst(len, lpos, op, rpos) =>
            val rhs = for (_ <- 1 to len) yield derefp
            val lhs = for (_ <- 1 to len) yield pop

            def assignment(): Unit = {
              var res: Any = null

              for (i <- len - 1 to 0 by -1)
                if (op eq null)
                  lhs(i) match {
                    case l: Assignable =>
                      rhs(i) match {
                        case `undefined` =>
                          problem(rpos(i), "'undefined' may not be assigned to a variable")
                        case r =>
                          l.value = r
                          res = l.value
                      }
                    case o => problem(lpos(i), s"not an l-value: $o")
                  } else
                  (op, deref(lhs(i))) match {
                    case (Symbol("-"), s: mutable.Shrinkable[_]) =>
                      s.asInstanceOf[mutable.Shrinkable[Any]] -= rhs(i)
                      res = s
                    case (Symbol("--"), s: mutable.Shrinkable[_]) =>
                      s.asInstanceOf[mutable.Shrinkable[Any]] --= rhs(i)
                        .asInstanceOf[IterableOnce[Any]]
                      res = s
                    case (Symbol("+"), g: mutable.Growable[_]) =>
                      g.asInstanceOf[mutable.Growable[Any]] += rhs(i)
                      res = g
                    case (Symbol("++"), g: mutable.Growable[_]) =>
                      g.asInstanceOf[mutable.Growable[Any]] ++= rhs(i)
                        .asInstanceOf[IterableOnce[Any]]
                      res = g
                    case _ =>
                      lhs(i) match {
                        case l: Assignable =>
                          (op, l.value) match {
                            case (Symbol("+"), seq: collection.Seq[Any]) =>
                              l.value = seq :+ rhs(i)
                            case (Symbol("+"), set: collection.Set[_]) =>
                              l.value = set
                                .asInstanceOf[collection.Set[Any]] concat List(rhs(i))
                            case (Symbol("+"), s: String) =>
                              l.value = s + rhs(i)
                            case (Symbol("<") | Symbol(">"), n: Number) =>
                              if (!rhs(i).isInstanceOf[Number])
                                problem(rpos(i), s"not a number: ${display(rhs(i))}")

                              if (BasicDAL.relate(op, n, rhs(i).asInstanceOf[Number]))
                                l.value = rhs(i)
                              else {
                                fail()
                                return
                              }
                            case (Symbol("<"), s: String) =>
                              if (s < String.valueOf(rhs(i)))
                                l.value = rhs(i)
                              else {
                                fail()
                                return
                              }
                            case (Symbol(">"), s: String) =>
                              if (s > String.valueOf(rhs(i)))
                                l.value = rhs(i)
                              else {
                                fail()
                                return
                              }
                            case (_, n: VMNumber) =>
                              if (!rhs(i).isInstanceOf[VMNumber])
                                problem(rpos(i), s"not a number: ${display(rhs(i))}")
                              else
                                l.value = BasicDAL.perform(op, n, rhs(i).asInstanceOf[VMNumber], interface.number)
                            case _ => problem(lpos(i), s"illegal assignment: '${op.name}'")
                          }

                          res = l.value
                        case _ =>
                          problem(lpos(i), s"not an l-value: ${display(lhs(i))}")
                      }
                  }

              push(res)
            }

            assignment()
          case UnaryInst(op, pos) => //todo: reimplement all '*' operations so that UnaryInst only represents a single DAL operation
            val v = pop
            val d = deref(v)

            if (!d.isInstanceOf[VMNumber])
              problem(pos, s"not a number: ${display(d)}")

            val n = d.asInstanceOf[VMNumber]

            op match {
              case Symbol("-") => push(BasicDAL.negate(n, interface.number))
              case Symbol("++*") | Symbol("--*") | Symbol("*++") | Symbol("*--") if !v.isInstanceOf[Assignable] =>
                problem(pos, "not an l-value")
              case Symbol("++*") | Symbol("--*") =>
                val res = BasicDAL.compute(op, d.asInstanceOf[VMNumber], ONE, interface.number)

                v.asInstanceOf[Assignable].value = res
                push(res)
              case Symbol("*++") | Symbol("*--") =>
                v.asInstanceOf[Assignable].value =
                  BasicDAL.compute(Symbol(op.name.last.toString), d.asInstanceOf[VMNumber], ONE, interface.number)
                push(d)
            }
          case BinaryInst(lpos, op, rpos) =>
            binaryOperation(lpos, op, rpos)
          case GeneratorInst => // todo: create Iterable, Range types
            def generate(g: GeneratorTemp): Unit = {
              if (g.it.hasNext) {
                push(g)
                pushChoice(-1)
                pop
                push(g.it.next())
              } else
                fail()
            }

            derefp match {
              case s: String =>
                generate(GeneratorTemp(s.iterator map (_.toString)))
              case a: Array[_] =>
                generate(
                  GeneratorTemp(a.indices.iterator map (new MutableSeqAssignable(a.asInstanceOf[Array[Any]], _))))
              case ms: MutableSeq[_] =>
                generate(
                  GeneratorTemp(
                    ms.indices.iterator map (new MutableSeqAssignable(ms.asInstanceOf[MutableSeq[Any]], _))))
              case o: VMObject if o.isIterable => generate(GeneratorTemp(o.iterator))
              case it: Iterator[_]             => generate(GeneratorTemp(it))
              case g: GeneratorTemp            => generate(g)
              case v                           => push(v)
            }
//					case IteratorInst( pos ) =>
//						derefp match {
//							case s: String => push( IteratorTemp(s.iterator map (_.toString)) )
//							case a: Array[_] => push( IteratorTemp(a.indices.iterator map (new MutableSeqAssignable(a.asInstanceOf[Array[Any]], _))) )
//							case ms: MutableSeq[_] => push( IteratorTemp(ms.indices.iterator map (new MutableSeqAssignable(ms.asInstanceOf[MutableSeq[Any]], _))) )
//							case t: Iterable[_] => push( IteratorTemp(t.iterator) )
//							case it: Iterator[_] => push( IteratorTemp(it) )
//							case v => problem( pos, s"non-iterable: $v" )
//						}
//					case IterateInst =>
//						derefp match {
//							case IteratorTemp( it ) =>
//								if (it.hasNext)
//									push( it.next )
//								else
//									fail
//						}
          case RangeInst(pf, pt, pb, inclusive) =>
            val b =
              derefp match {
                case n: VMNumber => n.value
                case v =>
                  problem(pb, s"expected a number as range end value: ${display(v)}")
              }
            val t =
              derefp match {
                case n: VMNumber => n.value
                case v =>
                  problem(pt, s"expected a number as range end value: ${display(v)}")
              }

            push(interface.range(derefp.asInstanceOf[VMNumber].value, t, b, inclusive))
          //                problem(pf, s"expected a number (real and not a fraction) as the range initial value: ${display(v)}")
          case UnboundedLazyListInst(fpos, bpos) =>
            val b =
              derefp match {
                case n: VMNumber => n
                case o =>
                  problem(bpos, s"expected step value to be a number: $o")
              }
            val f =
              derefp match {
                case n: VMNumber => n
                case o =>
                  problem(fpos, s"expected start value to be a number: $o")
              }

            push(LazyList.iterate(f)(v => BasicDAL.compute(Symbol("+"), v, b, interface.number)))
          case CommentInst(_) =>
          case EmptyInst      => push(derefp.asInstanceOf[Seq[_]].isEmpty)
          case HaltInst       => ip = HALT
          case BeginScanInst(spos) =>
            derefp match {
              case s: String =>
                val (oldseq, oldscanpos, oldptr) = (seq, scanpos, ptr)

                pushChoice(_ => {
                  seq = oldseq
                  scanpos = oldscanpos
                  ptr = oldptr
                })
                push(seq)
                push(scanpos)
                push(ptr)
                seq = s
                scanpos = 0
                ptr = 0
              case o => problem(spos, s"expected a string: $o")
            }
          case EndScanInst =>
            val (envseq, envscanpos, envptr) = (seq, scanpos, ptr)

            pushChoice(_ => {
              seq = envseq
              scanpos = envscanpos
              ptr = envptr
            })

            val res = pop

            ptr = pop.asInstanceOf[Int]
            scanpos = pop.asInstanceOf[Int]
            seq = pop.asInstanceOf[String]
            push(res)
          case DataBacktractInst(pos) =>
            pop match {
              case a: Assignable =>
                val oldvalue = a.value

                pushChoice(_ => a.value = oldvalue)
              case o => problem(pos, s"not assignable: $o")
            }
          case VectorInst =>
            val elem = derefp

            push(derefp.asInstanceOf[Vector[Any]] :+ elem)
          case ToListInst =>
            push(derefp.asInstanceOf[IterableOnce[Any]].iterator.to(List))
          case ToSetInst =>
            push(derefp.asInstanceOf[IterableOnce[Any]].iterator.to(Set))
          case BracketInst(epos, apos) =>
            val arg = derefp

            derefp match {
              case s: String =>
                arg match {
                  case n: Int =>
                    if (n < -s.length || n > s.length || n == 0)
                      problem(apos, s"out of range: $n")

                    push(s.charAt(if (n <= 0) s.length + n else n - 1).toString)
                  case x => problem(apos, s"expected integer: $x")
                }
              case o =>
                problem(epos, s"don't know what to do with '$o' in bracket expression")
            }
          case PushNoneInst => push(None)
          case SomeInst     => push(Some(derefp))
        }
      } else if (ip > code.length)
        sys.error(s"ip > code.length: $ip")
      else if (anchored && !eoi) {
        fail()
      } else
        return
    }
  }

  def execute: Any = {
    reset("", 0, Nil)
    run()

    if (data nonEmpty) data.head else ()
  }

  def global(idx: Int): Any = globals(idx)

  def call(callable: Any, args: List[Any]): Any = {
    reset("", HALT, args)

    val argc = args.length

    callIndirect(callable, null, null, List.fill(argc)(null), argc)
    run()
    data.head
  }

  def call(entry: Int, args: List[Any]): Any = {
    reset("", entry, args)
    run()
    data.head
  }

  def matches(subject: CharSequence): Boolean = {
    reset(subject, 0, Nil)
    pushMatchFailChoice()
    run()
    ip >= 0
  }

  def rematches: Boolean =
    if (ip >= 0) {
      if (trace)
        println("--- rematch ---")

      fail()
      run()
      ip >= 0
    } else
      false

  def groups: Map[String, (Int, Int, Any)] = captures

  override def toString: String = {
    val inst =
      if (ip >= code.length)
        "end-of-code"
      else if (ip < 0)
        "failure"
      else
        String.valueOf(code(ip))
    val char =
      if (peoi)
        "past end-of-input"
      else if (eoi)
        "end-of-input"
      else if (ptr < 0)
        "before-input"
      else
        current toString

    s"ip: $ip, inst: $inst, ptr: $char, starts: $starts, groups: $captures, data: $data, stack: $stack"
  }

  protected case class ChoicePoint(fl: Int,
                                   dat: List[Any],
                                   p: Int,
                                   ip: Int,
                                   strts: Map[String, Int],
                                   cptrs: TreeMap[String, (Int, Int, Any)],
                                   frm: Frame,
                                   mrk: Int,
                                   ps: Position,
                                   rt: Int,
                                   actn: VM => Unit)

  protected case class Repetition(lower: Int, upper: Option[Int], current: Int = 0)
  protected case class Cut(size: Int)
  protected case class Return(ret: Int)
  protected case class Pointer(p: Int)
  protected case class Scanning(m: Mode, p: Int)
  protected case class Flags(f: Int)

  protected case class GeneratorTemp(it: Iterator[Any])
  protected case class IteratorTemp(it: Iterator[Any])

  protected class bvmrorException extends RuntimeException
}

trait Mode
case object Forward extends Mode
case object Reverse extends Mode

case class ArgList(array: Any*)

case class Frame(locals: List[Array[Any]], ret: Int)
case class FunctionReference(var entry: Int, name: String, arity: Int, context: List[Array[Any]])

case class SectionOperation(op: Symbol)
case class LeftSectionOperation(lpos: Position, l: Any, op: Symbol)
case class RightSectionOperation(op: Symbol, rpos: Position, r: Any)

case class RecordConstructor(typename: String, name: String, fields: List[Symbol]) {
  val arity: Int = fields.length
  val symbolMap = Map(fields zipWithIndex: _*)
  val stringMap = Map(fields map (_.name) zipWithIndex: _*)
}

trait TupleLike extends Iterable[Any] {
  def arity: Int

  def element(idx: Int): Any

  def iterator: Iterator[Any] =
    new Iterator[Any] {
      private var idx = 0

      def hasNext: Boolean = idx < arity

      def next(): Any =
        if (hasNext) {
          val res = element(idx)

          idx += 1
          res
        }
    }
}

class Record(val name: String, elems: IndexedSeq[Any], val symbolMap: Map[Symbol, Int], val stringMap: Map[String, Int])
    extends TupleLike {
  val arity: Int = elems.length

  def element(idx: Int): Any = elems(idx)

  override def toString: String = if (arity == 0) name else s"$name( ${elems mkString ", "} )"
}

class Tuple(elems: IndexedSeq[Any]) extends TupleLike {
  val arity: Int = elems.length

  def element(idx: Int): Any = elems(idx)

  override def toString = s"(${elems mkString ", "})"
}

case object Fail

object undefined {
  override def toString = "undefined"
}

private case class Node(n: String, branches: List[Node])

sealed trait VMInst

case object StringMatchInst extends VMInst
case object StringMatchReverseInst extends VMInst
case class LiteralMatchInst(s: String) extends VMInst
case class LiteralMatchReverseInst(s: String) extends VMInst
case class ReferenceMatchInst(key: String) extends VMInst
case class ReferenceMatchReverseInst(key: String) extends VMInst
case class BranchInst(disp: Int) extends VMInst
case class BranchIfInst(disp: Int) extends VMInst
case class BranchIfNotInst(disp: Int) extends VMInst
case object ZeroLengthInst extends VMInst
case class FlagsClearInst(mask: Int) extends VMInst
case class ChoiceInst(disp: Int) extends VMInst
case object AdvanceInst extends VMInst
case object ReverseInst extends VMInst
case object DotInst extends VMInst
case object DotReverseInst extends VMInst
case class ClassInst(clas: Char => Boolean) extends VMInst
case class ClassReverseInst(clas: Char => Boolean) extends VMInst
case class CaptureBeginInst(parm: String, n: Int) extends VMInst
case class CaptureSaveInst(parm: String, conversion: CharSequence => Any) extends VMInst
case object AtomicInst extends VMInst
case object CutInst extends VMInst
case class RepeatBeginInst(lower: Int, upper: Option[Int]) extends VMInst
case class RepeatLoopInst(disp: Int) extends VMInst
case class UpperRepeatLoopInst(disp: Int) extends VMInst
case class LowerRepeatLoopInst(disp: Int) extends VMInst
case class CallInst(var disp: Int) extends VMInst
case class CallIndirectInst(fpos: Position, apos: Position, ps: List[Position], argc: Int) extends VMInst
case object ReturnInst extends VMInst
case object SavePointerInst extends VMInst
case object RestorePositionInst extends VMInst
case object BeginningPositionMatchInst extends VMInst
case object EndPositionMatchInst extends VMInst
case object FailInst extends VMInst
case object FailIfFalseInst extends VMInst
case object DropInst extends VMInst
case object SaveFlagsInst extends VMInst
case object RestoreFlagsInst extends VMInst
case class SetFlagsInst(setmask: Int, clearmask: Int) extends VMInst
case class NativeInst(function: VM => Unit) extends VMInst

case class PushInst(a: Any) extends VMInst
case object PushNoneInst extends VMInst
case object SomeInst extends VMInst
case class PushFunctionInst(compute: VM => Any) extends VMInst
case class PushFunctionReferenceInst(var entry: Int, name: String, arity: Int, fidx: Int) extends VMInst
case class FrameInst(locals: Int) extends VMInst
case class LocalInst(fidx: Int, idx: Int, name: String) extends VMInst
case class GlobalInst(idx: Int, name: String) extends VMInst
case class SetLocalInst(idx: Int, VariableAssignable: Boolean) extends VMInst
case class SetLocalsInst(idx: List[Int]) extends VMInst
case class SetGlobalInst(idx: Int, VariableAssignable: Boolean) extends VMInst
case class SetGlobalsInst(idx: List[Int]) extends VMInst
case class GlobalsInst(globalc: Int) extends VMInst
case class AssignmentInst(len: Int, lpos: Vector[Position], op: Symbol, rpos: Vector[Position]) extends VMInst
case object DerefInst extends VMInst
case class ConcatenateInst(parts: Int) extends VMInst
case class TupleInst(arity: Int) extends VMInst
case class ListInst(len: Int) extends VMInst
case class TupleElementInst(n: Int) extends VMInst
case object DupInst extends VMInst
case object DupUnderInst extends VMInst
case object OverInst extends VMInst
case object SwapInst extends VMInst
case object BindingsInst extends VMInst
case object BindingInst extends VMInst
case class MatchBindingInst(idx: Int) extends VMInst
case class TypeCheckInst(struc: StructureAST, pos: Position) extends VMInst
case object ListHeadInst extends VMInst
case object ListTailInst extends VMInst
case object EqInst extends VMInst
case object EmptyInst extends VMInst
case class ErrorInst(pos: Position, error: String) extends VMInst
case class CallingErrorInst(error: String) extends VMInst
case object SetInst extends VMInst
case object MapInst extends VMInst
case class BinaryInst(lpos: Position, op: Symbol, rpos: Position) extends VMInst
case class UnaryInst(op: Symbol, pos: Position) extends VMInst
//case object PopRetInst extends VMInst
case object FunctionReturnInst extends VMInst
case class MarkInst(disp: Int) extends VMInst
case object UnmarkInst extends VMInst
case object MarkStateInst extends VMInst
case object UnmarkSameDataInst extends VMInst
case class ChangeMarkInst(disp: Int) extends VMInst
case object GeneratorInst extends VMInst
//case class IteratorInst( pos: Position ) extends VMInst
//case object IterateInst extends VMInst
case class RangeInst(fpos: Position, tpos: Position, bpos: Position, inclusive: Boolean) extends VMInst
case class UnboundedLazyListInst(fpos: Position, bpos: Position) extends VMInst
case class CommentInst(comment: String) extends VMInst
case class DotOperatorInst(epos: Position, apos: Position, field: Symbol) extends VMInst
case object HaltInst extends VMInst
case class BeginScanInst(spos: Position) extends VMInst
case object EndScanInst extends VMInst
case class DataBacktractInst(pos: Position) extends VMInst
case object VectorInst extends VMInst
case object ToListInst extends VMInst
case object ToSetInst extends VMInst
case class BracketInst(epos: Position, apos: Position) extends VMInst
case object PushMatchedInst extends VMInst
case object PushCaptureGroupsInst extends VMInst
case object PushCaptureGroupsStringsInst extends VMInst
