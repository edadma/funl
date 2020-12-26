package xyz.hyperreal.funl

import java.io.{DataInputStream, FileInputStream, PrintStream}
import java.time.{LocalDate, LocalDateTime, LocalTime, ZonedDateTime}
import scala.util.parsing.input.Position
import scala.collection.immutable
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.math.BigInt
import scala.util.Random.{nextDouble, nextInt, setSeed}
import xyz.hyperreal.numbers_jvm.ComplexBigInt
import xyz.hyperreal.bvm._
import xyz.hyperreal.dal.BasicDAL

object Predef {

  def find(vm: VM, q: String, s: String, f: Int): Any = {
    s.indexOf(q, f) match {
      case -1 => Fail
      case idx =>
        def nextchoice(from: Int): Unit = {
          s.indexOf(q, from + 1) match {
            case -1 =>
            case nextidx =>
              vm.pushChoice(0, vm => {
                nextchoice(nextidx)
                vm.push(nextidx + 1)
              })
          }
        }

        nextchoice(idx)
        idx + 1
    }
  }

  def native(name: String,
             partialFunction: PartialFunction[(VM, Any), Any]): (String, (VM, Position, List[Position], Any) => Any) =
    (name, (vm: VM, pos: Position, ps: List[Position], args: Any) => {
      val arglist = argsderef(args)
      val fargs = (vm, arglist)

      if (partialFunction.isDefinedAt(fargs))
        partialFunction(fargs)
      else
        problem(pos, s"$name(): invalid arguments: $arglist")
    })

  val constants =
    Map(
      native("toInt", { case (vm: VM, v: String)    => v.toInt }),
      native("toLong", { case (vm: VM, v: String)   => v.toLong }),
      native("toBigInt", { case (vm: VM, v: String) => BigInt(v) }),
      native("toFloat", { case (vm: VM, v: String)  => v.toDouble }),
      native("eval", { case (vm: VM, expr: String)  => xyz.hyperreal.funl.run(expr) }),
      native("sqrt", { case (vm: VM, n: Number)     => BasicDAL.sqrtFunction(n) }),
      native("abs", { case (vm: VM, n: Number)      => BasicDAL.absFunction(n) }),
      native("exp", { case (vm: VM, n: Number)      => BasicDAL.expFunction(n) }),
      native("ln", { case (vm: VM, n: Number)       => BasicDAL.lnFunction(n) }),
      native("sin", { case (vm: VM, n: Number)      => BasicDAL.sinFunction(n) }),
      native("cos", { case (vm: VM, n: Number)      => BasicDAL.cosFunction(n) }),
      native("tan", { case (vm: VM, n: Number)      => BasicDAL.tanFunction(n) }),
      "None" -> None,
      "alphanum" -> ALPHANUM_CLASS,
      "digits" -> DIGIT_CLASS,
      "letters" -> LETTER_CLASS,
      "lcase" -> new CSet('a' to 'z'),
      "ucase" -> new CSet('A' to 'Z'),
      "i" -> ComplexBigInt.i,
      "pi" -> math.Pi,
      "e" -> math.E,
      "print" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        val list =
          args match {
            case a: ArgList => a.array toList
            case a          => List(a)
          }

        print(list map (a => display(deref(a))) mkString ", ")
      },
      "write" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        val list =
          args match {
            case a: ArgList => a.array toList
            case a          => List(a)
          }

        println(list map (a => display(deref(a))) mkString ", ")
      },
      "error" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case msg: String => problem(apos, msg)
          case _           => problem(apos, "error: expected one string argument")
        }
      },
      "array" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList() => mutable.ArraySeq()
          case n: Double if n.isValidInt =>
            mutable.ArraySeq.fill[Any](n.toInt)(VMUndefined)
          case n: Double =>
            problem(apos, s"array: given size is not integral: $n")
          case n: Int => mutable.ArraySeq.fill[Any](n)(VMUndefined)
          case ArgList(n1: Int, n2: Int) =>
            mutable.ArraySeq.fill[Any](n1, n2)(VMUndefined)
          case ArgList(n: Int, f: Function[_, _]) =>
            mutable.ArraySeq.tabulate[Any](n)(f.asInstanceOf[Int => Any])
          case ArgList(n1: Int, n2: Int, f: Function2[_, _, _]) =>
            mutable.ArraySeq
              .tabulate[Any](n1, n2)(f.asInstanceOf[(Int, Int) => Any])
          case init: Array[Any]  => mutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: Array[Byte] => mutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: Array[Int]  => mutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: Seq[_] if init.nonEmpty && init.head.isInstanceOf[Seq[Any]] =>
            mutable.ArraySeq[Any](init.asInstanceOf[Seq[Seq[Any]]] map (e => mutable.ArraySeq[Any](e: _*)): _*)
          case init: Seq[Any] => mutable.ArraySeq[Any](init: _*)
          case init: IterableOnce[Any] =>
            mutable.ArraySeq[Any](init.iterator.to(Seq): _*)
        }
      },
      "buffer" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList() => new VMBuffer
          case n: Int =>
            val res = new VMBuffer

            for (_ <- 1 to n) res.append(null)

            res
          case init: VMObject if init.isIterable => VMBufferClass.build(init.iterator)
        }
      },
      "seq" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList()  => immutable.ArraySeq()
          case a: ArgList => immutable.ArraySeq(a.array: _*)
          case init: Array[Any] =>
            immutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: Array[Byte] =>
            immutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: Array[Int] =>
            immutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: Seq[_] if init.nonEmpty && init.head.isInstanceOf[Seq[Any]] =>
            immutable.ArraySeq[Any](init map (e => immutable.ArraySeq[Any](e.asInstanceOf[Seq[Any]]: _*)): _*)
          case init: Seq[Any] => immutable.ArraySeq[Any](init.toIndexedSeq: _*)
          case init: IterableOnce[Any] =>
            immutable.ArraySeq[Any](init.iterator.to(Seq): _*)
          case _ => immutable.ArraySeq(args)
        }
      },
      "set" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList()            => new mutable.HashSet[Any]
          case a: ArgList           => mutable.HashSet(a.array: _*)
          case init: Array[Any]     => mutable.HashSet[Any](init.toIndexedSeq: _*)
          case init: Array[Byte]    => mutable.HashSet[Any](init.toIndexedSeq: _*)
          case init: Array[Int]     => mutable.HashSet[Any](init.toIndexedSeq: _*)
          case x: Seq[Any]          => mutable.HashSet(x: _*)
          case x: IterableOnce[Any] => mutable.HashSet(x.iterator.to(Seq): _*)
          case _                    => mutable.HashSet(args)
        }
      },
      "tuple" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList()                   => new mutable.HashSet[Any] //todo: shouldn't this be () (Unit)?
          case a: VMObject if a.isIterable => new Tuple(immutable.ArraySeq.from(a.iterator))
        }
      },
      "table" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList()               => new mutable.LinkedHashMap[Any, Any]
          case m: collection.Map[_, _] => mutable.LinkedHashMap(m.toSeq: _*)
          case s: Iterable[_] if s.head.isInstanceOf[Tuple] =>
            mutable.LinkedHashMap(s.asInstanceOf[Iterable[Tuple]].toSeq map (v => (v(0), v(1))): _*)
        }
      },
      "move" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case n: Int =>
            val pos = vm.scanpos + n

            if (pos < 0 || pos > vm.seq.length)
              Fail
            else {
              val res =
                if (vm.scanpos > pos)
                  vm.seq.subSequence(pos, vm.scanpos)
                else
                  vm.seq.subSequence(vm.scanpos, pos)
              val (oldseq, oldscanpos) = (vm.seq, vm.scanpos)

              vm.pushChoice(vm => {
                vm.seq = oldseq
                vm.scanpos = oldscanpos
              })
              vm.scanpos = pos
              res
            }
        }
      },
      "tab" -> ((vm: VM,
                 apos: Position,
                 ps: List[Position],
                 args: Any) => vm.tabToPosition(deref(args).asInstanceOf[Int])),
      "pos" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case n: Int =>
            if (n < -vm.seq.length || n > vm.seq.length + 1)
              Fail
            else {
              val pos =
                if (n > 0)
                  n - 1
                else
                  vm.seq.length - n

              if (vm.scanpos != pos)
                Fail
              else
                ()
            }
        }
      },
      "find" -> ((vm: VM,
                  apos: Position,
                  ps: List[Position],
                  args: Any) =>
                   argsderef(args) match {
                     case str: String                        => find(vm, str, vm.seq.toString, vm.scanpos)
                     case ArgList(str: String, subj: String) => find(vm, str, subj, 0)
                   }),
      "cset" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        val list =
          argsderef(args) match {
            case a: ArgList => a.array toList
            case a          => List(a)
          }

        new CSet(list: _*)
      },
      "upto" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        val (q, s, f) =
          argsderef(args) match {
            case ArgList(s: Any, subj: String) => (s, subj, 0)
            case s: Any                        => (s, vm.seq.toString, vm.scanpos)
          }
        val set =
          q match {
            case c: CSet   => c
            case s: String => new CSet(s)
          }

        s.indexWhere(set, f) match {
          case -1 => Fail
          case idx =>
            def nextchoice(from: Int): Unit = {
              s.indexWhere(set, from + 1) match {
                case -1 =>
                case nextidx =>
                  vm.pushChoice(0, vm => {
                    nextchoice(nextidx)
                    vm.push(nextidx + 1)
                  })
              }
            }

            nextchoice(idx)
            idx + 1
        }
      },
      "many" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        val (q, s, f) =
          argsderef(args) match {
            case ArgList(s: Any, subj: String) => (s, subj, 0)
            case s: Any                        => (s, vm.seq.toString, vm.scanpos)
          }
        val set =
          (q match {
            case c: CSet   => c
            case s: String => new CSet(s)
          }).complement

        s.indexWhere(set, f) match {
          case -1 =>
            if (f == s.length)
              Fail
            else
              s.length + 1
          case idx =>
            def nextchoice(from: Int): Unit = {
              s.indexWhere(set, from + 1) match {
                case -1 =>
                  if (vm.scanpos != vm.seq.length)
                    vm.pushChoice(0, _.push(vm.seq.length + 1))
                case nextidx =>
                  vm.pushChoice(0, vm => {
                    nextchoice(nextidx)
                    vm.push(nextidx + 1)
                  })
              }
            }

            nextchoice(idx)
            idx + 1
        }
      },
      "match" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        vm.matchString(argsderef(args).toString)
      },
      "any" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        val set =
          argsderef(args) match {
            case c: CSet   => c
            case s: String => new CSet(s)
          }

        if (vm.scanpos == vm.seq.length || !set(vm.seq.charAt(vm.scanpos)))
          Fail
        else
          vm.scanpos + 2
      },
      "type" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case _: String          => "string"
          case _: Double          => "float"
          case _: Int | _: BigInt => "integer"
          case _: Iterable[_]     => "iterable"
          case o                  => o.getClass.toString
        }
      },
      "swap" -> { (_: VM, apos: Position, ps: List[Position], args: Any) =>
        args match {
          case ArgList(a: Assignable, b: Assignable) =>
            val temp = a.value

            a.value = b.value
            b.value = temp
          case _ => problem(ps.headOption.getOrElse(apos), "swap: v1, v2")
        }
      },
      "center" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        val (src, width, padding) =
          argsderef(args) match {
            case ArgList() =>
              problem(ps.headOption.getOrElse(apos), "center( src, width, padding )")
            case s: String                   => (s, 1, " ")
            case ArgList(s1: String, i: Int) => (s1, i, " ")
            case ArgList(s1: String, i: Int, s2: String) =>
              (s1, i, s2.headOption.getOrElse(' ').toString)
          }

        if (src.length > width)
          src.substring(0, width)
        else {
          val diff = width - src.length
          val half = diff / 2
          val odd = diff % 2 == 1

          padding * half + src + padding * half + (if (odd) padding else "")
        }
      },
      "right" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        val (src, width, padding) =
          argsderef(args) match {
            case ArgList() =>
              problem(ps.headOption.getOrElse(apos), "right( src, width, padding )")
            case s: String                   => (s, 1, " ")
            case ArgList(s1: String, i: Int) => (s1, i, " ")
            case ArgList(s1: String, i: Int, s2: String) =>
              (s1, i, s2.headOption.getOrElse(' ').toString)
          }

        val diff = width - src.length

        if (src.length > width)
          src.substring(-diff, src.length)
        else {
          padding * diff + src
        }
      },
      "float" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case n: Number => n.doubleValue
          case s: String => s.toDouble
          case _ =>
            problem(ps.headOption.getOrElse(apos), s"float: expected a number")
        }
      },
      "integer" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case n: Double => n.toInt // todo: correction integer conversions
          case s: String =>
            val a = BigInt(s)

            if (a.isValidInt) a.toInt
            else a
          case _ =>
            problem(ps.headOption.getOrElse(apos), s"number: expected a number")
        }
      },
      "hex" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case n: BigInt => n.toString(16)
          case n: Int    => Integer.toHexString(n)
          case _ =>
            problem(ps.headOption.getOrElse(apos), s"hex: expected an integer")
        }
      },
      "chr" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case code: Int => code.toChar.toString
          case x =>
            problem(ps.headOption.getOrElse(apos), s"chr: expected a (small) integer")
        }
      },
      "ord" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case ch: String => ch.head.toInt
          case x =>
            problem(ps.headOption.getOrElse(apos), s"ord: expected a non-empty string")
        }
      },
      "min" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList() =>
            problem(ps.headOption.getOrElse(apos), "min( a1, a2, ... )")
          case l: Iterable[_] => l.min(ORDERING)
          case args: ArgList  => args.array.min(ORDERING)
          case _              => problem(ps.headOption.getOrElse(apos), "min( a1, a2, ... )")
        }
      },
      "max" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList() =>
            problem(ps.headOption.getOrElse(apos), "max( a1, a2, ... )")
          case l: Iterable[_] => l.max(ORDERING)
          case args: ArgList  => args.array.max(ORDERING)
          case _              => problem(ps.headOption.getOrElse(apos), "max( a1, a2, ... )")
        }
      },
//      "sum" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
//        argsderef(args) match {
//          case ArgList() =>
//            problem(ps.headOption.getOrElse(apos), "sum( a1, a2, ... )")
//          case l: Iterable[_] => l.asInstanceOf[Iterable[Number]].iterator.sum(NUMERIC)
//          case args: ArgList  => args.array.asInstanceOf[Iterable[Number]].sum(NUMERIC)
//          case _              => problem(ps.headOption.getOrElse(apos), "sum( a1, a2, ... )")
//        }
//      },
      "rnd" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case ArgList()                         => nextDouble().asInstanceOf[Number]
          case ArgList(l: Int, u: Int) if l <= u => nextInt(u - l) + l
          case n: Int                            => nextInt(n)
          case r: collection.immutable.Range =>
            nextInt(r.last + 1 - r.start) + r.start
        }
      },
      "seed" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        argsderef(args) match {
          case s: Number => setSeed(s.longValue)
        }
      },
      "gc" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        System.gc()
        System.gc()
        System.gc()
      },
      "sleep" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) =>
        deref(args) match {
          case millis: Number => Thread.sleep(millis.longValue)
          case x =>
            problem(ps.headOption.getOrElse(apos), s"sleep: expected a non-negative integer (milliseconds)")
        }
      }

//				"fail" -> { (vm: VM, apos: Position, ps: List[Position], args: Any) => Fail }

//			case (_: Double) | (_: BigDecimal) => a
//			case n: Number => n.doubleValue
//			case s: String => s.toDouble
    )

  private lazy val devrandom = new DataInputStream(new FileInputStream("/dev/random"))
  private val p32 = 0x100000000L.asInstanceOf[Double]

  private def rnd = devrandom.readInt.asInstanceOf[Long] & 0xFFFFFFFFL

  val sysvars: Map[String, VM => Any] =
    Map(
      "subject" -> (vm => vm.seq.toString),
      "pos" -> (vm => vm.scanpos + 1),
      "args" -> ((vm: VM) => vm.args),
      "version" -> ((_: VM) => "0.4.4"),
      "timemillis" -> ((_: VM) => BigInt(System.currentTimeMillis)),
      "time" -> ((_: VM) => LocalTime.now),
      "date" -> ((_: VM) => LocalDate.now),
      "datetime" -> ((_: VM) => LocalDateTime.now),
      "zoneddatetime" -> ((_: VM) => ZonedDateTime.now),
      "stdin" -> ((_: VM) => Console.in),
//      "stdout" -> ((_: VM) =>//todo: system variables
//        new Assignable {
//          val value: PrintStream = Console.out
//
//          def value_=(v: Any): Unit = {
//            v match {
//              case a: Array[Byte] => Console.out.write(a)
//              case o: AnyRef      => Console.out.println(o)
//            }
//          }
//        }),
      "stderr" -> ((_: VM) => Console.err),
      "user" -> (
          (_: VM) =>
            Map("dir" -> System.getProperty("user.dir"),
                "home" -> System.getProperty("user.home"),
                "name" -> System.getProperty("user.name"))),
      "os" -> (
          (_: VM) =>
            Map("arch" -> System.getProperty("os.arch"),
                "version" -> System.getProperty("os.version"),
                "name" -> System.getProperty("os.name"))),
      "java" -> ((_: VM) =>
        Map(
          "class" -> Map("path" -> System.getProperty("java.class.path")),
          "vendor" -> System.getProperty("java.vendor"),
          "version" -> System.getProperty("java.version"),
          "home" -> System.getProperty("java.home")
        )),
      "vmscaninfo" -> ((vm: VM) =>
        new Tuple(immutable.ArraySeq(new VMString(vm.seq.toString), VMNumber(vm.scanpos + 1)))),
      "vmstacksize" -> ((vm: VM) => vm.getdata.size),
      "rndi" -> ((_: VM) => BigInt(rnd)),
      "rnd" -> ((_: VM) => rnd / p32)
    )

  val macros: Map[String, List[AST] => AST] =
    Map[String, List[AST] => AST](
      "string" -> {
        case List(a: ExpressionAST) => PatternExpressionAST(StringPattern(a))
        case _ =>
          problem(null, "string() macro can only be applied to an expression")
      },
      "rep1" -> {
        case List(PatternExpressionAST(pat)) =>
          PatternExpressionAST(OneOrMorePattern(pat))
        case _ =>
          problem(null, "rep1() macro can only be applied to a pattern expression")
      },
      "rep" -> {
        case List(PatternExpressionAST(pat)) =>
          PatternExpressionAST(ZeroOrMorePattern(pat))
        case _ =>
          problem(null, "rep() macro can only be applied to a pattern expression")
      },
      "opt" -> {
        case List(PatternExpressionAST(pat)) =>
          PatternExpressionAST(OptionalPattern(pat))
        case _ =>
          problem(null, "opt() macro can only be applied to a pattern expression")
      },
      "rrep1" -> {
        case List(PatternExpressionAST(pat)) =>
          PatternExpressionAST(ReluctantOneOrMorePattern(pat))
        case _ =>
          problem(null, "rrep1() macro can only be applied to a pattern expression")
      },
      "rrep" -> {
        case List(PatternExpressionAST(pat)) =>
          PatternExpressionAST(ReluctantZeroOrMorePattern(pat))
        case _ =>
          problem(null, "rrep1() macro can only be applied to a pattern expression")
      },
      "ropt" -> {
        case List(PatternExpressionAST(pat)) =>
          PatternExpressionAST(ReluctantOptionalPattern(pat))
        case _ =>
          problem(null, "ropt() macro can only be applied to a pattern expression")
      },
      "dot" -> {
        case List() => PatternExpressionAST(DotPattern)
        case _ =>
          problem(null, "dot() macro can only be applied to a pattern expression")
      },
      "cat" -> {
        case ps: List[_] if ps.forall(_.isInstanceOf[PatternExpressionAST]) =>
          PatternExpressionAST(ConcatenationPattern(ps.asInstanceOf[List[PatternExpressionAST]] map (_.pat)))
        case _ =>
          problem(null, "cat() macro can only be applied to pattern expressions")
      },
      "capture" -> {
        case List(LiteralExpressionAST(name), PatternExpressionAST(pat)) if name.isInstanceOf[String] =>
          PatternExpressionAST(CapturePattern(name.asInstanceOf[String], pat, null))
        case _ =>
          problem(null, "capture() macro can only be applied to pattern expressions")
      },
    )
}
