//@
package xyz.hyperreal.bvm

import scala.collection.immutable.{ArraySeq, TreeMap}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.parsing.input.Position

class Compiler(constants: Map[String, Any],
               sysvars: Map[String, VM => Any],
               macros: Map[String, List[AST] => AST],
               dir: Mode = Forward,
               comments: Boolean = false) {
  sealed trait Decl
  case class VarDecl(idx: Int, declared: Boolean, var set: Boolean = false) extends Decl
  case class RecordDecl(const: RecordConstructor) extends Decl
  case class TypeDecl(name: String) extends Decl
  case class FunctionDecl(first: Position,
                          var last: Position,
                          var mark: Int,
                          arity: Int,
                          fixups: ArrayBuffer[Int],
                          var entry: Int,
                          parts: mutable.LinkedHashMap[Position, FunctionPart])
      extends Decl

  case class FunctionPart(var entry: Int, localdecls: Namespace)

  class Scope(val scopenum: Int) {
    val symbols = new mutable.HashMap[String, Decl]

    override def toString = s"<$scopenum, $symbols>"
  }

  class Namespace {
    var scopenum = 0
    var scopes: List[Scope] = Nil

    def enterScope(): Unit = {
      scopenum += 1
      scopes = new Scope(scopenum) :: scopes
    }

    def exitScope(): Unit = {
      val cur = scopes.head

      for ((k, v) <- cur.symbols if !k.head.isDigit) {
        cur.symbols(s"${cur.scopenum}$k") = v
        cur.symbols -= k
      }

      if (scopes.tail nonEmpty) {
        scopes.tail.head.symbols ++= cur.symbols
        scopes = scopes.tail
      }
    }

    def getscope(n: String): Option[Scope] = scopes find (_.symbols contains n)

    def get(n: String): Option[Decl] = getscope(n) map (_.symbols(n))

    def contains(n: String): Boolean = get(n) isDefined

    def put(n: String, d: Decl): Unit = scopes.head.symbols(n) = d

    def countvars: Int = scopes map (_.symbols.values.count(_.isInstanceOf[VarDecl])) sum

    def duplicate(name: String): Boolean = scopes.head.symbols contains name

    def getinner(name: String): Option[Decl] = scopes.head.symbols get name

    override def toString: String = scopes.toString
  }

  private var globals: Namespace = _
  private var code
    : ArrayBuffer[VMInst] = _ //todo: this could be moved to Compilation object which could then be passed to emitExtension() along with AST

  private def getdeclarations(namespaces: List[Namespace]) =
    if (namespaces nonEmpty)
      namespaces.head
    else
      globals

  private def getvariableall(name: String, namespaces: List[Namespace]) = getdeclarations(namespaces) get name

  //private def variableall(name: String, namespaces: List[Namespace]) = getvariableall(name, namespaces) get

  private def getvariable(name: String, namespaces: List[Namespace]) = getdeclarations(namespaces) getinner name

  private def variable(name: String, namespaces: List[Namespace]) = getvariable(name, namespaces) get

  private def getscope(name: String, namespaces: List[Namespace]) = getdeclarations(namespaces).getscope(name) get

  sealed trait VarResult
  case class ConstantVarResult(c: Any) extends VarResult
  case class DeclVarResult(d: Decl, space: Symbol, idx: Int) extends VarResult
  case object NoneVarResult extends VarResult

//  private def show(namespaces: List[Namespace]): Unit = {
//    for ((n, i) <- namespaces zipWithIndex) {
//      println(s"namespace $i")
//
//      for (s <- n.scopes) {
//        println(s"  scope ${s.scopenum}")
//
//        for ((k, v) <- s.symbols)
//          println(s"    $k -> $v")
//      }
//    }
//  }

  private def findvariable(name: String, namespaces: List[Namespace]): VarResult = {

//		show( namespaces )

    def context: Option[(Decl, Int)] =
      (namespaces zipWithIndex) find { case (n, _) => n contains name } map { case (n, i) => (n get name get, i) }

    context match {
      case None =>
        globals get name match {
          case None =>
            constants get name match {
              case None    => NoneVarResult
              case Some(c) => ConstantVarResult(c)
            }
          case Some(d) => DeclVarResult(d, Symbol("global"), 0)
        }
      case Some((d, i)) => DeclVarResult(d, Symbol("local"), i)
    }
  }

  private def findscope(name: String, namespaces: List[Namespace]): Option[Scope] =
    if (namespaces nonEmpty)
      namespaces find (_ contains name) map (_ getscope name) getOrElse (globals getscope name)
    else
      globals getscope name

  protected def sourceExplicitsExtension(explicits: AST => Unit): Unit = {}

  protected def sourceDeclsExtension(decls: AST => Unit): Unit = {}

  protected def sourceEmitExtension(emit: AST => Unit): Unit = {}

  protected def explicitsExtension: PartialFunction[(AST, AST => Unit), Any] = {
    case null => ()
  }

  protected def declsExtension: PartialFunction[(AST, AST => Unit), Any] = {
    case null => ()
  }

  private def decls(ast: AST, namespaces: List[Namespace], structvars: mutable.HashSet[String] = null): Unit = {
    def declarations = if (namespaces nonEmpty) namespaces.head else globals

    def varnum = declarations.countvars

    def duplicate(name: String) = declarations.duplicate(name)

    def declare(name: String, decl: Decl): Unit = declarations.put(name, decl)

    def explicits(ast: AST): Unit =
      ast match {
        case DefAST(oname, FunctionExpressionAST(name, pos, parms, _, parts, where)) => //todo: handle ... at end of parm list
          val arity = parms.length
          val part = FunctionPart(-1, new Namespace)

          getvariable(name, namespaces) match {
            case Some(f @ FunctionDecl(_, _, _, a, _, _, fparts)) =>
              if (a != arity)
                problem(pos,
                        s"function part definition with different arity than other parts: found $arity, expected $a")

              fparts(pos) = part
              f.last = pos
            case Some(_) => problem(pos, s"duplicate symbol: $oname")
            case None =>
              declare(name, FunctionDecl(pos, pos, -1, arity, new ArrayBuffer, -1, mutable.LinkedHashMap(pos -> part)))
          }
        case ValAST(struc, _, expr) =>
          decls(struc, namespaces, structvars = new mutable.HashSet[String])
          _decls(expr)
        case VarAST(pos, name, oname, _) =>
          if (duplicate(name))
            problem(pos, s"duplicate symbol '$oname'")

          declare(name, VarDecl(varnum, declared = true))
        case DataAST(pos, tname, constructors) =>
          if (duplicate(tname))
            problem(pos, s"duplicate symbol '$tname'")

          constructors match {
            case List((n, _)) if n == tname =>
            case _                          => declare(tname, TypeDecl(tname))
          }

          for ((cname, fields) <- constructors)
            if (duplicate(cname))
              problem(pos, s"duplicate symbol '$cname'")
            else
              declare(cname, RecordDecl(new RecordConstructor(tname, cname, fields)))
        case DeclarationBlockAST(s)                             => s foreach explicits
        case t if explicitsExtension isDefinedAt (t, explicits) => explicitsExtension(t, explicits)
        case _                                                  =>
      }

    def _decls(ast: AST): Unit =
      ast match {
        case SourceAST(s) =>
          globals.enterScope()
          sourceExplicitsExtension(explicits)
          s foreach explicits
          sourceDeclsExtension(_decls)
          s foreach _decls
          globals.exitScope()
        case DeclarationBlockAST(s) => s foreach _decls
        case DefAST(_, f @ FunctionExpressionAST(name, pos, parms, _, parts, where)) =>
          val FunctionDecl(first, last, mark, _, fixups, _, fparts) = variable(name, namespaces)
          val FunctionPart(_, localdecls) = fparts(pos)

          val dvars = new mutable.HashSet[String]

          localdecls.enterScope()

          for (p <- parms.reverse)
            decls(p, localdecls :: namespaces, dvars)

          decls(where, localdecls :: namespaces)

          for (FunctionPartExpressionAST(guard, body) <- parts) {
            if (guard nonEmpty)
              decls(guard get, localdecls :: namespaces)

            decls(body, localdecls :: namespaces)
          }

          localdecls.exitScope()

          val newname = s"${getscope(name, namespaces).scopenum}$name"

          f.name = newname
        case WhereClauseAST(where) =>
          where foreach explicits
          where foreach _decls
        case v @ VarAST(_, name, _, init) =>
          init foreach _decls

          val newname = s"${getscope(name, namespaces).scopenum}$name"

          v.name = newname
        case _: DataAST                                                                 =>
        case VariableStructureAST(_, "_", _) | LiteralStructureAST(_) | NilStructureAST =>
        case v @ VariableStructureAST(pos, name, oname) =>
          getvariable(name, namespaces) match {
            case Some(RecordDecl(c)) =>
              if (c.arity > 0)
                problem(pos, s"this is a record name: $oname")
            case Some(_) if (structvars ne null) && (structvars contains name) =>
            case Some(_)                                                       => problem(pos, s"duplicate symbol '$oname'")
            case None =>
              declare(name, VarDecl(varnum, declared = true))

              if (structvars ne null)
                structvars += name
          }

          val newname = s"${getdeclarations(namespaces).getscope(name).get.scopenum}$name"

          v.name = newname
        case TupleStructureAST(_, l) => l foreach _decls
        case ListStructureAST(_, l)  => l foreach _decls
        case ConsStructureAST(_, head, tail) =>
          _decls(head)
          _decls(tail)
        case RecordStructureAST(_, _, args) => args foreach _decls
        case AlternationStructureAST(l) =>
          def novars(struc: StructureAST): Unit =
            struc match {
              case VariableStructureAST(_, "_", _) | LiteralStructureAST(_) | NilStructureAST =>
              case VariableStructureAST(pos, name, _) =>
                getvariable(name, namespaces) match {
                  case Some(RecordDecl(c)) if c.arity == 0 =>
                  case _                                   => problem(pos, "illegal variable in pattern alternative")
                }
              case TupleStructureAST(_, args) => args foreach novars
              case ListStructureAST(_, args)  => args foreach novars
              case ConsStructureAST(_, head, tail) =>
                novars(head)
                novars(tail)
              case RecordStructureAST(_, _, args) => args foreach novars
              case AlternationStructureAST(args)  => args foreach novars
              case NamedStructureAST(pos, _, _)   => problem(pos, "illegal variable in pattern alternative")
            }

          l foreach novars
          l foreach _decls
        case n @ NamedStructureAST(pos, alias, s) =>
          getvariable(alias, namespaces) match {
            case Some(_) => problem(pos, s"duplicate symbol '$alias'")
            case None =>
              declare(alias, VarDecl(varnum, declared = true))
              _decls(s)
          }

          val newname = s"${getdeclarations(namespaces).getscope(alias).get.scopenum}$alias"

          n.alias = newname
        case BlockExpressionAST(s) =>
          declarations.enterScope()
          s foreach explicits
          s foreach _decls
          declarations.exitScope()
        case v @ VariableExpressionAST(pos, name, _) =>
          findvariable(name, namespaces) match {
            case NoneVarResult =>
              declare(name, VarDecl(varnum, declared = false))

              findscope(name, namespaces) match {
                case Some(s) =>
                  v.name = s"${s.scopenum}$name"
                case None => problem(pos, "*** BUG *** not found in any scope")
              }
            case ConstantVarResult(_) =>
            case DeclVarResult(_, _, _) =>
              findscope(name, namespaces) match {
                case Some(s) => v.name = s"${s.scopenum}$name"
                case None    =>
              }
          }
        case GenerateExpressionAST(_, collection) => _decls(collection)
        case BracketExpressionAST(_, f, _, arg) =>
          _decls(f)
          _decls(arg)
        case v @ SetValueExpressionAST(pos, name, oname, expr) =>
          _decls(expr)

          findvariable(name, namespaces) match {
            case NoneVarResult        => problem(pos, s"val not found: $oname")
            case ConstantVarResult(_) => problem(pos, s"constant not val: $oname")
            case DeclVarResult(_, _, _) =>
              findscope(name, namespaces) match {
                case Some(s) => v.name = s"${s.scopenum}$name"
                case None    => problem(pos, s"*** BUG ***: $oname")
              }
          }
        case AssignmentExpressionAST(lhs, _, rhs) =>
          lhs foreach { case (_, e) => _decls(e) }
          rhs foreach { case (_, e) => _decls(e) }
        case ScanAssignmentExpressionAST(_, lhs, rhs) =>
          _decls(lhs)
          _decls(rhs)
        case ReversableAssignmentExpressionAST(_, lhs, rhs) =>
          _decls(lhs)
          _decls(rhs)
        case EveryExpressionAST(_, gen, body, els) =>
          _decls(gen)
          body foreach _decls
          els foreach _decls
        case ForExpressionAST(_, gen, body, els) =>
          declarations.enterScope()
          gen foreach _decls
          _decls(body)
          els foreach _decls
          declarations.exitScope()
        case WhileExpressionAST(_, cond, body, els) =>
          _decls(cond)
          body foreach _decls
          els foreach _decls
        case BreakExpressionAST(_, _, expr) => expr foreach _decls
        case RepeatExpressionAST(_, body)   => _decls(body)
        case ListComprehensionExpressionAST(ComprehensionAST(expr, gen)) =>
          declarations.enterScope()
          gen foreach _decls
          _decls(expr)
          declarations.exitScope()
        case SetComprehensionExpressionAST(ComprehensionAST(expr, gen)) =>
          declarations.enterScope()
          gen foreach _decls
          _decls(expr)
          declarations.exitScope()
//				case IteratorExpressionAST( structure, _, traversable, filter ) =>
//					_decls( structure )
//					_decls( traversable )
//					filter foreach _decls
        case GeneratorExpressionAST(structure, _, traversable, filter) =>
          _decls(structure)
          _decls(traversable)
          filter foreach _decls
        case RangeExpressionAST(_, f, _, t, _, b, _) =>
          _decls(f)
          _decls(t)
          _decls(b)
        case DefinedExpressionAST(expr)       => _decls(expr)
        case UndefinedExpressionAST(expr)     => _decls(expr)
        case DefinedOptionExpressionAST(expr) => _decls(expr)
        case TupleExpressionAST(l)            => l foreach _decls
        case ListExpressionAST(l)             => l foreach _decls
        case SetExpressionAST(l)              => l foreach _decls
        case MapExpressionAST(l) =>
          for ((k, v) <- l) {
            _decls(k)
            _decls(v)
          }
        case UnboundedLazyListExpressionAST(_, f, _, b) =>
          _decls(f)
          _decls(b)
        case SequenceExpressionAST(_, f, _, t, _, b, _) =>
          _decls(f)
          _decls(t)
          _decls(b)
        case YieldExpressionAST(expr, _) =>
          _decls(expr)
        case RepeatedAlternationExpressionAST(expr) =>
          _decls(expr)
        case ApplyExpressionAST(_, f, _, args, _) =>
          _decls(f)
          args foreach { case (_, e) => _decls(e) }
        case UnaryExpressionAST(_, _, expr) => _decls(expr)
        case BinaryExpressionAST(_, left, _, _, right) =>
          _decls(left)
          _decls(right)
        case InterpolationExpressionAST(l)                                  => l foreach _decls
        case NotExpressionAST(expr)                                         => _decls(expr)
        case LeftSectionExpressionAST(_, _: LiteralExpressionAST, _, _, _)  =>
        case LeftSectionExpressionAST(_, _, lambda, _, _)                   => _decls(lambda)
        case RightSectionExpressionAST(_, _, _: LiteralExpressionAST, _, _) =>
        case RightSectionExpressionAST(_, _, _, lambda, _)                  => _decls(lambda)
        case DotExpressionAST(_, expr, _, _)                                => _decls(expr)
        case ScanExpressionAST(_, subject, expr) =>
          _decls(subject)
          _decls(expr)
        case EqExpressionAST(expr, _, _) => _decls(expr)
        case ConditionalExpressionAST(cond, els) =>
          cond foreach {
            case (a, b) =>
              _decls(a)
              _decls(b)
          }
          els foreach _decls
        case AndExpressionAST(l, r) =>
          _decls(l)
          _decls(r)
        case OrExpressionAST(l, r) =>
          _decls(l)
          _decls(r)
        case f @ FunctionExpressionAST(_, pos, _, _, _, _) =>
          val oname = "$" + pos.toString

          explicits(DefAST(oname, f))
          _decls(DefAST(oname, f))
        case t if declsExtension isDefinedAt (t, _decls)                          => declsExtension(t, _decls)
        case _: StatementAST | _: ExpressionAST | _: PatternAST | _: StructureAST =>
      }

    _decls(ast)
  }

  private def unify(struc: StructureAST,
                    pos: Position,
                    namespaces: List[Namespace],
                    vars: mutable.HashSet[String] = new mutable.HashSet): Unit = {
    def _unify(struc: StructureAST, pos: Position): Unit =
      struc match {
        case TupleStructureAST(_, args) =>
          code += TypeCheckInst(struc, pos)

          args.zipWithIndex foreach {
            case (e, i) =>
              if (i < args.length - 1)
                code += DupInst

              code += TupleElementInst(i)
              unify(e, pos, namespaces)
          }
        case NilStructureAST =>
          code += TypeCheckInst(struc, pos) // todo: maybe use EmptyInst instead and not emit a drop
          code += DropInst
        case ListStructureAST(_, l) =>
          code += TypeCheckInst(struc, pos)

          l foreach { e =>
            code += DupInst
            code += EmptyInst
            code += BranchIfNotInst(1)
            code += FailInst
            code += DupInst
            code += ListHeadInst
            unify(e, pos, namespaces)
            code += ListTailInst
          }

          code += EmptyInst
          code += BranchIfInst(1)
          code += FailInst
        case ConsStructureAST(_, head, tail) =>
          code += TypeCheckInst(struc, pos)
          code += DupInst
          code += ListHeadInst
          unify(head, pos, namespaces)
          code += ListTailInst
          unify(tail, pos, namespaces)
        case VariableStructureAST(_, "_", _) => code += DropInst
        case VariableStructureAST(p, n, _) if variable(n, namespaces).isInstanceOf[RecordDecl] =>
          code += TypeCheckInst(RecordStructureAST(p, n, Vector()), pos)
        case VariableStructureAST(_, n, _) =>
          val VarDecl(idx, _, _) = variable(n, namespaces)

          if (vars contains n)
            code += MatchBindingInst(idx)
          else {
            vars += n
            code += BindingInst
          }
        case NamedStructureAST(_, _, s) =>
          code += DupInst
          code += BindingInst
          unify(s, pos, namespaces)
        case RecordStructureAST(_, _, args) =>
          code += TypeCheckInst(struc, pos)

          args.zipWithIndex foreach {
            case (e, i) =>
              if (i < args.length - 1)
                code += DupInst

              code += TupleElementInst(i)
              unify(e, pos, namespaces)
          }
        case AlternationStructureAST(l) =>
          val jumps = new ArrayBuffer[Int]

          for (s <- l.init) {
            val backptr = code.length

            code += null
            unify(s, pos, namespaces)
            jumps += code.length
            code += null
            code(backptr) = ChoiceInst(code.length - backptr - 1)
          }

          unify(l.last, pos, namespaces)

          for (b <- jumps)
            code(b) = BranchInst(code.length - b - 1)
        case LiteralStructureAST(lit) =>
          code += PushInst(lit)
          code += EqInst
          code += BranchIfInst(1)
          code += FailInst
      }

    _unify(struc, pos)
  }

  private def indexes(struc: StructureAST, namespaces: List[Namespace]) = {
    def _indexes(struc: StructureAST): List[Int] =
      struc match {
        case TupleStructureAST(_, l)                                                    => l flatMap _indexes
        case ListStructureAST(_, l)                                                     => l flatMap _indexes
        case VariableStructureAST(_, "_", _) | LiteralStructureAST(_) | NilStructureAST => Nil
        case VariableStructureAST(pos, name, _) =>
          variable(name, namespaces) match {
            case d @ VarDecl(idx, _, _) =>
              d.set = true
              List(idx)
            case RecordDecl(_) => Nil
            case _             => problem(pos, "*** BUG ***")
          }
        case ConsStructureAST(_, head, tail) => List(_indexes(head), _indexes(tail)).flatten
        case RecordStructureAST(_, _, args)  => args.toList flatMap _indexes
        case NamedStructureAST(pos, alias, s) =>
          variable(alias, namespaces) match {
            case d @ VarDecl(idx, _, _) =>
              d.set = true
              idx :: _indexes(s)
            case _ => problem(pos, "*** BUG ***")
          }
        case AlternationStructureAST(l) => l flatMap _indexes
      }

    _indexes(struc)
  }

  private def removeDuplicates(l: List[Int]) = mutable.LinkedHashSet(l: _*).toList

  private def frameSameData(c: => Unit): Unit = {
    val ptr = code.length

    code += null
    c
    code += UnmarkSameDataInst
    code(ptr) = MarkInst(code.length - ptr - 1)
  }

  protected def emitExtension: PartialFunction[(AST, AST => Unit), Any] = {
    case null => ()
  }

  protected def emit(ast: AST, namespaces: List[Namespace]): Unit = {
    var markNesting = 0
    val loops = new ArrayBufferStack[Label]

    case class Label(construct: String,
                     name: String,
                     nesting: Int,
                     loop: Int,
                     breaks: ArrayBuffer[Int],
                     continues: ArrayBuffer[Int])

    def search(name: String) = loops find (_.name == name)

    def comment(s: String, pos: Position = null) =
      if (comments)
        code += CommentInst(if (pos eq null) s else s"$pos: $s")

    def _emit(ast: AST): Unit = {
//			println( ast )
      ast match {
        case SourceAST(l) =>
          code += GlobalsInst(globals.countvars)

          l foreach {
            case s: DeclarationStatementAST => _emit(s)
            case e: ExpressionAST =>
              val backptr = code.length

              code += null
              markNesting += 1
              _emit(e)
              markNesting -= 1
              code += UnmarkSameDataInst
              code(backptr) = MarkInst(code.length - backptr - 1)
          }

          sourceEmitExtension(_emit)
        case DeclarationBlockAST(l) => l foreach _emit
        case DefAST(oname, FunctionExpressionAST(name, pos, parms, _, parts, where)) =>
          val f @ FunctionDecl(first, last, mark, _, fixups, _, fparts) = variable(name, namespaces)
          val p @ FunctionPart(_, localdecls) = fparts(pos)

          code += null //todo: place functions after the end of top level code: segments

          val entry = code.length

          p.entry = entry

          if (pos == first) {
            f.entry = entry

            for (idx <- fixups)
              code(idx) match {
                case inst: PushFunctionReferenceInst => inst.entry = code.length
                case _                               => problem(pos, "error doing fixups")
              }
          } else
            code(mark) = MarkInst(entry - mark - 1)

          f.mark = entry

          if (last == pos) {
            code += MarkInst(1)
            code += BranchInst(1)
            code += CallingErrorInst(s"error calling function $oname: argument match not found")
          } else
            code += null

          comment(s"--- $oname()")
          //					code += PopRetInst
          code += BindingsInst

          val uvars = new mutable.HashSet[String]

          for (p <- parms.reverse)
            unify(p, null, localdecls :: namespaces, uvars)

          code += FrameInst(localdecls.countvars)
          code += SetLocalsInst(removeDuplicates(parms.reverse.flatMap(indexes(_, localdecls :: namespaces))))

          //					show( localdecls :: namespaces )
          emit(where, localdecls :: namespaces)

          for (p @ FunctionPartExpressionAST(guard, body) <- parts) {
            val guardloc = code.length

            if (guard.isDefined) {
              if (p != parts.last)
                code += null

              emit(guard.get, localdecls :: namespaces)

              if (p != parts.last)
                code += UnmarkInst
              else
                code += DropInst
            }

            code += UnmarkSameDataInst
            emit(body, localdecls :: namespaces)
            code += FunctionReturnInst
            code(entry - 1) = BranchInst(code.length - entry)

            if (guard.isDefined && p != parts.last)
              code(guardloc) = MarkInst(code.length - guardloc - 1)
          }
        case WhereClauseAST(where) => where foreach _emit
        case ValAST(s, pos, init) =>
          val backptr = code.length

          code += null
          _emit(init)
          code += BranchInst(1)
          code(backptr) = MarkInst(code.length - backptr - 1)
          code += ErrorInst(pos, "match error")
          code += BindingsInst
          unify(s, pos, namespaces)

          if (namespaces isEmpty)
            code += SetGlobalsInst(removeDuplicates(indexes(s, Nil)))
          else
            code += SetLocalsInst(removeDuplicates(indexes(s, namespaces)))

          code += UnmarkInst
        case VarAST(pos, name, _, init) =>
          variable(name, namespaces) match {
            case d @ VarDecl(idx, _, _) =>
              init match {
                case None       => code += PushInst(VMUndefined)
                case Some(expr) => _emit(expr)
              }

              if (namespaces isEmpty)
                code += SetGlobalInst(idx, VariableAssignable = true)
              else
                code += SetLocalInst(idx, VariableAssignable = true)

              d.set = true
            case _ => problem(pos, "*** BUG ***")
          }
        case DataAST(_, _, _) =>
        case VariableExpressionAST(pos, name, oname) =>
          findvariable(name, namespaces) match {
            //						case ContextVarResult => sys.error( "bug: should have been declared" )
            case DeclVarResult(VarDecl(_, true, false), _, fidx) =>
              problem(pos, s"illegal forward reference to symbol '$oname'")
            case DeclVarResult(d @ VarDecl(idx, declared, set), space, fidx) =>
              if (!declared && !set && fidx == 0) {
                code += PushInst(VMUndefined)

                if (space == Symbol("global"))
                  code += SetGlobalInst(idx, VariableAssignable = true)
                else
                  code += SetLocalInst(idx, VariableAssignable = true)

                d.set = true
              }

              if (space == Symbol("local"))
                code += LocalInst(fidx, idx, name)
              else
                code += GlobalInst(idx, name)
            case DeclVarResult(RecordDecl(c), _, _) =>
              code += PushInst(if (c.arity == 0) new VMRecord(c.name, Vector(), c.symbolMap, c.stringMap) else c)
            case DeclVarResult(f @ FunctionDecl(_, _, _, arity, _, -1, _), _, fidx) =>
              f.fixups += code.length
              code += PushFunctionReferenceInst(-1, name, arity, fidx)
            case DeclVarResult(FunctionDecl(_, _, _, arity, _, entry, _), _, fidx) =>
              code += PushFunctionReferenceInst(entry, oname, arity, fidx)
            case DeclVarResult(_, _, _) => problem(pos, s"symbol may not be referenced in this context '$oname'")
            case ConstantVarResult(c)   => code += PushInst(c)
            case NoneVarResult          =>
//							println( name )
              problem(pos, s"unknown symbol '$oname'")
          }
        case RangeExpressionAST(pf, f, pt, t, pb, b, inclusive) =>
          _emit(f)
          _emit(t)
          _emit(b)
          code += RangeInst(pf, pt, pb, inclusive)
        case SequenceExpressionAST(pf, f, pt, t, pb, b, inclusive) =>
          _emit(GenerateExpressionAST(pf, RangeExpressionAST(pf, f, pt, t, pb, b, inclusive)))
        case UnboundedLazyListExpressionAST(pf, f, pb, b) =>
          _emit(f)
          _emit(b)
          code += UnboundedLazyListInst(pf, pb)
        case ApplyExpressionAST(epos, f, apos, args, _) =>
          args map { case (_, a) => a } foreach _emit
          _emit(f)
          code += CallIndirectInst(epos, apos, args map { case (p, _) => p }, args.length)
        case DotExpressionAST(epos, expr, apos, field) =>
          _emit(expr)
          code += DotOperatorInst(epos, apos, field)
        case LiteralExpressionAST(v) => code += PushInst(v)
        case InterpolationExpressionAST(l) =>
          l.reverse foreach _emit
          code += ConcatenateInst(l.length)
        case TupleExpressionAST(l) =>
          for (e <- l)
            _emit(e)

          code += TupleInst(l.length)
        case ListExpressionAST(l) =>
          for (e <- l)
            _emit(e)

          code += ListInst(l.length) // todo: use ListBuffer approach instead
        case SetExpressionAST(l) =>
          code += PushInst(Set())

          for (e <- l) {
            _emit(e)
            code += SetInst
          }
        case MapExpressionAST(l) =>
          code += PushInst(Map())

          for ((k, v) <- l) {
            _emit(k)
            _emit(v)
            code += MapInst
          }
        case ErrorExpressionAST(pos, error) => problem(pos, error)
        case AlternationStructureAST(l) =>
          val jumps = new ArrayBuffer[Int]

          for (s <- l.init) {
            val backptr = code.length

            code += null
            _emit(s)
            jumps += code.length
            code += null
            code(backptr) = ChoiceInst(code.length - backptr - 1)
          }

          _emit(l.last)

          for (b <- jumps)
            code(b) = BranchInst(code.length - b - 1)
        case pat: PatternAST => compile(pat, dir == Forward)
        case SetValueExpressionAST(pos, name, oname, expr) =>
          val idx =
            variable(name, namespaces) match {
              case d @ VarDecl(idx, _, _) =>
                d.set = true
                idx
              case RecordDecl(_) => problem(pos, s"record not val: $oname")
              case _             => problem(pos, "*** BUG ***")
            }

          _emit(expr)
          code += DupInst
          code += (if (namespaces isEmpty)
                     SetGlobalInst(idx, VariableAssignable = false)
                   else
                     SetLocalInst(idx, VariableAssignable = false))
        case AssignmentExpressionAST(lhs, op, rhs) =>
          val len = lhs.length

          if (len > rhs.length)
            problem(lhs.drop(rhs.length).head._1, "left hand side has more components than right hand side")
          else if (len < rhs.length)
            problem(rhs.drop(len).head._1, "right hand side has more components than left hand side")

          for ((_, l) <- lhs)
            _emit(l)

          for ((_, r) <- rhs) {
            _emit(r)
            emitderef()
          }

          code += AssignmentInst(len,
                                 lhs map { case (p, _) => p } toVector,
                                 if (op == Symbol("")) null else op,
                                 rhs map { case (p, _) => p } toVector)
        case ScanAssignmentExpressionAST(lpos, lhs, rhs) =>
          _emit(lhs)
          code += DupInst
          code += BeginScanInst(lpos)
          _emit(rhs)
          code += EndScanInst
          code += AssignmentInst(1, Vector(lpos), null, null)
        case ReversableAssignmentExpressionAST(lpos, lhs, rhs) =>
          _emit(lhs)
          code += DupInst
          code += DataBacktractInst(lpos)
          _emit(rhs)
          code += AssignmentInst(1, Vector(lpos), null, null)
        case UnaryExpressionAST(op, pos, expr) =>
          _emit(expr)
          code += UnaryInst(op, pos)
        case GenerateExpressionAST(pos, collection) =>
          _emit(collection)
          code += GeneratorInst
        case BinaryExpressionAST(lpos, left, op, rpos, right) =>
          _emit(left)
          _emit(right)
          code += BinaryInst(lpos, op, rpos)
        case BlockExpressionAST(l) =>
          l.init foreach {
            case s: DeclarationStatementAST => _emit(s)
            case e: ExpressionAST =>
              val backptr = code.length

              code += null
              markNesting += 1
              _emit(e)
              markNesting -= 1
              code += UnmarkInst
              code(backptr) = MarkInst(code.length - backptr - 1)
          }

          _emit(l.last)

          if (l.last.isInstanceOf[DeclarationStatementAST])
            code += PushInst(())
        case OrExpressionAST(l, r) =>
          val backptr = code.length

          code += null
          _emit(l)

          val jump = code.length

          code += null
          code(backptr) = ChoiceInst(code.length - backptr - 1)
          _emit(r)
          code(jump) = BranchInst(code.length - jump - 1)
        case AndExpressionAST(l, r) =>
          _emit(l)
          code += DropInst
          _emit(r)
        case ConditionalExpressionAST(cond, els) =>
          val jumps = new ArrayBuffer[Int]
          val last = cond.last

          for (e @ (c, yes) <- cond) {
            comment("--- if condition begin")

            val backptr = code.length

            if (els.isDefined || (e ne last))
              code += null
            else
              code += MarkStateInst

            markNesting += 1
            _emit(c)
//						code += FailIfFalseInst
            markNesting -= 1
            code += UnmarkInst
            comment("--- if condition end; then begin")
            _emit(yes)
            comment("--- if then end")

            if (els.isDefined || (e ne last)) {
              jumps += code.length
              code += null
              code(backptr) = MarkInst(code.length - backptr - 1)
            }
          }

          if (els isDefined)
            _emit(els get)

          for (b <- jumps)
            code(b) = BranchInst(code.length - b - 1)
        case RepeatExpressionAST(label: Option[String], body) =>
          val loop = code.length
          val breaks = new ArrayBuffer[Int]
          val continues = new ArrayBuffer[Int]

          loops += Label("repeat", label orNull, markNesting, loop, breaks, continues)

          code += MarkInst(loop - code.length - 1)
          markNesting += 1
          _emit(body)
          markNesting -= 1
          code += UnmarkInst
          code += BranchInst(loop - code.length - 1)

          for (b <- breaks)
            code(b) = BranchInst(code.length - b - 1)

          loops pop
        case WhileExpressionAST(label, cond, body, els) =>
          val loop = code.length
          val breaks = new ArrayBuffer[Int]
          val continues = new ArrayBuffer[Int]

          loops += Label("while", label orNull, markNesting, loop, breaks, continues)

          if (els isDefined)
            code += null
          else
            code += MarkStateInst

          markNesting += 1
          _emit(cond)
          markNesting -= 1
          code += UnmarkInst

          if (body isDefined) {
            code += MarkInst(loop - code.length - 1)
            markNesting += 1
            _emit(body get)
            markNesting -= 1
            code += UnmarkInst
          }

          code += BranchInst(loop - code.length - 1)

          if (els isDefined) {
            code(loop) = MarkInst(code.length - loop - 1)
            _emit(els get)
          }

          for (b <- breaks)
            code(b) = BranchInst(code.length - b - 1)

          loops pop
        case BreakExpressionAST(pos, label, expr) =>
          val Label(construct, _, nesting, _, breaks, _) =
            if (label isDefined)
              loops search ((_: Label).name == label.get) match {
                case None    => problem(pos, s"label unknown: $loops")
                case Some(l) => l
              } else
              loops top

          construct match {
            case _ =>
              for (_ <- 1 to markNesting - nesting)
                code += UnmarkInst

              expr match {
                case None    => code += PushInst(())
                case Some(e) => _emit(e)
              }

              breaks += code.length
              code += null
          }
        case ContinueExpressionAST(pos, label) =>
          val Label(construct, _, nesting, loop, _, continues) =
            if (label isDefined)
              loops search ((_: Label).name == label.get) match {
                case None    => problem(pos, s"label unknown: $loops")
                case Some(l) => l
              } else
              loops top

          construct match {
            case "every" =>
              for (_ <- 1 until markNesting - nesting)
                code += UnmarkInst

              code += FailInst
            case "repeat" | "while" =>
              for (_ <- 1 to markNesting - nesting)
                code += UnmarkInst

              code += BranchInst(loop - code.length - 1)
          }
        case EveryExpressionAST(label, gen, body, els) =>
          val loop = code.length
          val breaks = new ArrayBuffer[Int]
          val continues = new ArrayBuffer[Int]

          loops += Label("every", label orNull, markNesting, loop, breaks, continues)

          if (els isDefined)
            code += null
          else
            code += MarkStateInst

          markNesting += 1
          _emit(gen)
          code += DropInst

          if (body isDefined) {
            comment("--- every body begin ---")
            code += MarkStateInst
            markNesting += 1
            _emit(body get)
            markNesting -= 1
            code += UnmarkInst
            comment("--- every body end ---")
          }

          markNesting -= 1
          code += FailInst

          if (els isDefined) {
            code(loop) = MarkInst(code.length - loop - 1)
            _emit(els get)
          }

          for (b <- breaks)
            code(b) = BranchInst(code.length - b - 1)

          for (b <- continues)
            code(b) = BranchInst(code.length - b - 1)

          loops pop
        case ForExpressionAST(label, gen, body, els) =>
          def forloop(gs: List[GeneratorExpressionAST]): ExpressionAST =
            gs match {
              case Nil => body
              case hd :: tl =>
                EveryExpressionAST(if (gs == gen) label else None, hd, Some(forloop(tl)), if (gs == gen) els else None)
            }

          _emit(forloop(gen))
        //				case ForLoopExpressionAST( label, gen, body, els ) =>
        //					comment( "--- iterator begin ---" )
        //					_emit( gen.traversable )
        //					code += IteratorInst( gen.pos )
        //					_emit( WhileExpressionAST(label, gen, body, els) )
        //				case IteratorExpressionAST( structure, pos, traversable, filter ) =>
        //					code += IterateInst
        //					code += MarkInst( 1 )
        //					code += BranchInst( 1 )
        //					code += ErrorInst( pos, "match error" )
        //					code += BindingsInst
        //					markNesting += 1
        //					comment( "--- iterator unify begin ---" )
        //					unify( structure, pos, namespaces )
        //					markNesting -= 1
        //					code += UnmarkSameDataInst
        //
        //					if (namespaces isEmpty)
        //						code += SetGlobalsInst( removeDuplicates(indexes(structure, Nil)) )
        //					else
        //						code += SetLocalsInst( removeDuplicates(indexes(structure, namespaces)) )
        //
        //					if (filter isDefined) {
        //						code += MarkStateInst
        //						_emit( filter get )
        //						code += UnmarkInst
        //					}
        //
        //					code += PushInst( () )
        //					comment( "--- iterator unify end ---" )
        case GeneratorExpressionAST(structure, pos, traversable, filter) =>
          comment("--- generator begin ---")
          _emit(traversable)
          code += GeneratorInst
          code += MarkInst(1)
          code += BranchInst(1)
          code += ErrorInst(pos, "match error")
          code += BindingsInst
          markNesting += 1
          comment("--- generator unify begin ---")
          unify(structure, pos, namespaces)
          markNesting -= 1
          code += UnmarkSameDataInst

          if (namespaces isEmpty)
            code += SetGlobalsInst(removeDuplicates(indexes(structure, Nil)))
          else
            code += SetLocalsInst(removeDuplicates(indexes(structure, namespaces)))

          if (filter isDefined) {
            code += MarkStateInst
            _emit(filter get)
            code += UnmarkInst
          }

          code += PushInst(())
          comment("--- generator unify end ---")
        case ListComprehensionExpressionAST(ComprehensionAST(expr, gen)) =>
          code += PushFunctionInst((_: VM) => new VMArray)
          frameSameData { _emit(ForExpressionAST(None, gen, ComprehensionBodyAST(expr), None)) }
          code += ToListInst
        case SetComprehensionExpressionAST(ComprehensionAST(expr, gen)) =>
          code += PushFunctionInst((_: VM) => new mutable.HashSet)
          frameSameData { _emit(ForExpressionAST(None, gen, ComprehensionBodyAST(expr), None)) }
          code += ToSetInst
        case ComprehensionBodyAST(expr) =>
          comment("--- comprehension body start ---")
          _emit(expr)
          code += AssignmentInst(1, Vector(null), Symbol("+"), Vector(null))
          comment("--- comprehension body end ---")
        case FailExpressionAST        => code += FailInst
        case SectionExpressionAST(op) => code += PushInst(SectionOperation(op))
        case LeftSectionExpressionAST(pos, LiteralExpressionAST(v), _, op, _) =>
          code += PushInst(LeftSectionOperation(pos, v, op))
        case LeftSectionExpressionAST(pos, _, lambda, op, _) =>
          _emit(lambda)
        case RightSectionExpressionAST(op, pos, LiteralExpressionAST(v), _, _) =>
          code += PushInst(RightSectionOperation(op, pos, v))
        case RightSectionExpressionAST(op, pos, _, lambda, _) =>
          _emit(lambda)
        case ReturnExpressionAST(expr) => //todo: local variables should be dereferenced before being returned (pg. 126)
          for (_ <- 1 to markNesting)
            code += UnmarkInst

          _emit(expr)
          code += FunctionReturnInst
        case YieldExpressionAST(expr, result) =>
          comment("--- yield begin ---")

          val susp = code.length

          code += null
          _emit(expr)
          code += FunctionReturnInst
          code(susp) = ChoiceInst(code.length - susp - 1)

          result match {
            case None    => code += PushInst(())
            case Some(r) => _emit(r)
          }

          comment("--- yield end ---")
        case NotExpressionAST(expr) =>
          val choice = code.length

          code += null
          _emit(expr)
          code += UnmarkInst
          code += FailInst
          code(choice) = MarkInst(code.length - choice - 1)
          code += PushInst(())
        case f @ FunctionExpressionAST(name, pos, parms, arb, parts, where) =>
          val oname = "$" + pos.toString

          _emit(DefAST(oname, f))

          findvariable(name, namespaces) match {
            case DeclVarResult(FunctionDecl(_, _, _, arity, _, entry, _), _, fidx) =>
              code += PushFunctionReferenceInst(entry, oname, arity, fidx)
            case _ => sys.error("problem")
          }
        case PartialFunctionExpressionAST(cases) =>
        //			case IteratorExpressionAST( e, gs ) =>
        //				push( iterator(e, gs) )
        case ScanExpressionAST(spos, subject, expr) =>
          _emit(subject)
          code += BeginScanInst(spos)
          _emit(expr)
          code += EndScanInst
        case EqExpressionAST(expr, target, result) =>
          _emit(expr)
          _emit(target)
          code += EqInst
          code += BranchIfInst(1)
          code += FailInst
          _emit(result)
        case RepeatedAlternationExpressionAST(expr) =>
          val start = code.length

          code += MarkStateInst
          _emit(expr)
          code += ChangeMarkInst(start - code.length - 1)
        case DereferenceExpressionAST(expr) =>
          _emit(expr)
          emitderef()
        case MatchExpressionAST(expr) =>
          _emit(expr)
          code += NativeInst { vm =>
            vm.matchString(vm.pop.asInstanceOf[String]) match {
              case Fail   => vm.fail()
              case p: Int => vm.push(vm.tabToPosition(p))
            }
          }
        case DefinedExpressionAST(expr) =>
          _emit(expr)
          code += DupInst
          code += PushInst(VMUndefined)
          code += EqInst
          code += BranchIfNotInst(1)
          code += FailInst
        case UndefinedExpressionAST(expr) =>
          _emit(expr)
          code += DupInst
          code += PushInst(VMUndefined)
          code += EqInst
          code += BranchIfInst(1)
          code += FailInst
        case DefinedOptionExpressionAST(expr) =>
          _emit(expr)
          code += DupInst
          code += PushInst(VMUndefined)
          code += EqInst
          code += BranchIfNotInst(3)
          code += DropInst
          code += PushNoneInst
          code += BranchInst(1)
          code += SomeInst
        case SysvarExpressionAST(pos, name) =>
          sysvars get name match {
            case None    => problem(pos, s"unknown system variable '$name'")
            case Some(v) => code += PushFunctionInst(v)
          }
        case BracketExpressionAST(epos, f, apos, arg) =>
          _emit(f)
          _emit(arg)
          code += BracketInst(epos, apos)
        case PatternExpressionAST(pat) =>
          _emit(pat)
          code += PushCaptureGroupsInst
        case PatternExpressionStringsAST(pat) =>
          _emit(pat)
          code += PushCaptureGroupsStringsInst
        case NativeFunctionExpressionAST(function) => code += PushFunctionInst(function)
        //				case c: StatementAST => //sys.error( s"missed a case: $c" )
        case CaptureGroupsExpressionAST                => code += PushCaptureGroupsInst
        case _: StructureAST                           =>
        case t if emitExtension isDefinedAt (t, _emit) => emitExtension(t, _emit)
      }
    }

    _emit(ast)
  }

  private def emitderef(): Unit =
    code.last match {
      case PushInst(_) =>
      case _           => code += DerefInst
    }

  private def expandMacros(ast: AST): AST =
    ast match {
      case SourceAST(statements) => SourceAST(statements map expandMacros)
      case a                     => a
    }

  private def expandMacros(ast: StatementAST): StatementAST =
    ast match {
      case a: ExpressionAST => expandMacros(a)
      case a                => a
    }

  private def expandMacros(ast: ExpressionAST): ExpressionAST =
    ast match {
      case ApplyExpressionAST(_, VariableExpressionAST(_, name, _), _, args, _) if macros contains name =>
        macros(name)(args map { case (_, e) => expandMacros(e) }).asInstanceOf[ExpressionAST]
      case ApplyExpressionAST(epos, f, apos, args, tailrecursive) =>
        ApplyExpressionAST(epos, f, apos, args map { case (p, e) => (p, expandMacros(e)) }, tailrecursive)
      case ScanExpressionAST(spos, subject, expr) => ScanExpressionAST(spos, expandMacros(subject), expandMacros(expr))
      case RegexLiteralAST(pos, regex) =>
        PatternParser.parseRegexWithResult(regex) match {
          case Right(pat)       => PatternExpressionAST(pat)
          case Left((_, error)) => problem(pos, error)
        }
      case a => a
    }

  private def compile(pat: PatternAST, mode: Boolean): Unit =
    pat match {
      case LookaheadClassPattern(clas) =>
        if (mode)
          code += ClassInst(clas)
        else
          code += ClassReverseInst(clas)
      case LookbehindClassPattern(clas) =>
        if (mode)
          code += ClassReverseInst(clas)
        else
          code += ClassInst(clas)
      case FlagConditionalPattern(mask, set, unset) =>
        code += FlagsClearInst(mask)

        val conditional = code.length

        code += null
        compile(set, mode)

        val exit = code.length

        code += null
        code(conditional) = BranchIfInst(code.length - conditional - 1)
        compile(unset, mode)
        code(exit) = BranchInst(code.length - exit - 1)
      case BeginningOfInputPattern =>
        code += BeginningPositionMatchInst
      case EndOfInputPattern =>
        code += EndPositionMatchInst
      case AtomicPattern(subpat) =>
        code += AtomicInst
        compile(subpat, mode)
        code += CutInst
      case ClassPattern(clas) =>
        if (mode) {
          code += ClassInst(clas)
          code += AdvanceInst
        } else {
          code += ReverseInst
          code += ClassReverseInst(clas)
        }
      case CompiledSubPattern(forward, reverse) =>
        code ++= (if (mode) forward.code else reverse.code)
      case CapturePattern(key, subpat, conversion) =>
        code += CaptureBeginInst(key, -1)
        compile(subpat, mode)
        code += CaptureSaveInst(key, conversion)
      case NumberedCapturePattern(subpat, n) =>
        code += CaptureBeginInst(n.toString, n)
        compile(subpat, mode)
        code += CaptureSaveInst(n.toString, null)
      case AnyPattern =>
        if (mode)
          code += AdvanceInst
        else
          code += ReverseInst
      case DotPattern => code += DotInst
      case AlternationPattern(alt) =>
        val jumps = new ArrayBuffer[Int]

        for (p <- alt.init) {
          val backptr = code.length

          code += null
          compile(p, mode)
          jumps += code.length
          code += null
          code(backptr) = ChoiceInst(code.length - backptr - 1)
        }

        compile(alt.last, mode)

        for (b <- jumps)
          code(b) = BranchInst(code.length - b - 1)
      case StringPattern(s) =>
        emit(s, null)

        if (mode)
          code += StringMatchInst
        else
          code += StringMatchReverseInst
      case LiteralPattern(s) =>
        if (mode)
          code += LiteralMatchInst(s)
        else
          code += LiteralMatchReverseInst(s)
      case ReferencePattern(key) =>
        if (mode)
          code += ReferenceMatchInst(key)
        else
          code += ReferenceMatchReverseInst(key)
      case ConcatenationPattern(seq) =>
        for (p <- if (mode) seq else seq.reverse)
          compile(p, mode)
      case OptionalPattern(subpat) =>
        val backptr = code.length

        code += null
        compile(subpat, mode)
        code(backptr) = ChoiceInst(code.length - backptr - 1)
      case ReluctantOptionalPattern(subpat) =>
        code += ChoiceInst(1)

        val branch = code.length

        code += null
        compile(subpat, mode)
        code(branch) = BranchInst(code.length - branch)
      case ZeroOrMorePattern(subpat) =>
        val backpatch = code.length

        code += null
        code += SavePointerInst
        compile(subpat, mode)
        code += ZeroLengthInst
        code += BranchIfNotInst(backpatch - code.length - 1)
        code(backpatch) = ChoiceInst(code.length - backpatch - 1)
      case ReluctantZeroOrMorePattern(subpat) =>
        code += ChoiceInst(1)

        val branch = code.length

        code += null
        compile(subpat, mode)
        code += ChoiceInst(branch - code.length)
        code(branch) = BranchInst(code.length - branch - 1)
      case OneOrMorePattern(subpat) =>
        val start = code.length

        code += SavePointerInst
        compile(subpat, mode)
        code += ZeroLengthInst
        code += BranchIfInst(2)
        code += ChoiceInst(1)
        code += BranchInst(start - code.length - 1)
      case ReluctantOneOrMorePattern(subpat) =>
        val start = code.length

        compile(subpat, mode)
        code += ChoiceInst(start - code.length - 1)
      case RepeatPattern(subpat, lower, _, upper) =>
        code += RepeatBeginInst(lower, upper)

        val loop = code.length

        code += null
        code += SavePointerInst
        compile(subpat, mode)
        code += ZeroLengthInst
        code += BranchIfInst(1)
        code += BranchInst(loop - code.length - 1)
        code(loop) = RepeatLoopInst(code.length - loop - 1)
      case ReluctantRepeatPattern(subpat, lower, _, upper) =>
        code += RepeatBeginInst(lower, upper)

        val loop1 = code.length

        code += null
        compile(subpat, mode)
        code += BranchInst(loop1 - code.length - 1)
        code(loop1) = LowerRepeatLoopInst(code.length - loop1 - 1)

        val loop2 = code.length

        code += null
        compile(subpat, mode)
        code += BranchInst(loop2 - code.length - 1)
        code(loop2) = UpperRepeatLoopInst(code.length - loop2 - 1)
      case LookaheadPattern(subpat) =>
        code += AtomicInst
        code += SavePointerInst
        compile(subpat, mode)
        code += RestorePositionInst
        code += CutInst
      case NegationPattern(subpat) =>
        code += AtomicInst

        val choice = code.length

        code += null
        compile(subpat, mode)
        code += CutInst
        code += FailInst
        code(choice) = ChoiceInst(code.length - choice - 1)
        code += DropInst
      case SetFlagsPattern(setmask, clearmask) => code += SetFlagsInst(setmask, clearmask)
      case SetFlagsGroupPattern(setmask, clearmask, subpat) =>
        code += SaveFlagsInst
        code += SetFlagsInst(setmask, clearmask)
        compile(subpat, mode)
        code += RestoreFlagsInst
      case LookbehindPattern(subpat) =>
        code += AtomicInst
        code += SavePointerInst
        compile(subpat, !mode)
        code += RestorePositionInst
        code += CutInst
      case GroupPattern(subpat)   => compile(subpat, mode)
      case NativePattern(matcher) => code += NativeInst(matcher)
      case EmptyPattern           => /* no code generated */
    }

  def compile(ast: AST): Compilation = {
    val captureTrees = { //todo: needs to be done differently to support regex macro expressions
      var count = 0
      var map = TreeMap[Int, Node]()

      def _numberCaptureGroups(pat: AST, buf: ArrayBuffer[Node]): Unit = {
        pat match {
          case ConcatenationPattern(seq) => seq foreach (_numberCaptureGroups(_, buf))
          case AlternationPattern(alt)   => alt foreach (_numberCaptureGroups(_, buf))
          case r: NumberedCapturePattern =>
            val nodes = new ArrayBuffer[Node]()

            r.n = count
            count += 1
            _numberCaptureGroups(r.subpat, nodes)

            val node = Node(r.n.toString, nodes.toList)

            buf += node
            map += (r.n -> node)
          case op: OperatorPattern => _numberCaptureGroups(op.subpat, buf)
          case _                   =>
        }
      }

      val nodes = new ArrayBuffer[Node]()

      _numberCaptureGroups(ast, nodes)
      map.values.to(ArraySeq)
    }

    globals = new Namespace
    code = new ArrayBuffer[VMInst]

    val ast1 = expandMacros(ast)

    decls(ast1, Nil)
    emit(ast1, Nil)

    val functions =
      if (globals.scopes isEmpty)
        Map.empty[String, (Int, Int)]
      else
        globals.scopes.head.symbols filter {
          case (k, _: FunctionDecl) if k matches """1[a-zA-Z_]+""" => true
          case _                                                   => false
        } map {
          case (k, FunctionDecl(_, _, _, arity, _, entry, _)) => (k.substring(1), (entry, arity))
          case _                                              => sys.error("problem")
        } toMap

    val variables =
      if (globals.scopes isEmpty)
        Map.empty[String, Int]
      else
        globals.scopes.head.symbols filter {
          case (k, _: VarDecl) if k matches """1[a-zA-Z_]+""" => true
          case _                                              => false
        } map {
          case (k, VarDecl(idx, _, _)) => (k.substring(1), idx)
          case _                       => sys.error("problem")
        } toMap

    new Compilation(functions, variables, constants, captureTrees, code.to(ArraySeq))
  }
}
