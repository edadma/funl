package xyz.hyperreal

import java.io.ByteArrayOutputStream

import scala.util.parsing.input.Position
import xyz.hyperreal.bvm.Pattern._
import xyz.hyperreal.dal.BasicDAL

import scala.collection.immutable.ArraySeq

package object bvm {

  type NativeFunction = (_, _, _, _) => _
  type cset = Char => Boolean

  val DIGIT_CLASS: Char => Boolean = (_: Char).isDigit
  val HEXDIGIT_CLASS = new CSet(DIGIT_CLASS, 'a' to 'f', 'A' to 'F')
  val LETTER_CLASS: Char => Boolean = (_: Char).isLetter
  val ALPHANUM_CLASS = new CSet(LETTER_CLASS, DIGIT_CLASS)
  val WORD_CLASS = new CSet(ALPHANUM_CLASS, (_: Char) == '_')
  val NONWORD_CLASS: Char => Boolean = new CSet(unionOf(ALPHANUM_CLASS, (_: Char) == '_')).complement
  val WHITESPACE_CLASS: Set[Char] = " \t\r\u000B\n".toSet
  val HORIZONTAL_WHITESPACE_CLASS = new CSet('\u2000' to '\u200a', "\t\u00A0\u1680\u180e\u202f\u205f\u3000")
  val VERTICAL_WHITESPACE_CLASS: Set[Char] = "\n\u000B\f\r\u0085\u2028\u2029".toSet
  val TERMINATOR_CLASS: Set[Char] = "\u000A\u000B\u000C\u000D\u0085\u2028\u2029".toSet

  val LineBreakPattern: PatternAST = Pattern.compiledSubpattern(
    FlagConditionalPattern(
      UNIX_LINES,
      StringPattern(LiteralExpressionAST("\n")),
      AlternationPattern(List(StringPattern(LiteralExpressionAST("\r\n")), ClassPattern(TERMINATOR_CLASS)))
    ))
  val BeginningOfLinePattern: PatternAST = Pattern.compiledSubpattern(
    FlagConditionalPattern(
      MULTILINE,
      AlternationPattern(List(
        FlagConditionalPattern(UNIX_LINES, LookbehindClassPattern(_ == '\n'), LookbehindClassPattern(TERMINATOR_CLASS)),
        BeginningOfInputPattern)),
      BeginningOfInputPattern
    ))
  val EndOfLinePattern: PatternAST = Pattern.compiledSubpattern(
    FlagConditionalPattern(MULTILINE,
                           LookaheadPattern(AlternationPattern(List(LineBreakPattern, EndOfInputPattern))),
                           EndOfInputPattern))
  val EndOfInputBeforeFinalTerminatorPattern: PatternAST = Pattern.compiledSubpattern(
    LookaheadPattern(ConcatenationPattern(List(OptionalPattern(LineBreakPattern), EndOfInputPattern))))
  val NonEmptyInputPattern: PatternAST =
    Pattern.compiledSubpattern(NegationPattern(ConcatenationPattern(List(BeginningOfInputPattern, EndOfInputPattern))))
  val WordBoundaryPattern: PatternAST = Pattern.compiledSubpattern(
    AlternationPattern(
      List(
        ConcatenationPattern(
          List(LookbehindClassPattern(NONWORD_CLASS), LookaheadClassPattern(WORD_CLASS), NonEmptyInputPattern)),
        ConcatenationPattern(
          List(LookbehindClassPattern(WORD_CLASS), LookaheadClassPattern(NONWORD_CLASS), NonEmptyInputPattern))
      )))
  val NonWordBoundaryPattern: PatternAST = Pattern.compiledSubpattern(NegationPattern(WordBoundaryPattern))

  val HEXOCTET: PatternAST = Pattern.compiledSubpattern(ConcatenationPattern(List(ClassPattern(HEXDIGIT_CLASS))))
  val UUID: PatternAST =
    Pattern.compiledSubpattern(
      ConcatenationPattern(
        List(
          hexOctets(4),
          LiteralPattern("-"),
          hexOctets(2),
          LiteralPattern("-"),
          hexOctets(2),
          LiteralPattern("-"),
          hexOctets(2),
          LiteralPattern("-"),
          hexOctets(6)
        )))
  val INTEGER: PatternAST = Pattern.compiledSubpattern(OneOrMorePattern(ClassPattern(DIGIT_CLASS)))
  val HEXINTEGER: PatternAST = Pattern.compiledSubpattern(OneOrMorePattern(ClassPattern(HEXDIGIT_CLASS)))

  def hexOctets(octets: Int): PatternAST = RepeatPattern(HEXOCTET, octets, null, Some(octets))

  def deref(a: Any): Any =
    a match {
      case a: Assignable => a.value
      case _             => a
    }

  def argsderef(a: Any): Any =
    a match {
      case a: ArgList => ArgList(a.array map deref: _*)
      case _          => deref(a)
    }

  def problem(pos: Position, error: String): Nothing =
    sys.error(if (pos eq null) error else s"${pos.line}: $error\n${pos.longString}")

  def displayQuoted(a: Any): String =
    a match {
      case s: String =>
        var t = s

        for ((k, v) <- List("\\" -> "\\\\",
                            "\"" -> "\\\"",
                            "\t" -> "\\t",
                            "\b" -> "\\b",
                            "\f" -> "\\f",
                            "\n" -> "\\n",
                            "\r" -> "\\r",
                            "\b" -> "\\b"))
          t = t.replace(k, v)

        s""""$t""""
      case _ => display(a)
    }

  def display(a: Any): String =
    a match {
      case a: Array[_] => (a map display).mkString("Array(", ", ", ")")
      case l: List[_]  => (l map displayQuoted).mkString("[", ", ", "]")
      case s: LazyList[_] =>
        val howMany = 100
        val bunch = s take (howMany + 1)

        if (s isDefinedAt (howMany + 1))
          (bunch take howMany map display).mkString("[", ", ", ", ...]")
        else
          display(bunch toList)
      case s: collection.Set[_] if s isEmpty    => "void"
      case s: collection.Set[_]                 => s.map(display).mkString("{", ", ", "}")
      case m: collection.Map[_, _] if m isEmpty => "{}"
      case m: collection.Map[_, _] =>
        m.toList.map({ case (k, v) => displayQuoted(k) + ": " + displayQuoted(v) }).mkString("{", ", ", "}")
      case t: Vector[_]   => t.map(display).mkString("<", ", ", ">")
      case t: ArraySeq[_] => t.map(display).mkString("<", ", ", ">")
      case t: Tuple       => t.map(display).mkString("(", ", ", ")")
      case p: Product if p.productArity > 0 && !p.productPrefix.startsWith("Tuple") =>
        p.productPrefix + '(' + p.productIterator.map(display).mkString(", ") + ')'
      case p: Product if p.productArity == 0 => p.productPrefix
      //			case Some( a ) => "Some(" + display(a) + ")"
      case _ => String.valueOf(a)
    }

  val NUMERIC: Numeric[Number] =
    new Numeric[Number] {
      def parseString(str: String): Option[Number] = sys.error("shouldn't call Numeric.parseString()")

      def compare(x: Number, y: Number): Int = naturalCompare(x, y)

      def fromInt(x: Int): Integer = Integer.valueOf(x)

      def minus(x: Number, y: Number): Number = BasicDAL.compute(Symbol("-"), x, y)

      def negate(x: Number): Number = BasicDAL.negate(x)

      def plus(x: Number, y: Number): Number = BasicDAL.compute(Symbol("+"), x, y)

      def times(x: Number, y: Number): Number = BasicDAL.compute(Symbol("*"), x, y)

      def toDouble(x: Number): Double = x.asInstanceOf[Number].doubleValue

      def toFloat(x: Number): Float = x.asInstanceOf[Number].floatValue

      def toInt(x: Number): Int = x.asInstanceOf[Number].intValue

      def toLong(x: Number): Long = x.asInstanceOf[Number].longValue
    }

  val ORDERING: Ordering[Any] = (x: Any, y: Any) => naturalCompare(x, y)

  def naturalCompare(x: Any, y: Any): Int =
    (x, y) match {
      case (a: Number, b: Number) =>
        if (BasicDAL.relate(Symbol("<"), a, b))
          -1
        else if (BasicDAL.relate(Symbol(">"), a, b))
          1
        else
          0
      case (a: String, b: String)     => a compare b
      case (a: Seq[Any], b: Seq[Any]) => lexicographicalCompare(a, b)
      case (a: Product, b: Product) if a.productPrefix == b.productPrefix =>
        lexicographicalCompare(a.productIterator.toSeq, b.productIterator.toSeq)
      case _ => sys.error(s"non-comparable: $x, $y")
    }

  def lexicographicalCompare(a: Seq[Any], b: Seq[Any]): Int = {
    for ((u, v) <- a zip b)
      if (u != v)
        return naturalCompare(u, v)

    val (alen, blen) = (a.length, b.length)

    if (alen < blen)
      -1
    else if (alen > blen)
      1
    else
      0
  }

}
