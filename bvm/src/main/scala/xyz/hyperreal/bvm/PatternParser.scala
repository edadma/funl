package xyz.hyperreal.bvm

import scala.util.parsing.input.{CharSequenceReader, Position, Positional}
import util.parsing.combinator.{PackratParsers, RegexParsers}


object PatternParser {

	def parseRegex( regex: String ) = (new PatternParser).parseFromString( regex )

	def parseRegexWithResult( regex: String ) = (new PatternParser).parseFromStringWithResult( regex )

}

class PatternParser extends RegexParsers with PackratParsers {

	override val skipWhitespace = false

	lazy val pos = positioned( success( new Positional {} ) ) ^^ (_.pos)

	lazy val number: PackratParser[Int] = """\d+""".r ^^ (_.toInt)

	lazy val name: PackratParser[String] = regex( """[a-zA-Z][a-zA-Z0-9]*""".r )

	lazy val regex: PackratParser[PatternAST] = altRegex

	lazy val altRegex: PackratParser[PatternAST] =
		opt(conRegex) ~ rep("|" ~> opt(conRegex)) ^^ {
			case None ~ Nil => EmptyPattern
			case Some( f ) ~ Nil => f
			case Some( f ) ~ r => AlternationPattern( f +: r.map(_.getOrElse( EmptyPattern )) )
			case None ~ r => AlternationPattern( EmptyPattern +: r.map(_.getOrElse( EmptyPattern )) )
		}

	lazy val conRegex: PackratParser[PatternAST] =
		quantRegex ~ quantRegex.* ^^ {
			case f ~ Nil => f
			case f ~ r => ConcatenationPattern( f +: r )
		}

	def quant( r: PatternAST, q: PatternAST => PatternAST ) =
		r match {
			case LiteralPattern(s: String) if s.length > 1 => ConcatenationPattern( List(LiteralPattern(s.init), q(LiteralPattern(s.last.toString))) )
			case _ => q( r )
		}

	lazy val quantRegex: PackratParser[PatternAST] =
		primaryRegex <~ "??" ^^ (r => quant( r, ReluctantOptionalPattern )) |
		primaryRegex <~ "+?" ^^ (r => quant( r, ReluctantOneOrMorePattern )) |
		primaryRegex <~ "*?" ^^ (r => quant( r, ReluctantZeroOrMorePattern )) |
		primaryRegex ~ ("{" ~> number) ~ (opt("," ~> opt(pos ~ number)) <~ "}?") ^^ {
			case p ~ l ~ None => quant( p, ReluctantRepeatPattern(_, l, null, Some(l)) )
			case p ~ l ~ Some( None ) => quant( p, ReluctantRepeatPattern(_, l, null, None) )
			case p ~ l ~ Some( Some(pos ~ u) ) => quant( p, ReluctantRepeatPattern(_, l, pos, Some(u)) )
		} |
		primaryRegex <~ "?+" ^^ (r => quant( r, s => AtomicPattern(OptionalPattern(s)) )) |
		primaryRegex <~ "++" ^^ (r => quant( r, s => AtomicPattern(OneOrMorePattern(s)) )) |
		primaryRegex <~ "*+" ^^ (r => quant( r, s => AtomicPattern(ZeroOrMorePattern(s)) )) |
		primaryRegex ~ ("{" ~> number) ~ (opt("," ~> opt(pos ~ number)) <~ "}+") ^^ {
			case p ~ l ~ None => quant( p, r => AtomicPattern(RepeatPattern(r, l, null, Some(l))) )
			case p ~ l ~ Some( None ) => quant( p, r => AtomicPattern(RepeatPattern(r, l, null, None)) )
			case p ~ l ~ Some( Some(pos ~ u) ) => quant( p, r => AtomicPattern(RepeatPattern(r, l, pos, Some(u))) )
		} |
		primaryRegex <~ "?" ^^ (r => quant( r, OptionalPattern )) |
		primaryRegex <~ "+" ^^ (r => quant( r, OneOrMorePattern )) |
		primaryRegex <~ "*" ^^ (r => quant( r, ZeroOrMorePattern )) |
		primaryRegex ~ ("{" ~> number) ~ (opt("," ~> opt(pos ~ number)) <~ "}") ^^ {
			case p ~ l ~ None => quant( p, RepeatPattern(_, l, null, Some(l)) )
			case p ~ l ~ Some( None ) => quant( p, RepeatPattern(_, l, null, None) )
			case p ~ l ~ Some( Some(pos ~ u) ) => quant( p, RepeatPattern(_, l, pos, Some(u)) )
		} |
		primaryRegex

	lazy val classRegex: PackratParser[PatternAST] =
		"[^" ~> charSet <~ "]" ^^ (c => ClassPattern( Pattern.except(c) )) |
		"[" ~> charSet <~ "]" ^^ ClassPattern

	lazy val charSet: PackratParser[Set[Char]] =
		rep1(("""[^\]]""".r <~ "-") ~ ".".r | "[:lower:]" | "[:upper:]" | """[^\]]""".r) ~ opt("&&[" ~> charSet <~ "]") ^^ {
			case es ~ None => es map {
				case (l: String) ~ (u: String) => l.head to u.head toSet
				case "[:lower:]" => 'a' to 'z' toSet
				case "[:upper:]" => 'A' to 'Z' toSet
				case c: String => c.toSet
				} reduce (_ union _)
			case es ~ Some( s ) => (es map {
				case (l: String) ~ (u: String) => l.head to u.head toSet
				case c: String => c.toSet
				} reduce (_ union _)) & s
		}

	private def chars2flags( s: String ) = {
		var mask = 0

		s foreach {
			case 'i' => mask |= Pattern.CASE_INSENSITIVE
			case 'd' => mask |= Pattern.UNIX_LINES
			case 'm' => mask |= Pattern.MULTILINE
			case 's' => mask |= Pattern.DOTALL
			case 'u' => mask |= Pattern.UNICODE_CASE
			case 'x' => mask |= Pattern.COMMENTS
			case 'U' => mask |= Pattern.UNICODE_CHARACTER_CLASS
		}

		mask
	}

	lazy val primaryRegex: PackratParser[PatternAST] =
		"""[^\\^$.|?*+(){\[ \n\t]+(?![+*?])""".r ^^ { s => LiteralPattern(s) } |
		"""[^\\^$.|?*+(){\[ \n\t]""".r ^^ { s => LiteralPattern(s) } |
		"""\""" ~> """[\\^$.|?*+(){}\[\] ]""".r ^^ { s => LiteralPattern(s) } |
		"""\0""" ~> "(?:(?:[0-3])[0-7])[0-7]".r ^^ (o => LiteralPattern(Integer.parseInt(o, 8).toChar.toString)) |
		"""\x""" ~> "[0-9a-fA-F]{2}".r ^^ (o => LiteralPattern(Integer.parseInt(o, 16).toChar.toString)) |
		("""\""" ~ "u") ~> "[0-9a-fA-F]{4}".r ^^ (o => LiteralPattern(Integer.parseInt(o, 16).toChar.toString)) |
		"""\t""" ^^^ LiteralPattern("\t") |
		"""\n""" ^^^ LiteralPattern("\n") |
		"""\r""" ^^^ LiteralPattern("\r") |
		"""\f""" ^^^ LiteralPattern("\f") |
		"""\a""" ^^^ LiteralPattern("\u0007") |
		"""\e""" ^^^ LiteralPattern("\u001B") |
		"""\c""" ~> "[A-Z]".r ^^ (o => LiteralPattern((o.head - 'A').toChar.toString)) |
		classRegex |
		"." ^^^ DotPattern |
		"""\d""" ^^^ ClassPattern(DIGIT_CLASS) |
		"""\D""" ^^^ ClassPattern(Pattern.except(DIGIT_CLASS)) |
		"""\h""" ^^^ ClassPattern(HORIZONTAL_WHITESPACE_CLASS) |
		"""\H""" ^^^ ClassPattern(Pattern.except(HORIZONTAL_WHITESPACE_CLASS)) |
		"""\s""" ^^^ ClassPattern(WHITESPACE_CLASS) |
		"""\S""" ^^^ ClassPattern(Pattern.except(WHITESPACE_CLASS)) |
		"""\v""" ^^^ ClassPattern(VERTICAL_WHITESPACE_CLASS) |
		"""\V""" ^^^ ClassPattern(Pattern.except(VERTICAL_WHITESPACE_CLASS)) |
		"""\w""" ^^^ ClassPattern(WORD_CLASS) |
		"""\W""" ^^^ ClassPattern(Pattern.except(WORD_CLASS)) |
		"""\""" ~> """\d""".r ^^ ReferencePattern |
		"""\k<""" ~> name <~ ">" ^^ ReferencePattern |
		"""^""" ^^^ BeginningOfLinePattern |
		"""$""" ^^^ EndOfLinePattern |
		"""\b""" ^^^ WordBoundaryPattern |
		"""\B""" ^^^ NonWordBoundaryPattern |
		"""\A""" ^^^ BeginningOfInputPattern |
		"""\z""" ^^^ EndOfInputPattern |
		"""\Z""" ^^^ EndOfInputBeforeFinalTerminatorPattern |
		"""\R""" ^^^ LineBreakPattern |
		"(?:" ~> optRegex <~ ")" ^^ GroupPattern |
		"(?>" ~> optRegex <~ ")" ^^ AtomicPattern |
		"(?=" ~> optRegex <~ ")" ^^ LookaheadPattern |
		"(?!" ~> optRegex <~ ")" ^^ NegationPattern |
		"(?<=" ~> optRegex <~ ")" ^^ LookbehindPattern |
		"(?<!" ~> optRegex <~ ")" ^^ (r => NegationPattern( LookbehindPattern(r) )) |
		("(?<" ~> name <~ ">") ~ (optRegex <~ ")") ^^ { case n ~ r => CapturePattern( n, r, null ) } |
		("(?" ~> "[idmsuxU]*".r <~ "-") ~ ("[idmsuxU]*".r <~ ")") ^^ { case s ~ c => SetFlagsPattern( chars2flags(s), chars2flags(c) ) } |
		("(?" ~> "[idmsuxU]*".r <~ "-") ~ ("[idmsuxU]*".r <~ ":") ~ (optRegex <~ ")") ^^ { case s ~ c ~ r => SetFlagsGroupPattern( chars2flags(s), chars2flags(c), r ) } |
		"(" ~> optRegex <~ ")" ^^ (r => NumberedCapturePattern( r ))

	lazy val optRegex: PackratParser[PatternAST] =
		opt( regex ) ^^ {
			case None => ConcatenationPattern( Nil )
			case Some( r ) => r
		}

	def problem( pos: Position, error: String ) =
		if (pos eq null)
			sys.error( error )
		else if (pos.line == 1)
			sys.error( s"$error\n${pos.longString}" )
		else
			sys.error( s"${pos.line}: $error\n${pos.longString}" )

	def parseFromStringWithResult[T]( src: String ) =
		parseAll( regex, new CharSequenceReader(src) ) match {
			case Success( tree, _ ) => Right( tree )
			case NoSuccess( error, rest ) => Left( (rest.pos, error) )
		}

	def parseFromString[T]( src: String ) =
		parseFromStringWithResult( src ) match {
			case Right( tree ) => tree
			case Left( (pos, error) ) => problem( pos, error )
		}

}
