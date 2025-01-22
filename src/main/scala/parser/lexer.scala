package parser

import parsley.Parsley
import parsley.character.newline
import parsley.token.descriptions.SpaceDesc
import parsley.token.descriptions.numeric.{ExponentDesc, NumericDesc}
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.symbol.ImplicitSymbol

object lexer {

  import parsley.token.Lexer
  import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc}
  import parsley.token.predicate.Basic

  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = Basic(c => c == '@' || c == '$' || c == '#'),
      identifierLetter = Basic(c => c.isLetterOrDigit || c == '_'),
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set("label", "fn", "extern", "call", "addi", "subi", "muli", "divi", "const", "struct", "ret", "branch", "goto"),
      hardOperators = Set("="),
    ),
    numericDesc = NumericDesc.plain.copy(
      octalExponentDesc = ExponentDesc.NoExponents,
      binaryExponentDesc = ExponentDesc.NoExponents,
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.haskell,
    ),
    spaceDesc = SpaceDesc.plain.copy(
      commentLine = "//",
      commentStart = "/*",
      commentEnd = "*/",
      nestedComments = true,
      space = Basic(c => c == ' ' || c == '\n' || c == '\t' || c == '\r'),
    )
  )
  private val lexer = new Lexer(desc)
  
  val LABEL_ID: Parsley[String] = lexer.lexeme.names.identifier(Basic(_ == '@'))
  val VAR_ID: Parsley[String] = lexer.lexeme.names.identifier(Basic(_ == '$'))
  val STRUCT_ID: Parsley[String] = lexer.lexeme.names.identifier(Basic(_ == '#'))
  val INTEGER: Parsley[BigInt] = lexer.lexeme.natural.number
  val FLOAT: Parsley[BigDecimal] = lexer.lexeme.floating.number
  val INT_OR_FLOAT: Parsley[Either[BigInt, BigDecimal]] = lexer.lexeme.unsignedCombined.number
  // Strictly speaking, Haskell files are probably encoded as UTF-8, but this
  // is not supported by Parsley _yet_
  val STRING: Parsley[String] = lexer.lexeme.string.fullUtf16
  val CHAR: Parsley[Int] = lexer.lexeme.character.fullUtf16
  val NEWLINE: Parsley[Unit] = lexer.lexeme(newline).void
  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits

  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
