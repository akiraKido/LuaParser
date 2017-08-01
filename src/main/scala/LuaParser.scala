import AST._

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

object LuaParserConsole {
  def main(args: Array[String]): Unit = {
    val code = Utils.readTextFromResource("test.lua")
    if (code.isEmpty) {
      println("file not found")
    } else {
      val luaObject = LuaParser.parse(code.get)
      luaObject.foreach(item => println(item.generateCSharpScript()))
    }
  }

}

object LuaParser extends RegexParsers with PackratParsers {
  def stat = assignment
  def laststat = ("return" ~> exp) | "break"
  def assignment = _var ~ ("=" ~> exp) ^^ (result => new LuaAssignmentExpression(result._1.value.toString, result._2))
  def _var = """[a-zA-Z][a-zA-Z0-9]*""".r ^^ (result => new LuaIdentifier(result))
  def exp: LuaParser.Parser[LuaExpression]
    = binopExp | number | string
  def binopExp = literal ~ binop ~ exp ^^ (result => new LuaBinopExpression(result._1._1, result._1._2, result._2))
  def binop = "+" | "-" | "*" | "/" | "^" | "%"

  def literal = string | number | _var
  def number = """[.|0-9]+""".r                 ^^ (result => new LuaNumber(result.toFloat))
  def string = "\"".r ~> (".*(?=\")".r <~ "\"") ^^ (result => new LuaString(result))

  def parse(input:String): List[LuaObject] = parseAll(phrase(rep(stat)), new PackratReader[Char](new CharSequenceReader(input))).get
}


