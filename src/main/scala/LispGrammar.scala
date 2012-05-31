import util.parsing.combinator.JavaTokenParsers

class Context(parent: Option[Context]) {

  import scala.collection.mutable.Map

  val bindings = Map.empty[String, Binding]

  lazy val child = new Context(Some(this))

  def +(id: String, func: Binding) = {
    bindings += (id -> func)
  }

  def resolve(id: String): Option[Binding] = {
    if (bindings contains id) {
      Some(bindings(id))
    } else {
      parent match {
        case Some(c) => c resolve id
        case None => None
      }
    }
  }
}

object EmptyContext extends Context(None)

trait LispGrammar extends JavaTokenParsers {

  override def ident: Parser[String] = """[a-zA-Z_]+""".r

  def expr: Parser[Expression] = (
    funcDef
      | ifStatement
      | functionCall
      | term
      | ((operator ~! term ~ term) ^^ { case op ~ x ~ y => Operation(op, x, y) })
    )


  def funcDef = {
    (("defun" ~> ident <~ "(") ~! repsep(ident, ",") <~ ")") ~ opt(string) ~ ("(" ~> rep(expr) <~ ")") ^^ {
      case funcName ~ paramNames ~ desc ~ body => FunctionDef(funcName, paramNames, desc, body)
    }
  }

  def string = stringLiteral ^^ (s => s.substring(1, s.length - 1))

  def ifStatement = {
    ("if" ~> term) ~! term ~ term ^^ {
      case condition ~ t ~ f => If(condition, t, f)
    }
  }

  def operator = "+" | "-" | "<" | ">"

  def term: Parser[Expression] = wholeNumber ^^ (Number(_)) | ident ^^ (Identifier(_, List.empty)) | "(" ~> expr <~ ")"

  def functionCall = ident ~ rep(term) ^^ {
    case id ~ params => Identifier(id, params)
  }
}

sealed abstract class Expression {
  def eval(context: Context): Any
}

case class FunctionDef(name: String, paramNames: List[String], desc: Option[String], body: List[Expression]) extends Expression {
  def eval(context: Context): Any = {
    context +(name, Binding(paramNames, body))
    "Defined function '%s'".format(desc)
  }
}

case class Binding(paramNames: List[String], body: List[Expression]) {
  def run(params: List[Any], context: Context) = {
    val localContext = context.child
    paramNames.zip(params).foreach {
      param =>
        localContext +(param._1, Binding(List.empty, List(Number(param._2.toString))))
    }
    body.take(body.size - 1).foreach(_.eval(localContext))
    body.last.eval(localContext)
  }
}

case class If(condition: Expression, ifTrue: Expression, ifFalse: Expression) extends Expression {
  def eval(context: Context) = if (condition.eval(context).asInstanceOf[Boolean]) ifTrue.eval(context) else ifFalse.eval(context)
}

case class Number(value: String) extends Expression {
  def eval(context: Context) = value.toLong
}

case class Identifier(id: String, params: List[Expression]) extends Expression {
  def eval(context: Context) = context.resolve(id) match {
    case Some(binding) => binding.run(params.map(_.eval(context)), context)
    case None => throw new RuntimeException("Unknown identifier [" + id + "]")
  }
}

case class Operation(operator: String, expr1: Expression, expr2: Expression) extends Expression {
  def eval(context: Context) = {
    val val1 = expr1.eval(context).asInstanceOf[Long]
    val val2 = expr2.eval(context).asInstanceOf[Long]
    operator match {
      case "+" => val1 + val2
      case "-" => val1 - val2
      case "<" => val1 < val2
      case ">" => val1 > val2
      case op => throw new ParseException("Invalid operator " + op)
    }
  }
}

class ParseException(msg: String) extends Exception(msg)
