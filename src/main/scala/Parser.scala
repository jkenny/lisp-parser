import java.io.FileReader

object Parser extends LispGrammar {
  private val environment: Context = EmptyContext

  def main(args: Array[String]) {
    for (input <- io.Source.stdin.getLines()) {
      parse(input)
    }
  }

  def parse(input: String): String = {
    val start = System.nanoTime()
    val parseResult = if (input.startsWith("file:")) {
      val reader = new FileReader(input.substring(input.indexOf("file:") + 5).trim)
      parseAll(expr, reader)
    } else {
      parseAll(expr, input.trim)
    }

    val result = parseResult match {
      case Success(parsedExpression, _) => println(parsedExpression); parsedExpression.eval(environment).toString
      case NoSuccess(error, _) => sys.error(parseResult.toString); error
    }

    println("Time taken: " + ((System.nanoTime() - start) / 1000000) + "ms")

    result
  }
}
