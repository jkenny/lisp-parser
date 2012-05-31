import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.Checkers

/**
 *
 *
 * @author John Kenny
 * @since 30/05/2012
 */
class TestParser extends FlatSpec with ShouldMatchers with Checkers {

  import Parser._

  "A Lisp parser" should "add correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(+ %s %s)".format(x, y)).toLong == x + y
    }
  }

  it should "subtract correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(- %s %s)".format(x, y)).toLong == x - y
    }
  }

  it should "< correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(< %s %s)".format(x, y)).toBoolean == (x < y)
    }
  }

  it should "> correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(> %s %s)".format(x, y)).toBoolean == (x > y)
    }
  }

  it should "if correctly" in {
    check {
      (x: Long, y: Long) =>
        parse("(if (< %s %s) %s %s)".format(x, y, x, y)).toLong == math.min(x, y)
    }
  }
}
