package fpinscala.parsing

import scala.util.matching.Regex
import fpinscala.testing._
import fpinscala.testing.Prop._
import language.higherKinds
import language.implicitConversions

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))
  implicit def regex(r: Regex): Parser[String]


  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]]

  // define in terms of or, map2, and succeed
  def many_v2[A](p: Parser[A]): Parser[List[A]] =
    succeed(List[A]()).map(s => s) | map2(p, p.map(pp => List(pp)))(_ :: _)
  def many_v3[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  // Using map2 and succeed, implement the listOfN combinator from earlier.
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)


  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)


  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]

  // 9.7
  def product2[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    flatMap(p)(a => map(p2)(b => (a, b)))
  def map22[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    flatMap(p)(a => flatMap(p2)(b => succeed(f(a, b))))
  def map22a[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield f(a,b)

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    flatMap(p)(a => succeed(f(a)))

  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def skipL[B](p: Parser[Any], p2: => Parser[B]): Parser[B] =
    map2(slice(p), p2)((_,b) => b)

  def skipR[A](p: Parser[A], p2: => Parser[Any]): Parser[A] =
    map2(p, slice(p2))((a,b) => a)

  def attempt[A](p: Parser[A]): Parser[A]

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def token[A](p: Parser[A]): Parser[A] =
    attempt(p) <* whitespace

  def whitespace: Parser[String] = "\\s*".r
  def number: Parser[String] = "\\d+".r

  def surround[A](start: Parser[Any], stop: Parser[Any])(p: => Parser[A]) =
    start *> p <* stop

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
    sep1(p,p2) or succeed(List())

  /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
  def sep1[A](p: Parser[A], p2: Parser[Any]): Parser[List[A]] =
    map2(p, many(p2 *> p))(_ :: _)

  def root[A](p: Parser[A]): Parser[A]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many = self.many(p)


    def slice: Parser[String] = self.slice(p)
    def many1: Parser[List[A]] = self.many1(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def product[B](p2: => Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def as[B](b: B): Parser[B] = self.map(self.slice(p))(_ => b)
    def *>[B](p2: => Parser[B]) = self.skipL(p, p2)
    def <*(p2: => Parser[Any]) = self.skipR(p, p2)
    def label(msg: String): Parser[A] = self.label(msg)(p)
    def token = self.token(p)
  }

  object Laws {
		def unbiasL[A,B,C](p: ((A,B), C)): (A,B,C) = (p._1._1, p._1._2, p._2)
		def unbiasR[A,B,C](p: (A, (B,C))): (A,B,C) = (p._1, p._2._1, p._2._2)

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def notEqual[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) != run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

		def succeedLaw[A](in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(s))(s) == Right(s))

    // How do we test associativity
    def productLaw[A,B](a: Parser[A], b: Parser[B])(in: Gen[String]): Prop =
      equal(a.map(aa => b.map(bb => (aa, bb))), product(a, b))(in)

    // Not commutative
    def productLaw1[A,B](a: Parser[A], b: Parser[B])(in: Gen[String]): Prop =
      notEqual(a.map(aa => b.map(bb => (aa, bb))), product(b, a))(in)

    def productLaw2[A,B,C](a: Parser[A], b: Parser[B], c: Parser[C])(in: Gen[String]): Prop =
      equal((a ** b) ** c map (unbiasL _), a ** (b ** c) map (unbiasR _))(in)
  }
}

case class Location(input: String, offset: Int = 0) {

  lazy val line = input.slice(0,offset+1).count(_ == '\n') + 1
  lazy val col = input.slice(0,offset+1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset+n)

  /* Returns the line corresponding to this location */
  def currentLine: String = 
    if (input.length > 1) input.lines.drop(line-1).next
    else ""
}

case class ParseError(stack: List[(Location,String)] = List(),
                      otherFailures: List[ParseError] = List()) {
}

object Parsers {

}
