package fpinscala.parsing

import fpinscala.testing._
import fpinscala.testing.Prop._
import language.higherKinds

trait Parsers[Parser[+_]] { self => // so inner classes may call methods of trait
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]

  implicit def string(s: String): Parser[String]
  implicit def operators[A](p: Parser[A]) = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
    ParserOps[String] = ParserOps(f(a))

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  def many[A](p: Parser[A]): Parser[List[A]]
  def slice[A](p: Parser[A]): Parser[String]

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)


  def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)]

  def map[A,B](a: Parser[A])(f: A => B): Parser[B]

  def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] =
    map(product(p, p2))(f.tupled)


  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
    def many = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice[A](p: Parser[A]): Parser[String] = self.slice(p)
    def many1[A](p: Parser[A]): Parser[List[A]] = self.many1(p)

    def **[B](p2: => Parser[B]): Parser[(A,B)] =
      self.product(p,p2)

    def product[A,B](p: Parser[A], p2: Parser[B]): Parser[(A,B)] = self.product(p, p2)
    def map2[A,B,C](p: Parser[A], p2: Parser[B])(f: (A,B) => C): Parser[C] = self.map2(p, p2)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)
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
