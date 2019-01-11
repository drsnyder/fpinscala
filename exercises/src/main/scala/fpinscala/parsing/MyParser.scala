package fpinscala
package parsing

import MyParserTypes._
import scala.util.matching.Regex

object MyParserTypes {
  type MyParser[+A] = Location => Result[A]
  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]
}


object MyParser extends Parsers[MyParser] {
	def run[A](p: MyParser[A])(input: String): Either[ParseError,A] = ???
  implicit def regex(r: Regex): MyParser[String] = ???

	implicit def string(s: String): MyParser[String] =
    (loc: Location) =>
      if (loc.input.startsWith(s)) Success(s, loc.input.length)
      else Failure(Location(loc.input).toError("Expected: " + s))

	def attempt[A](p: MyParser[A]): MyParser[A] = ???
	def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] = ???
	def label[A](msg: String)(p: MyParser[A]): MyParser[A] = ???
	def many[A](p: MyParser[A]): MyParser[List[A]] = ???
	def or[A](s1: MyParser[A],s2: => MyParser[A]): MyParser[A] = ???
	def product[A, B](p: MyParser[A],p2: => MyParser[B]): MyParser[(A, B)] = ???
	def root[A](p: MyParser[A]): MyParser[A] = ???
	def scope[A](msg: String)(p: MyParser[A]): MyParser[A] = ???
	def slice[A](p: MyParser[A]): MyParser[String] = ???
}
