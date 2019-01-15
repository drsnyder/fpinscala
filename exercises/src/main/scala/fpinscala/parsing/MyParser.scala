package fpinscala
package parsing

import MyParserTypes._
import scala.util.matching.Regex

object MyParserTypes {
  type MyParser[+A] = ParseState => Result[A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean = true) extends Result[Nothing]

  case class ParseState(loc: Location) {
    def advanceBy(numChars: Int): ParseState =
      copy(loc = loc.copy(offset = loc.offset + numChars))
    def input: String = loc.input.substring(loc.offset)
    def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
  }



  sealed trait Result[+A] {
    def extract: Either[ParseError,A] = this match {
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)
    }
     


    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] =
      this match { 
        case Failure(e,true) => Failure(e,false)
        case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] =
      this match {
        case Failure(e,c) => Failure(e, c || isCommitted)
        case _ => this
      }

    def advanceSuccess(n: Int): Result[A] =
      this match {
        case Success(a,m) => Success(a,n+m)
        case _ => this
      }
  }

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    var i = 0
    while (i < s1.length && i < s2.length) {
      if (s1.charAt(i+offset) != s2.charAt(i)) return i
      i += 1
    }
    if (s1.length-offset >= s2.length) -1
    else s1.length-offset
  }


}


object MyParser extends Parsers[MyParser] {
	def run[A](p: MyParser[A])(input: String): Either[ParseError,A] =
    p(ParseState(Location(input, 0))).extract

  implicit def regex(r: Regex): MyParser[String] = {
    val msg = "regex " + r
    state => r.findPrefixOf(state.input) match {
      case None => Failure(state.loc.toError(msg), false)
      case Some(m) => Success(m, m.length)
    }
  }

  implicit def string(w: String): MyParser[String] = {
    val msg = "'" + w + "'"
    state => {
      val i = firstNonmatchingIndex(state.loc.input, w, state.loc.offset)
      if (i == -1) // they matched
        Success(w, w.length)
      else
        Failure(state.loc.advanceBy(i).toError(msg), i != 0)
    }
  }


  def succeed[A](a: A): MyParser[A] =
    state => Success(a, 0)

	def attempt[A](p: MyParser[A]): MyParser[A] =
    state => p(state).uncommit

	def flatMap[A, B](p: MyParser[A])(f: A => MyParser[B]): MyParser[B] =
    state => p(state) match {
      case Success(r, n) => f(r)(state.advanceBy(n))
                                .addCommit(n != 0) // if 1 or more is consumed
                                .advanceSuccess(n)
      case e@Failure(_,_) => e
    }

  def label[A](msg: String)(p: MyParser[A]): MyParser[A] =
    state => p(state).mapError(_.label(msg))

  def or[A](s1: MyParser[A],s2: => MyParser[A]): MyParser[A] =
    state => s1(state) match {
      case Failure(r,false) => s2(state)
      case r => r
    }

  def scope[A](msg: String)(p: MyParser[A]): MyParser[A] =
    state => p(state).mapError(_.push(state.loc, msg))

  def slice[A](p: MyParser[A]): MyParser[String] =
    state => p(state) match {
      case Success(_, n) => Success(state.slice(n), n)
      case f@Failure(_,_) => f
    }
}
