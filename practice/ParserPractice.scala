
//run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
//run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
//run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

// Parser[Int] that recognizes 0 or more a
//run(patternCounter(zeroOrMore("a")))("aa") == Right(2)
//run(patternCounter("a*"))("abcd") == Right(1)
//run(patternCounter("a*"))("bbbb") == Right(0)
//run(patternCounter("a*"))("") == Right(0)


// Parser[Int] that recognizes 1 or more a
//run(patternCounter(once("a") | zeroOrMore("a")))("aa") == Right(2)
//run(patternCounter("a" | "a*"))("a") == Right(1)
//run(patternCounter("a" | "a*"))("b") == Lefct(ParserError("Expected a | a*"))

// A parser that recognizes zero or more 'a', followed by one or more 'b', and
// which results in the pair of counts of characters seen. 
//run(patternCounter(List(zeroOrMore("a"), once("b"), zeroOrMore("b"))))("b") == Right((0, 1))
//run(patternCounter(List("a*", "b", "b*")))("ab") == Right((1, 1))
//run(patternCounter(List("a*", "b", "b*")))("aabbbb") == Right((2, 4))

// Does a|b mean the same thing asb|a? This is a choice you get to make. What are
// the consequences if the answer is yes? What about if the answer is no?
// Or should be commutative-- a | b == b | a. If not then combinations may not be
// intuitive for our users.

// Does a|(b|c) mean the same thing as (a|b)|c? If yes, is this a primitive law
// for your algebra, or is it implied by something simpler?
// Same as above. It should be implied by the OR operator.

// Does & make sense in a parser? What would that mean? I don't think it makes sense.
// I can't think of reasonable explanation for what that would mean.

// Should we support ^-- starts with?
//run(startsWith("ab"))("abc") == Right(True)
//run(startsWith("abc"))("bc") == Right(False)

// Support for endswith?
//run(endsWith("bc"))("bc") == Right(True)
//run(endsWith("c"))("a") == Right(False)

// import fpinscala.parsing._
// def myParser[P[+_]](P: Parsers[P]) = {
//   import P._
//   char('a').many.slice.map(_.size) ** char('b').many1.slice.map(_.size)
// }
// 
// trait MyParser[P[+_]] extends Parsers[P] {
//   def ab = 
//     char('a').many.slice.map(_.size) **
//     char('b').many1.slice.map(_.size)
// }

// 9.6
// flatMap(regex("\d+.*".r))(n => listOfN(n.toInt, char('a')))
// answer
// for {
//   digit <- "\d+.r"
//   val n <- digit.toInt
//   _ <- listOfN(n, char('a'))
// }

// 9.10
//Given the parser "abra".**(" ".many).**("cadabra"), what sort of error would
//you like to report given the input "abra cAdabra" (note the capital 'A')? Only
//something like Expected 'a'? Or Expected "cadabra"? What if you wanted to
//choose a different error message, like"Magicwordincorrect,tryagain!"?
// ==
// I would like to see something like Expected "cadabra" at position 5 or Expected 'a' at position 6.

//Given a or b, if a fails on the input, do we always want to run b, or are there
//cases where we might not want to? If there are such cases, can you think of
//addi- tional combinators that would allow the programmer to specify when or
//should consider the second parser?
// ==
// If a was some form of parse error that was caused by malformed input (e.g. unaccepted char)
// then we probably don't want to proceed with b since we know it too will fail.

// How do you want to handle reporting the location of errors?
// I think we want to report the position within the string. Ideally we also provide some additional
// context and as much information as we can to help the programmer fix it.

// Given a or b, if a and b both fail on the input, might we want to support
// reporting both errors? And do we always want to report both errors, or do we
// want to give the programmer a way to specify which of the two errors is
// reported?
// Yes, I think we want to support reporting both if we decide to execute both.

import fpinscala.parsing.MyParser._
import fpinscala.parsing.Location
string("foo")(Location(""))

