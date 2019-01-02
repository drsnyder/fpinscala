package fpinscala.parsing

import language.higherKinds
import language.implicitConversions

trait JSON
object JSON {
	case object JNull extends JSON
	case class JNumber(get: Double) extends JSON
	case class JString(get: String) extends JSON
	case class JBool(get: Boolean) extends JSON
	case class JArray(get: IndexedSeq[JSON]) extends JSON
	case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    def tok(s: String) = P.token(P.string(s))

    def lit = tok("true").as(JBool(true))

    root(lit)
  }
}
