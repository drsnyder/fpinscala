import fpinscala.applicative._
import java.text._
import java.util.Date

object ApplicativePractice {
    import Applicative.streamApplicative._

    def run() {
        // sequence creates an infinite table with the
        // row of streams provided
        println(
            sequence(List(unit(1), unit(2)))
        )
    }
}

case class WebForm(name: String, birthdate: Date, phoneNumber: String)
object Validation {
    import Applicative._
    def validName(name: String): Validation[String, String] =
        if (name != "") Success(name)
        else Failure("Name cannot be empty")

    def validBirthdate(birthdate: String): Validation[String, Date] =
        try {
            Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
        } catch {
            case e => Failure("Birthdate must be in the form yyyy-MM-dd")
        }

    def validPhone(phoneNumber: String): Validation[String, String] =
        if (phoneNumber.matches("[0-9]{10}"))
            Success(phoneNumber)
        else Failure("Phone number must be 10 digits")

    def validWebForm(name: String, birthdate: String,
                 phone: String): Validation[String, WebForm] =
        validationApplicative[String].map3(
            validName(name),
            validBirthdate(birthdate),
            validPhone(phone))(
            WebForm(_,_,_))

    def run() {
        println(
            Validation.validWebForm("bill", "2010-11-01", "1234567890")
        )
    }
}

object Laws {
  import Applicative.optionApplicative._
  val v = Some(1)
  val fa = Some('a')
  def f(x: Int): Int = x + 1
  def g(x: Int): Int = x + 2

  def run() {
    println(
      map(v)(x => x) == v
    )
    println(
      map(map(v)(g))(f) == map(v)(f _ compose g _)
    )
    println(
        map2(unit(()), fa)((_,a) => a) == fa
    )
    println(
        map2(fa, unit(()))((a,_) => a) == fa
    )
  }
}

object Traverse {
    import Traverse._
    import Applicative._

    def run() {
        println(
            listTraverse.sequence(List(Option(1), Option(2)))(optionApplicative)
        )
        println(
            listTraverse.sequence(List(Option(1), Option(2), None))(optionApplicative)
        )
        println(
            treeTraverse.sequence(
                Tree(Option(1),
                List(Tree(Option(2), List()), Tree(Option(3), List())))
            )(optionApplicative)
        )
        println(
            treeTraverse.sequence(
                Tree(Option(1),
                List(Tree(Option(2), List()), Tree(None, List())))
            )(optionApplicative)
        )

        println(
            treeTraverse.reverse(Tree(1, List(Tree(2, List()))))
        )

        val x = List(1, 2, 3)
        val y = List(4, 5, 6)
        import listTraverse._
        println(
            toList(reverse(x)) ++ toList(reverse(y)) == reverse(toList(y) ++ toList(x))
        )

    }
}

