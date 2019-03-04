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

}