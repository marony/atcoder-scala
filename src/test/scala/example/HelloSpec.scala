package example

import com.binbo_kodakusan.Main
import org.scalatest._

class HelloSpec extends FlatSpec with Matchers {
  "The Hello object" should "say hello" in {
    Main.greeting shouldEqual "hello"
  }
}
