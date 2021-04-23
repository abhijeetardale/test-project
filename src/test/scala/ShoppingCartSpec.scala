import org.scalatest.{MustMatchers, WordSpec}

class ShoppingCartSpec extends WordSpec with MustMatchers {

	"Calling checkout" must {

		"return 0 if list is empty" in {

			ShoppingCart.checkout(List.empty) mustBe "£0.0"
		}

		"return £0.6 if list contains an Apple" in {

			ShoppingCart.checkout(List("Apple")) mustBe "£0.6"
		}

		"return £1.2 if list contains 2 Apples" in {

			ShoppingCart.checkout(List("Apple", "Apple")) mustBe "£1.2"
		}

		"return £0.25 if list contains an Orange" in {

			ShoppingCart.checkout(List("Orange")) mustBe "£0.25"
		}

		"return £1.0 if list contains 4 Oranges" in {

			ShoppingCart.checkout(List("Orange", "Orange", "Orange", "Orange")) mustBe "£1.0"
		}

		"return £2.05 for list 0f Apple, Apple, Orange, Apple" in {

			ShoppingCart.checkout(List("Apple", "Apple", "Orange", "Apple")) mustBe "£2.05"
		}

		"return £2.8 for list 0f Apple, Apple, Orange, Orange, Orange, Apple, Apple if no offer" in {

			ShoppingCart.checkout(List("Orange", "Apple", "Apple", "Orange", "Orange", "Orange", "Apple")) mustBe "£2.8"
		}

		"return £2.8 for list 0f Apple, Apple, Orange, Orange, Orange, Apple, Apple if offer applied" in {

			ShoppingCart.checkout(List("Orange", "Apple", "Apple", "Orange", "Orange", "Orange", "Apple"), true) mustBe "£1.95"
		}
	}

	"AppleOffer" must {

		"rerun £1.2 for 2 Apples and no offer" in {
			ShoppingCart.appleOffer(2, false) mustBe 1.2
		}

		"rerun £0.6 for an Apple" in {
			ShoppingCart.appleOffer(1, true) mustBe 0.6
		}

		"rerun £0.6 for 2 Apples" in {
			ShoppingCart.appleOffer(2, true) mustBe 0.6
		}

		"rerun £1.2 for 3 Apples" in {
			ShoppingCart.appleOffer(3, true) mustBe 1.2
		}
	}

	"OrangeOffer" must {

		"rerun £0.75 for three Oranges and no offer" in {
			ShoppingCart.orangeOffer(3, false) mustBe 0.75
		}

		"rerun £0.25 for an Orange" in {
			ShoppingCart.orangeOffer(1, true) mustBe 0.25
		}

		"rerun £0.50 for two Oranges" in {
			ShoppingCart.orangeOffer(2, true) mustBe 0.50
		}

		"rerun £0.50 for three Oranges" in {
			ShoppingCart.orangeOffer(3, true) mustBe 0.50
		}

		"rerun £0.75 for 4 Oranges" in {
			ShoppingCart.orangeOffer(4, true) mustBe 0.75
		}
	}

}
