
trait Fruit

case object Apple extends Fruit {
	def calculate(numberOfElement: Int, offer: Boolean): Double = {
		if (offer) {
			(numberOfElement / 2 * 0.6) + (numberOfElement % 2 * 0.6)
		} else {
			numberOfElement * 0.6d
		}
	}
}

case object Orange extends Fruit {
	def calculate(numberOfElement: Int, offer: Boolean): Double = {
		if (offer) {
			(numberOfElement / 3 * 0.5) + (numberOfElement % 3 * 0.25)
		} else {
			numberOfElement * 0.25
		}
	}
}


object ShoppingCart {

	implicit class FruitCalculator(total: Double){

	 def apples(list: List[String], applyOffer: Boolean = false): Double = {
		 total + Apple.calculate(list.count(_=="Apple"), applyOffer)
	 }

	 def oranges(list: List[String], applyOffer: Boolean = false): Double = {
		 total + Orange.calculate(list.count(_=="Orange"), applyOffer)
	 }
	}

	def orangeOffer(numberOfElement: Int, offer: Boolean): Double = {
		if(offer) {
			(numberOfElement / 3 * 0.5) + (numberOfElement % 3 * 0.25)
		} else {
			numberOfElement * 0.25
		}
	}

	def appleOffer(numberOfElement: Int, offer: Boolean): Double = {
		if(offer) {
			(numberOfElement / 2 * 0.6) + (numberOfElement % 2 * 0.6)
		} else {
			numberOfElement * 0.6d
		}
	}

	def checkout(list: List[String], applyOffer: Boolean = false): String = {

		val result = if(list.nonEmpty) {

			val x = list.collect{
			case name if name == "Apple" => name
			case name if name == "Apple" => name
			}

				//			Apple.calculate(list.count(_=="Apple"), applyOffer) + Orange.calculate(list.count(_=="Orange"), applyOffer)
			0.0.apples(list,applyOffer).oranges(list, applyOffer)

		} else{
			0
		}
		s"£$result"
	}
}