

sealed trait STOP {
  def name: String

  def zone: Seq[Int]
}

sealed trait TMODE

case object TUBE extends TMODE

case object BUS extends TMODE

case class HOLBORN() extends STOP {
  val name = "Holborn"
  val zone: Seq[Int] = Seq(1)
}

case class EARLSCOURT() extends STOP {
  val name = "Earl's Court"
  val zone: Seq[Int] = Seq(1, 2)
}

case class WIMBLEDON() extends STOP {
  val name = "Wimbledon"
  val zone: Seq[Int] = Seq(3)
}

case class HAMMERSMITH() extends STOP {
  val name = "Hammersmith"
  val zone: Seq[Int] = Seq(2)
}

case class CHELSEA() extends STOP {
  val name = "Chelsea"
  val zone: Seq[Int] = Seq(3)
}

case class BUS() extends STOP {
  val name = "bus"
  val zone: Seq[Int] = Seq(1, 2, 3)
}

case class Trips(startPoint: STOP, endPoint: STOP)

case class Passenger(totalAmount: Double,
                     dayAmount: Double = 0,
                     startPoint: Option[STOP] = None,
                     endPoint: Option[STOP] = None,
                     transportType: TMODE = TUBE,
                     trips: Seq[Trips] = Seq.empty
                    )

class LondonOyster() {

  def loadAmount(p: Double): Passenger = {
    Passenger(totalAmount = p)
  }

  def startFromA(p: Passenger, stop: STOP, transportType: TMODE = TUBE): Passenger = {

    if (p.trips.isEmpty) {
      Passenger(p.totalAmount - 3.2, 3.2, Some(stop), None, transportType, p.trips)
    } else {
      Passenger(p.totalAmount-(3.2-p.dayAmount), 3.2, Some(stop), None, transportType, p.trips)
    }

  }

  def endOnA(p: Passenger, stop: STOP): Passenger = {

    val fare = if (p.trips.isEmpty) {
      p.transportType match {
        case BUS => 1.8
        case TUBE => priceCalculatuion(p.startPoint.getOrElse(stop), stop)
      }
    } else {
      3.2
    }

    val diff = if (p.trips.isEmpty) { 3.2 - fare } else { 0 }

    val trip = Trips(p.startPoint.getOrElse(stop), stop)

    Passenger(p.totalAmount + diff, fare, None, None, TUBE, p.trips ++ Seq(trip))
  }


  private def priceCalculatuion(startPoint: STOP, endPoint: STOP): Double = {

    val startZone1 = startPoint.zone.contains(1)
    val endZone1 = endPoint.zone.contains(1)
    val zoneUnion = startPoint.zone.union(endPoint.zone)

    if (startZone1 && endZone1) return 2.5
    if (!startZone1 && !endZone1 && zoneUnion == 1) 2.0
    if (zoneUnion.length == 2 && zoneUnion.contains(1)) 3.0
    if (zoneUnion.length == 2 && !zoneUnion.contains(1)) 2.25
    if (zoneUnion.length == 3) 2.25 else 3.20

  }

  def startJourney(): Unit = {
    val p = loadAmount(30)
    println(s"Initial total amount : ${p.totalAmount}   fare : ${p.dayAmount}")

    val boarding1 = startFromA(p, HOLBORN())
    println(s"before 1st trip total amount : ${boarding1.totalAmount}   fare : ${boarding1.dayAmount}")

    val exit1 = endOnA(boarding1, EARLSCOURT())
    println(s"after 1st trip total amount : ${exit1.totalAmount}   fare : ${exit1.dayAmount}")

    val boarding2 = startFromA(exit1, EARLSCOURT(), BUS)
    println(s"before 2nd trip total amount : ${boarding2.totalAmount}   fare : ${boarding2.dayAmount}")

    val exit2 = endOnA(boarding2, CHELSEA())
    println(s"after 2nd trip total amount : ${exit2.totalAmount}   fare : ${exit2.dayAmount}")

    val boarding3 = endOnA(exit2, EARLSCOURT())
    println(s"before 3rd trip total amount : ${boarding3.totalAmount}   fare : ${boarding3.dayAmount}")

    val exit3 = endOnA(boarding3, HAMMERSMITH())
    println(s"after 3rd trip total amount : ${exit3.totalAmount}   fare : ${exit3.dayAmount}")
  }

}


object Runner {
  def main(args: Array[String]): Unit = {
    val oyester = new LondonOyster
    oyester.startJourney()
  }
}