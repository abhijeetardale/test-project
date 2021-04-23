
case class Loan(lender : String, preapproved: Boolean, APR: Float)

object Solution extends App {

	val loans = List(Loan("B", false, 5.0f), Loan("A", true, 2.0f), Loan("C", true, 3.0f), Loan("D", false, 4.0f))
//	val loans = List(Loan("B", false, 1.0f), Loan("A", true, 1.0f))


	def sortLoans(loans: List[Loan]) : Seq[List[Loan]] = {

		def orderLoans(loans: Seq[Loan], orderedLoans: Seq[List[Loan]], rows: Int): Seq[List[Loan]] = {
			val noOfPre = loans.count(_.preapproved)
			loans match {
				case Nil => Seq.empty[List[Loan]]
				//					case _ if loans.length > rows => loans
				case _ if noOfPre > 1 =>

					val res1 = loans.filter(!_.preapproved)

					loans.filter(_.preapproved).flatMap{ res =>

						orderedLoans ++ Seq(combineResults(res, res1.toList, rows))
					}

				case _ => orderedLoans ++ Seq(loans.sortBy(_.preapproved).reverse.toList)
			}
		}

		def combineResults(head: Loan, remainingPart: List[Loan], rows: Int): List[Loan] ={
			remainingPart match {
				case Nil => List(head)
				case _ if remainingPart.length >= rows => List(head) ++ remainingPart.take(rows-1).sortBy(_.APR)
				case _  => List(head) ++ remainingPart
			}
		}

		orderLoans(loans, Seq(), 2)
	}

	println(sortLoans(loans))
}