object Main
{
	object WeekDay extends Enumeration
	{
		type WeekDay = Value
		val Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday = Value
	}

	class SimpleDate(month: Int, day: Int, year: Int) // extends Ordered[Int] //extends DateOrd
	{
		import WeekDay._

		def dayOfWeek: WeekDay = {
			val zellerMonth = if (month > 2) month else month + 12
		    val yearOfCentury = year % 100
		    val century = year / 100
		    val dayWeek = (day - 1 + ((13 * (zellerMonth + 1)) / 5) + yearOfCentury + (yearOfCentury / 4) + (century / 4) + 5 * century) % 7

		    WeekDay.apply(((dayWeek + 5) % 7) + 1)
		}

		override def toString = month + "/" + day + "/" + year
		override def hashCode : Int = year * daysInYear + ordinalDate

		def compareTo(that: SimpleDate): Int = this.hashCode.compare(that.hashCode)

		def isLeapYear: Boolean =SimpleDate.isLeapYear(year)
		def validDate: Boolean = SimpleDate.validDate(month, day, year)
		def daysInYear: Int = SimpleDate.daysInYear(year)
		def daysInMonth: Int = SimpleDate.daysInMonth(month, year)

		def ordinalDate: Int = {
		    var daysTotal = 0

		    (0 until month).foreach {
				m => (daysTotal += SimpleDate.daysInMonth(m, year))
			}

			daysTotal + day
		}

		// We could possibly use an anonymous function and a wrapper function for prevDate and nextDate
		def prevDate: SimpleDate = {
		    var newYear = year
		    var newMonth = month
		    var newDay = day

		    if(newDay > SimpleDate.daysInMonth(month, year)) {
		    	newDay = 1
		    	newMonth += 1

		    	if(newMonth > SimpleDate.NUM_MONTHS) {
		    		newMonth = 1
		    		newYear += 1
		    	}
		    }

		    new SimpleDate(newMonth, newDay, newYear)
		}

		def nextDate: SimpleDate = {
		    var newYear = year
		    var newMonth = month
		    var newDay = day + 1

		    if(newDay > SimpleDate.daysInMonth(month, year)) {
		    	newDay = 1
		    	newMonth += 1

		    	if(newMonth > SimpleDate.NUM_MONTHS) {
		    		newMonth = 1
		    		newYear += 1
		    	}
		    }

		    new SimpleDate(newMonth, newDay, newYear)
		}

		// could take advantage of anonymouse functions here..
		def daysAgo(n: Int) {
			var newDate: SimpleDate = this

		    if(n < 0)
		     	return daysFromNow(n * -1)

		    for(i <- 0 to n)
		    	newDate = newDate.prevDate

		    newDate
		}

		def daysFromNow(n: Int) {
			var newDate: SimpleDate = this

			if(n < 0)
		   		return daysAgo(n * -1)

		    for(i <- 0 to n)
		      newDate = newDate.nextDate

		    newDate
		}
	}

	// This essentially acts as the static methods
	object SimpleDate
	{
		var MIN_YEAR = 1753
		var NUM_MONTHS = 12
		var DAYS_WEEK = 7
		var DAYS_YEAR = 365
		var DAYS_LEAP_YEAR = 366
		var DAYS_IN_MONTH = Array(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
		var DAYS_THUS_FAR = Array(0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

		def isLeapYear(year: Int) : Boolean = if(!(year % 100 == 0 && year % 400 != 0)) year % 4 == 0 else false
		def daysInYear(year: Int) : Int = if(isLeapYear(year)) DAYS_LEAP_YEAR else DAYS_YEAR
		def daysInMonth(month: Int, year: Int) : Int = DAYS_IN_MONTH(month) + (if(isLeapYear(year) && month == 2) 1 else 0)

		def validDate(month: Int, day: Int, year: Int) : Boolean = {
			year >= MIN_YEAR &&
		 	(1 to NUM_MONTHS).contains(month) && 
			(1 to daysInYear(year)).contains(day) && 
			(1 to daysInMonth(month, year)).contains(day)
		}
	}

	def main(args: Array[String]) {
		//var date = new SimpleDate(2, 30, 1904) // invalid
		//var date = new SimpleDate(2, 29, 1905) // invalid
		var date = new SimpleDate(2, 29, 1904)
		//var date = new SimpleDate(12, 31, 1905)

		printf("Testing %s ...\n", date)
		println("Valid? " + date.validDate)
		println("Is leap year? " + date.isLeapYear)
		println("Ordinal date: " + date.ordinalDate)
		println("Days in month: " + date.daysInMonth)
	}
}

