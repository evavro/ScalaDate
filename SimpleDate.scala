object Main
{
	object WeekDay extends Enumeration
	{
		type WeekDay = Value
		//val Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday = Value
		val Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday = Value
	}

	class SimpleDate(month: Int, day: Int, year: Int)
	{
		import WeekDay._

		//if(!validDate)
		//	throw new IllegalArgumentException("The provided date is invalid")

		def isLeapYear: Boolean = SimpleDate.isLeapYear(year)
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
		    var newDay = day - 1

		    if(newDay < 1) {
		    	newMonth = if(newMonth - 1 == 0) SimpleDate.NUM_MONTHS else newMonth - 1
		    	newDay = SimpleDate.daysInMonth(newMonth, year)

		    	if(newMonth == SimpleDate.NUM_MONTHS)
		    		newYear -= 1
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
		def daysAgo(n: Int): SimpleDate = {
			var newDate = this

			if(n < 0)
				return daysFromNow(n * -1)

		    for(i <- 0 until n)
		    	newDate = newDate.prevDate

		    newDate
		}

		def daysFromNow(n: Int): SimpleDate = {
			var newDate = this

			if(n < 0)
				return daysAgo(n * -1)

		    for(i <- 0 until n)
		      newDate = newDate.nextDate

		    newDate
		}

		def daysBetween(other: SimpleDate): Int = {
			0
		}

		def dayOfWeek: WeekDay = {
			val zellerMonth = if (month > 2) month else month + 12
		    val yearOfCentury = year % 100
		    val century = year / 100
		    val dayWeek = (day - 1 + ((13 * (zellerMonth + 1)) / 5) + yearOfCentury + (yearOfCentury / 4) + (century / 4) + 5 * century) % 7

		    // (1 = Monday to 7 = Sunday)
		    WeekDay.apply((((dayWeek + 5) % 7) + 1) % 7)
		}

		override def toString = month + "/" + day + "/" + year
		override def hashCode: Int = year * daysInYear + ordinalDate

		def compareTo(that: SimpleDate): Int = this.hashCode.compare(that.hashCode)
	}

	object SimpleDate
	{
		var MIN_YEAR = 1753
		var NUM_MONTHS = 12
		var DAYS_WEEK = 7
		var DAYS_YEAR = 365
		var DAYS_LEAP_YEAR = 366
		var DAYS_IN_MONTH = Array(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
		var DAYS_THUS_FAR = Array(0, 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)

		def isLeapYear(year: Int): Boolean = if(!(year % 100 == 0 && year % 400 != 0)) year % 4 == 0 else false
		def daysInYear(year: Int): Int = if(isLeapYear(year)) DAYS_LEAP_YEAR else DAYS_YEAR
		def daysInMonth(month: Int, year: Int): Int = DAYS_IN_MONTH(month) + (if(isLeapYear(year) && month == 2) 1 else 0)

		def validDate(month: Int, day: Int, year: Int): Boolean = {
			year >= MIN_YEAR &&
		 	(1 to NUM_MONTHS).contains(month) && 
			(1 to daysInYear(year)).contains(day) && 
			(1 to daysInMonth(month, year)).contains(day)
		}
	}

	def main(args: Array[String]) {
		//val date = new SimpleDate(2, 30, 1904) // invalid
		//val date = new SimpleDate(2, 29, 1905) // invalid
		//val date = new SimpleDate(2, 29, 1904)
		//val date = new SimpleDate(12, 31, 1905)
		//val date = new SimpleDate(4, 10, 2012)
		//val date = new SimpleDate(4, 9, 2012) // BORKED
		//var date = new SimpleDate(4, 8, 2012)
		val date = new SimpleDate(4, 16, 2012)

		printf("Testing %s ...\n", date)
		println("Valid? " + date.validDate)
		println("Day of week: " + date.dayOfWeek)
		println("Is leap year? " + date.isLeapYear)
		println("Ordinal date: " + date.ordinalDate)
		println("Days in month: " + date.daysInMonth)
		println("5 days from now: " + date.daysFromNow(5))
		println("5 days ago: " + date.daysAgo(5))
		println("2 years from now: " + date.daysFromNow(365 * 2))
		println("2 years ago: " + date.daysAgo(365 * 2))
	}
}

