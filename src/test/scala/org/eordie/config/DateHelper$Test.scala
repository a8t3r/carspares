package org.eordie.config

import org.eordie.BaseSpec
import java.util.Date
import org.joda.time.DateTime
import org.eordie.lib.DateHelper
import org.scalatest.Ignore

/**
 *
 * @author Alexandr Kolosov
 * @since 10/20/12
 */
@Ignore
class DateHelper$Test extends BaseSpec {

  "DateHelper" should {
    "correctly determine relative day" in {
      val today: Date = new Date()
      val yesterday: Date = new DateTime(today).minusDays(1).toDate
      val someDate = new Date(0)
      DateHelper.yesterdayOrToday(today) should equal("Today")
      DateHelper.yesterdayOrToday(yesterday) should equal("Yesterday")
      DateHelper.yesterdayOrToday(someDate) should equal("01.01.1970")
    }
  }
}
